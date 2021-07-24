;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; based on clojure.tools.reader
(ns ^{:doc "A clojure reader in clojure"
      :author "Bronsa"}
  typed.clj.reader
  (:refer-clojure :exclude [read read-line read-string char read+string
                            default-data-readers *default-data-reader-fn*
                            *read-eval* *data-readers* *suppress-read*])
  (:require [typed.clj.reader.reader-types :refer
             [read-char unread peek-char indexing-reader? source-logging-push-back-reader source-logging-reader?
              get-line-number get-column-number get-file-name string-push-back-reader log-source]]
            [typed.clj.reader.impl.utils :refer :all] ;; [char ex-info? whitespace? numeric? desugar-meta]
            [typed.clj.reader.impl.errors :as err]
            [typed.clj.reader.impl.commons :refer :all]
            [typed.clj.reader.default-data-readers :as data-readers])
  (:import (clojure.lang PersistentHashSet IMeta
                         RT Symbol Reflector Var IObj
                         PersistentVector IRecord Namespace)
           typed.clj.reader.reader_types.SourceLoggingPushbackReader
           java.lang.reflect.Constructor
           java.util.regex.Pattern))

(set! *warn-on-reflection* true)


;; TODO
;; - avoid (contains? m :val) for distinguishing values from whitespace
;;   - it narrows potential optimisations
;;   - eg., if :op ::forms is a defrecord, it sometimes has a val,
;;     sometimes not. but if you dissoc :val from a record, it loses
;;     its type.
;;   - perhaps :type #{:ws :val}?
;;     - Q: is ::discard whitespace?
;;     - Q: what about ::cond-splicing, we can't always be sure
;;          how many values it has with *suppress-read*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare ^:private read*
         macros dispatch-macros
         ^:dynamic *read-eval*
         ^:dynamic *data-readers*
         ^:dynamic *default-data-reader-fn*
         ^:dynamic *suppress-read*
         default-data-readers)

(defn- top-level-forms-ast [forms]
  {:pre [(vector? forms)
         (not= 0 (count forms))]
   :post [(or (and (map? %)
                   #_ ;; might be a comment
                   (or (contains? % :val)
                       (:eof %)))
              (assert nil (pr-str %)))]}
  (-> (if (= 1 (count forms))
        (nth forms 0)
        (into {:op ::forms
               :forms (into [] (map (fn [m]
                                      ;(assert (not (#{::forms} (:op m))) m)
                                      (dissoc m :read-finished)))
                            forms)}
              (select-keys (peek forms) [:val :eof :read-finished])))
      (assoc :top-level true)))

(defn- ^PersistentVector forms->vals [forms]
  {:pre [(vector? forms)]
   :post [(vector? %)]}
  (into []
        (mapcat (fn [m]
                  {:pre [(map? m)]}
                  (when-some [[_ v] (find m :val)]
                    (if (and (#{::cond-splicing} (:op m))
                             (not (:suppress-read m)))
                      v
                      [v]))))
        forms))

(defn ^:private ns-name* [x]
  (if (instance? Namespace x)
    (name (ns-name x))
    (name x)))

(defn- macro-terminating? [ch]
  (case ch
    (\" \; \@ \^ \` \~ \( \) \[ \] \{ \} \\) true
    false))

(defn- ^String read-token
  "Read in a single logical token from the reader"
  [rdr kind initch]
  (if-not initch
    (err/throw-eof-at-start rdr kind)
    (loop [sb (StringBuilder.) ch initch]
      (if (or (whitespace? ch)
              (macro-terminating? ch)
              (nil? ch))
        (do (when ch
              (unread rdr ch))
            (str sb))
        (recur (.append sb ch) (read-char rdr))))))

(declare read-tagged)

(defn- read-dispatch
  [rdr _ opts]
  (if-let [ch (read-char rdr)]
    (if-let [dm (dispatch-macros ch)]
      (dm rdr ch opts)
      (read-tagged (doto rdr (unread ch)) ch opts)) ;; ctor reader is implemented as a tagged literal
    (err/throw-eof-at-dispatch rdr)))

(defn- read-unmatched-delimiter
  [rdr ch opts]
  (err/throw-unmatch-delimiter rdr ch))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn read-regex
  [rdr ch opts]
  (let [sb (StringBuilder.)]
    (loop [ch (read-char rdr)]
      (if (identical? \" ch)
        {:op ::regex
         :string (str sb)
         :val (Pattern/compile (str sb))}
        (if (nil? ch)
          (err/throw-eof-reading rdr :regex sb)
          (do
            (.append sb ch )
            (when (identical? \\ ch)
              (let [ch (read-char rdr)]
                (if (nil? ch)
                  (err/throw-eof-reading rdr :regex sb))
                (.append sb ch)))
            (recur (read-char rdr))))))))

(defn- read-unicode-char
  ;; returns char
  ([^String token ^long offset ^long length ^long base]
   (let [l (+ offset length)]
     (when-not (== (count token) l)
       (err/throw-invalid-unicode-literal nil token))
     (loop [i offset uc 0]
       (if (== i l)
         (char uc)
         (let [d (Character/digit (int (nth token i)) (int base))]
           (if (== d -1)
             (err/throw-invalid-unicode-digit-in-token nil (nth token i) token)
             (recur (inc i) (long (+ d (* uc base))))))))))

  ;; returns [char original-string]
  ([rdr prefix-no-base initch base length exact?]
   {:pre [(string? prefix-no-base)
          (char? initch)]
    :post [(and (vector? %)
                (char? (nth % 0))
                (string? (nth % 1)))]}
   (let [base (long base)
         length (long length)]
     (loop [i 1
            uc (long (Character/digit (int initch) (int base)))
            all (doto (StringBuilder.)
                  (.append \\)
                  (.append prefix-no-base)
                  (.append initch))]
       (if (== uc -1)
         (err/throw-invalid-unicode-digit rdr initch)
         (if-not (== i length)
           (let [ch (peek-char rdr)]
             (if (or (whitespace? ch)
                     (macros ch)
                     (nil? ch))
               (if exact?
                 (err/throw-invalid-unicode-len rdr i length)
                 [(char uc) (str all)])
               (let [d (Character/digit (int ch) (int base))]
                 (read-char rdr)
                 (if (== d -1)
                   (err/throw-invalid-unicode-digit rdr ch)
                   (recur (inc i)
                          (long (+ d (* uc base)))
                          (doto (.append all ch)))))))
           [(char uc) (str all)]))))))

(def ^:private ^:const upper-limit (int \uD7ff))
(def ^:private ^:const lower-limit (int \uE000))

(defn- read-char*
  "Read in a character literal"
  [rdr backslash opts]
  (let [ch (read-char rdr)]
    (if-not (nil? ch)
      (let [token (if (or (macro-terminating? ch)
                          (whitespace? ch))
                    (str ch)
                    (read-token rdr :character ch))
            token-len (count token)
            v (cond

                (== 1 token-len)  (Character/valueOf (nth token 0))

                (= token "newline") \newline
                (= token "space") \space
                (= token "tab") \tab
                (= token "backspace") \backspace
                (= token "formfeed") \formfeed
                (= token "return") \return

                (.startsWith token "u")
                (let [c (read-unicode-char token 1 4 16)
                      ic (int c)]
                  (if (and (> ic upper-limit)
                           (< ic lower-limit))
                    (err/throw-invalid-character-literal rdr (Integer/toString ic 16))
                    c))

                (.startsWith token "o")
                (let [len (dec token-len)]
                  (if (> len 3)
                    (err/throw-invalid-octal-len rdr token)
                    (let [uc (read-unicode-char token 1 len 8)]
                      (if (> (int uc) 0377)
                        (err/throw-bad-octal-number rdr)
                        uc))))

                :else (err/throw-unsupported-character rdr token))]
        {:op ::character
         :string token
         :val v})
      (err/throw-eof-in-character rdr))))

(defn ^:private starting-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (int (dec (int (get-column-number rdr))))]))

(defn ^:private ending-line-col-info [rdr]
  (when (indexing-reader? rdr)
    [(get-line-number rdr) (get-column-number rdr)]))

(defonce ^:private READ_EOF (Object.))

(defn- flatten-forms [m]
  (if (#{::forms} (:op m))
    (:forms m)
    [m]))

(def ^:dynamic *read-delim* false)
(defn- ^PersistentVector read-delimited
  "Reads and returns a collection of forms (including whitespace) ended with delim"
  [kind delim rdr opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        delim (char delim)]
    (binding [*read-delim* true]
      (loop [a []]
        (let [forms (read* rdr false delim opts)
              pf (peek forms)]
          (if (:read-finished pf)
            (into a (mapcat flatten-forms) (update forms (dec (count forms)) dissoc :read-finished))
            (if (:eof pf)
              (err/throw-eof-delimited rdr kind start-line start-column (count a))
              (recur (into a (mapcat flatten-forms) forms)))))))))

(defn- forms->list [forms]
  {:pre [(vector? forms)]}
  (let [the-list (forms->vals forms)]
    (if (empty? the-list)
      '()
      (clojure.lang.PersistentList/create the-list))))

(defn- read-list
  "Read in a list, including its location if the reader is an indexing reader"
  [rdr _ opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        forms (read-delimited :list \) rdr opts)
        [end-line end-column] (ending-line-col-info rdr)
        meta-info (when start-line
                    (into
                      {:line start-line
                       :column start-column
                       :end-line end-line
                       :end-column end-column}
                      (when-let [file (get-file-name rdr)]
                        {:file file})))]
    (cond-> {:op ::list
             :val (with-meta (forms->list forms)
                             meta-info)
             :forms forms}
      meta-info (assoc :pos meta-info))))

(defn- read-vector
  "Read in a vector, including its location if the reader is an indexing reader"
  [rdr _ opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        forms (read-delimited :vector \] rdr opts)
        the-vector (forms->vals forms)
        [end-line end-column] (ending-line-col-info rdr)]
    {:op ::vector
     :val (with-meta
            the-vector
            (when start-line
              (merge
                (when-let [file (get-file-name rdr)]
                  {:file file})
                {:line start-line
                 :column start-column
                 :end-line end-line
                 :end-column end-column})))
     :forms forms}))

(defn- read-map
  "Read in a map, including its location if the reader is an indexing reader"
  [rdr _ opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        forms (read-delimited :map \} rdr opts)
        the-map (forms->vals forms)
        map-count (count the-map)
        [end-line end-column] (ending-line-col-info rdr)]
    (when (odd? map-count)
      (err/throw-odd-map rdr start-line start-column the-map))
    {:op ::map
     :val (with-meta
            (if (zero? map-count)
              {}
              (RT/map (to-array the-map)))
            (when start-line
              (merge
                (when-let [file (get-file-name rdr)]
                  {:file file})
                {:line start-line
                 :column start-column
                 :end-line end-line
                 :end-column end-column})))
     :forms forms}))

(defn- read-number
  [rdr initch]
  (loop [sb (doto (StringBuilder.) (.append initch))
         ch (read-char rdr)]
    (if (or (whitespace? ch) (macros ch) (nil? ch))
      (let [s (str sb)
            _ (unread rdr ch)
            v (or (match-number s)
                  (err/throw-invalid-number rdr s))]
        {:op ::number
         :string s
         :val v})
      (recur (doto sb (.append ch)) (read-char rdr)))))

(defn- escape-char [sb rdr]
  (let [ch (read-char rdr)]
    (case ch
      \t ["\t" "\\t"]
      \r ["\r" "\\r"]
      \n ["\n" "\\n"]
      \\ ["\\" "\\\\"]
      \" ["\"" "\\\""]
      \b ["\b" "\\b"]
      \f ["\f" "\\f"]
      \u (let [ch (read-char rdr)]
           (if (== -1 (Character/digit (int ch) 16))
             (err/throw-invalid-unicode-escape rdr ch)
             (read-unicode-char rdr "u" ch 16 4 true)))
      (if (numeric? ch)
        (let [[ch original] (read-unicode-char rdr "" ch 8 3 false)]
          (if (> (int ch) 0377)
            (err/throw-bad-octal-number rdr)
            [ch original]))
        (err/throw-bad-escape-char rdr ch)))))

(defn- read-string*
  [reader _ opts]
  (loop [sb (StringBuilder.)
         all (StringBuilder.)
         ch (read-char reader)]
    (case ch
      nil (err/throw-eof-reading reader :string sb)
      \\ (let [[escape-sequence original] (escape-char sb reader)]
           (recur (doto sb (.append escape-sequence))
                  (doto all (.append original))
                  (read-char reader)))
      \" {:op ::string
          :string (str all)
          :val (str sb)}
      (recur (doto sb (.append ch))
             (doto all (.append ch))
             (read-char reader)))))

(defn- read-symbol
  [rdr initch]
  (let [[line column] (starting-line-col-info rdr)]
    (when-let [token (read-token rdr :symbol initch)]
      (case token

        ;; special symbols
        "nil" {:op ::nil
               :string "nil"
               :val nil}
        "true" {:op ::boolean
                :string token
                :val true}
        "false" {:op ::boolean
                 :string token
                 :val false}
        "/" {:op ::symbol
             :string token
             :val '/}

        (or (when-let [p (parse-symbol token)]
              {:op ::symbol
               :string token
               :val 
                (with-meta (symbol (p 0) (p 1))
                  (when line
                    (merge
                     (when-let [file (get-file-name rdr)]
                       {:file file})
                     (let [[end-line end-column] (ending-line-col-info rdr)]
                       {:line line
                        :column column
                        :end-line end-line
                        :end-column end-column}))))})
            (err/throw-invalid rdr :symbol token))))))

(def ^:dynamic *alias-map*
  "Map from ns alias to ns, if non-nil, it will be used to resolve read-time
   ns aliases instead of (ns-aliases *ns*).

   Defaults to nil"
  nil)

(defn- resolve-alias [sym]
  ((or *alias-map*
       (ns-aliases *ns*)) sym))

(defn- resolve-ns [sym]
  (or (resolve-alias sym)
      (find-ns sym)))

(defn- read-keyword
  [reader initch opts]
  (let [ch (read-char reader)]
    (if-not (whitespace? ch)
      (let [token (read-token reader :keyword ch)
            s (parse-symbol token)]
        (if s
          (let [^String ns (s 0)
                ^String name (s 1)
                auto? (identical? \: (nth token 0))
                k (if auto?
                    (if ns
                      (let [ns (resolve-alias (symbol (subs ns 1)))]
                        (if ns
                          (keyword (str ns) name)
                          (err/throw-invalid reader :keyword (str \: token))))
                      (keyword (str *ns*) (subs name 1)))
                    (keyword ns name))]
            {:op (if auto? ::auto-keyword ::keyword)
             :string (cond-> token
                       auto? (subs 1))
             :val k})
          (err/throw-invalid reader :keyword (str \: token))))
      (err/throw-single-colon reader))))

(defn- wrapping-reader
  "Returns a function which wraps a reader in a call to sym"
  [op sym]
  (fn [rdr _ opts]
    (let [m (read* rdr true opts)]
      (assert (vector? m))
      {:op op
       :val (list* sym (forms->vals m))
       :forms m})))

(defn- read-meta
  "Read metadata and return the following object with the metadata applied"
  [rdr _ opts]
  (let [[line column] (starting-line-col-info rdr)
        mforms (into []
                     (mapcat flatten-forms)
                     (read* rdr true opts))
        lm (peek mforms)
        _ (assert (contains? lm :val) (pr-str lm))
        m (desugar-meta (:val lm))]
    (when-not (map? m)
      (err/throw-bad-metadata rdr m))
    (let [vforms (into []
                       (mapcat flatten-forms)
                       (read* rdr true opts))
          lf (peek vforms)
          _ (assert (contains? lf :val) (pr-str lf))
          o (:val lf)]
      (if (instance? IMeta o)
        {:op ::meta
         :forms (into mforms vforms)
         :val (let [m (if (and line (seq? o))
                        (assoc m :line line :column column)
                        m)]
                (if (instance? IObj o)
                  (vary-meta o merge m)
                  (reset-meta! o m)))}
        (err/throw-bad-metadata-target rdr o)))))

(defn- read-set
  [rdr _ opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        ;; subtract 1 from start-column so it includes the # in the leading #{
        start-column (if start-column (int (dec (int start-column))))
        forms (read-delimited :set \} rdr opts)
        the-set (PersistentHashSet/createWithCheck (forms->vals forms))
        [end-line end-column] (ending-line-col-info rdr)]
    {:op ::set
     :val (with-meta
            the-set
            (when start-line
              (merge
                (when-let [file (get-file-name rdr)]
                  {:file file})
                {:line start-line
                 :column start-column
                 :end-line end-line
                 :end-column end-column})))
     :forms forms}))

(defn- read-discard
  "Read and discard the first object from rdr"
  [rdr _ opts]
  {:op ::discard
   :forms (read* rdr true opts)})

(defn- read-symbolic-value
  [rdr _ opts]
  (let [[sym :as all] (read* rdr true opts)
        _ (when-not (and (= 1 (count all))
                         (#{::symbol} (:op sym)))
            ;;FIXME print actual string
            (err/reader-error rdr (str "Invalid token (printed as ASTs): " all)))
        v (case (:val sym)
            Inf Double/POSITIVE_INFINITY
            -Inf Double/NEGATIVE_INFINITY
            NaN Double/NaN
            (err/reader-error rdr (str "Invalid token: ##" sym)))]
    {:op ::symbolic-value
     :string (:string sym)
     :val v}))

(def ^:private RESERVED_FEATURES #{:else :none})

(defn- has-feature?
  [rdr feature opts]
  {:pre [(vector? feature)]
   :post [(boolean? %)]}
  (let [lf (peek feature)]
    (if (#{::keyword} (:op lf))
      (or (= :default (:val lf)) (contains? (get opts :features) (:val lf)))
      (err/throw-feature-not-keyword rdr feature))))

;; WIP, move to errors in the future
(defn- check-eof-error
  [forms rdr ^long first-line]
  {:pre [(vector? forms)
         (seq forms)]}
  (when (:eof (peek forms))
    (err/throw-eof-error rdr (and (< first-line 0) first-line))))

(defn- check-reserved-features
  [rdr forms]
  {:pre [(vector? forms)]}
  (let [lf (peek forms)]
    (when (and (#{::keyword} (:op lf))
               (get RESERVED_FEATURES (:val lf)))
      (err/reader-error rdr "Feature name " (:val lf) " is reserved"))))

(defn- check-invalid-read-cond
  [forms rdr ^long first-line]
  {:pre [(vector? forms)
         (seq forms)]}
  (when (:read-finished (peek forms))
    (if (< first-line 0)
      (err/reader-error rdr "read-cond requires an even number of forms")
      (err/reader-error rdr "read-cond starting on line " first-line " requires an even number of forms"))))

(defn- read-suppress
  "Read next form and suppress. Return forms."
  [first-line rdr opts]
  (binding [*suppress-read* true]
    (let [forms (read* rdr false \) opts)]
      (check-eof-error forms rdr first-line)
      forms)))

(defn- match-feature
  "Read next feature. If matched, read next form and return.
   Otherwise, read and skip next form, returning a tuple of:
   - boolean which is true iff a match was found
   - vector of feature forms
   - vector of rhs forms (or nil)"
  [first-line rdr opts]
  (let [feature (read* rdr false \) opts)]
    (check-eof-error feature rdr first-line)
    (if (:read-finished (peek feature))
      [false feature nil]
      (let [_ (check-reserved-features rdr feature)
            matched (has-feature? rdr feature opts)
            rhs (if matched
                  ;; feature matched, read selected form
                  (doto (read* rdr false \) opts)
                    (check-eof-error rdr first-line)
                    (check-invalid-read-cond rdr first-line))
                  ;; feature not matched, ignore next form
                  (read-suppress first-line rdr opts))]
        [matched feature rhs]))))

(defn- read-cond-delimited
  [rdr splicing opts skipped-ws]
  (let [first-line (if (indexing-reader? rdr) (get-line-number rdr) -1)
        [all result] (loop [all []
                            matched nil]
                       (cond
                         ;; still looking for match, read feature+form
                         (nil? matched)
                         (let [[found-match feature rhs] (match-feature first-line rdr opts)
                               finished? (:read-finished (peek feature))
                               _ (assert (not (and rhs finished?))
                                         "Feature only finishes read if rhs is not available")
                               all (-> all
                                       (into (update feature (dec (count feature))
                                                     #(cond-> %
                                                        true (dissoc :read-finished)
                                                        found-match (assoc :matched-feature-key true))))
                                       (into (cond-> rhs
                                               found-match (assoc-in [(dec (count rhs)) :matched-feature-val] true))))]
                           (if finished?
                             [(subvec all 0 (dec (count all))) ;; remove :read-finished placeholder form
                              matched]
                             (recur all (when found-match (peek rhs)))))

                         ;; found match, read and collect the rest
                         (not (:read-finished (peek all)))
                         (recur (into all (read-suppress first-line rdr opts))
                                matched)

                         :else [(subvec all 0 (dec (count all))) ;; remove :read-finished placeholder form
                                matched]))
        all (cond->> [{:op ::list
                       :val (forms->list all) ;;TODO with-meta
                       :forms all}]
              (seq skipped-ws) (into [{:op ::whitespace
                                       :string skipped-ws}]))]
    (if (nil? result)
      ;; no match
      {:op (if splicing ::cond-splicing ::cond)
       :forms all}
      (do
        (assert (contains? result :val))
        (if splicing
          (if (and (#{::list ::vector} (:op result))
                   #_(instance? java.util.List (:val result)))
            {:op ::cond-splicing
             :forms all
             :val (:val result)}
            (err/reader-error rdr "Spliced form list in read-cond-splicing must implement java.util.List."))
          {:op ::cond
           :forms all
           :val (:val result)})))))


(defn- read-cond
  [rdr _ opts]
  (when (not (and opts (#{:allow :preserve} (:read-cond opts))))
    (throw (RuntimeException. "Conditional read not allowed")))
  (if-let [ch (read-char rdr)]
    (let [splicing (= ch \@)
          ch (if splicing (read-char rdr) ch)]
      (when splicing
        (when-not *read-delim*
          (err/reader-error rdr "cond-splice not in list")))
      (if-let [[skipped-ws ch] (if (whitespace? ch)
                                 (update (read-past+skipped whitespace? rdr)
                                         0 #(str ch %))
                                 ["" ch])]
        (if (not= ch \()
          (throw (RuntimeException. "read-cond body must be a list"))
          (let [rc (binding [*suppress-read* (or *suppress-read* (= :preserve (:read-cond opts)))]
                     (if *suppress-read*
                       (let [m (read-list rdr ch opts)]
                         {:op (if splicing ::cond-splicing ::cond)
                          :forms (cond->> [m]
                                   (seq skipped-ws)
                                   (into [{:op ::whitespace
                                           :string skipped-ws}]))
                          :suppress-read true
                          :val (reader-conditional (:val m) splicing)})
                       (read-cond-delimited rdr splicing opts skipped-ws)))]
            (assert (#{::cond ::cond-splicing} (:op rc)))
            rc))
        (err/throw-eof-in-character rdr)))
    (err/throw-eof-in-character rdr)))

(def ^:private ^:dynamic arg-env)

(defn- garg
  "Get a symbol for an anonymous ?argument?"
  [^long n]
  (symbol (str (if (== -1 n) "rest" (str "p" n))
               "__" (RT/nextID) "#")))

(defn- read-fn
  [rdr _ opts]
  (when (thread-bound? #'arg-env)
    (throw (IllegalStateException. "Nested #()s are not allowed")))
  (binding [arg-env (sorted-map)]
    (let [forms (read* (doto rdr (unread \()) true opts) ;; this sets bindings
          _ (assert (= 1 (count forms)))
          _ (assert (#{::list} (:op (first forms))))
          rargs (rseq arg-env)
          args (if rargs
                 (let [higharg (long (key (first rargs)))]
                   (let [args (loop [i 1 args (transient [])]
                                (if (> i higharg)
                                  (persistent! args)
                                  (recur (inc i) (conj! args (or (get arg-env i)
                                                                 (garg i))))))
                         args (if (arg-env -1)
                                (conj args '& (arg-env -1))
                                args)]
                     args))
                 [])]
      {:op ::fn
       :forms (-> forms first :forms)
       :val (list* 'fn* args (forms->vals forms))})))

(defn- register-arg
  "Registers an argument to the arg-env"
  [suffix-str n]
  (if (thread-bound? #'arg-env)
    (let [sym (if-let [ret (arg-env n)]
                ret
                (let [g (garg n)]
                  (set! arg-env (assoc arg-env n g))
                  g))]
      {:op ::fn-arg
       :sym sym
       :string suffix-str
       :val sym})
    (throw (IllegalStateException. "Arg literal not in #()")))) ;; should never hit this

(declare read-symbol)

(defn- read-arg
  [rdr pct opts]
  (if-not (thread-bound? #'arg-env)
    (read-symbol rdr pct)
    (let [ch (peek-char rdr)]
      (cond
       (or (whitespace? ch)
           (macro-terminating? ch)
           (nil? ch))
       (register-arg "" 1)

       (identical? ch \&)
       (do (read-char rdr)
           (register-arg "&" -1))

       :else
       (let [[n & more] (read* rdr true opts)]
         (if (or more
                 (not (and (#{::number} (:op n))
                           (integer? (:val n)))))
           (throw (IllegalStateException. "Arg literal must be %, %& or %integer"))
           (register-arg (:string n) (:val n))))))))

(defn- read-eval
  "Evaluate a reader literal"
  [rdr _ opts]
  (when-not *read-eval*
    (err/reader-error rdr "#= not allowed when *read-eval* is false"))
  (let [forms (read* rdr true opts)]
    {:op ::read-eval
     :forms forms
     :val (-> forms peek :val eval)}))

(def ^:private ^:dynamic gensym-env nil)

(defn- read-unquote
  [rdr comma opts]
  (when-let [ch (peek-char rdr)]
    (if (identical? \@ ch)
      ((wrapping-reader ::unquote-splicing 'clojure.core/unquote-splicing) (doto rdr read-char) \@ opts)
      ((wrapping-reader ::unquote 'clojure.core/unquote) rdr \~ opts))))

(declare syntax-quote*)
(defn- unquote-splicing? [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote-splicing)))

(defn- unquote? [form]
  (and (seq? form)
       (= (first form) 'clojure.core/unquote)))

(defn- expand-list
  "Expand a list by resolving its syntax quotes and unquotes"
  [s]
  (loop [s (seq s) r (transient [])]
    (if s
      (let [item (first s)
            ret (conj! r
                       (cond
                        (unquote? item)          (list 'clojure.core/list (second item))
                        (unquote-splicing? item) (second item)
                        :else                    (list 'clojure.core/list (syntax-quote* item))))]
        (recur (next s) ret))
      (seq (persistent! r)))))

(defn- flatten-map
  "Flatten a map into a seq of alternate keys and values"
  [form]
  (loop [s (seq form) key-vals (transient [])]
    (if s
      (let [e (first s)]
        (recur (next s) (-> key-vals
                          (conj! (key e))
                          (conj! (val e)))))
      (seq (persistent! key-vals)))))

(defn- register-gensym [sym]
  (if-not gensym-env
    (throw (IllegalStateException. "Gensym literal not in syntax-quote")))
  (or (get gensym-env sym)
      (let [gs (symbol (str (subs (name sym)
                                  0 (dec (count (name sym))))
                            "__" (RT/nextID) "__auto__"))]
        (set! gensym-env (assoc gensym-env sym gs))
        gs)))

(defn ^:dynamic resolve-symbol
  "Resolve a symbol s into its fully qualified namespace version"
  [s]
  (if (pos? (.indexOf (name s) "."))
    (if (.endsWith (name s) ".")
      (let [csym (symbol (subs (name s) 0 (dec (count (name s)))))]
        (symbol (str (name (resolve-symbol csym)) ".")))
      s)
    (if-let [ns-str (namespace s)]
      (let [ns (resolve-ns (symbol ns-str))]
        (if (or (nil? ns)
                (= (ns-name* ns) ns-str)) ;; not an alias
          s
          (symbol (ns-name* ns) (name s))))
      (if-let [o ((ns-map *ns*) s)]
        (if (class? o)
          (symbol (.getName ^Class o))
          (if (var? o)
            (symbol (-> ^Var o .ns ns-name*) (-> ^Var o .sym name))))
        (symbol (ns-name* *ns*) (name s))))))

(defn- add-meta [form ret]
  (if (and (instance? IObj form)
           (seq (dissoc (meta form) :line :column :end-line :end-column :file :source)))
    (list 'clojure.core/with-meta ret (syntax-quote* (meta form)))
    ret))

(defn- syntax-quote-coll [type coll]
  ;; We use sequence rather than seq here to fix https://clojure.atlassian.net/browse/CLJ-1444
  ;; But because of https://clojure.atlassian.net/browse/CLJ-1586 we still need to call seq on the form
  (let [res (list 'clojure.core/sequence
                  (list 'clojure.core/seq
                        (cons 'clojure.core/concat
                              (expand-list coll))))]
    (if type
      (list 'clojure.core/apply type res)
      res)))

(defn map-func
  "Decide which map type to use, array-map if less than 16 elements"
  [coll]
  (if (>= (count coll) 16)
    'clojure.core/hash-map
    'clojure.core/array-map))

(defn- syntax-quote* [form]
  (->>
   (cond
    (special-symbol? form) (list 'quote form)

    (symbol? form)
    (list 'quote
          (if (namespace form)
            (let [maybe-class ((ns-map *ns*)
                               (symbol (namespace form)))]
              (if (class? maybe-class)
                (symbol (.getName ^Class maybe-class) (name form))
                (resolve-symbol form)))
            (let [sym (str form)]
              (cond
               (.endsWith sym "#")
               (register-gensym form)

               (.startsWith sym ".")
               form

               :else (resolve-symbol form)))))

    (unquote? form) (second form)
    (unquote-splicing? form) (throw (IllegalStateException. "unquote-splice not in list"))

    (coll? form)
    (cond

     (instance? IRecord form) form
     (map? form) (syntax-quote-coll (map-func form) (flatten-map form))
     (vector? form) (list 'clojure.core/vec (syntax-quote-coll nil form))
     (set? form) (syntax-quote-coll 'clojure.core/hash-set form)
     (or (seq? form) (list? form))
     (let [seq (seq form)]
       (if seq
         (syntax-quote-coll nil seq)
         '(clojure.core/list)))

     :else (throw (UnsupportedOperationException. "Unknown Collection type")))

    (or (keyword? form)
        (number? form)
        (char? form)
        (string? form)
        (nil? form)
        (instance? Boolean form)
        (instance? Pattern form))
    form

    :else (list 'quote form))
   (add-meta form)))

(defn- read-syntax-quote
  [rdr backquote opts]
  (binding [gensym-env {}]
    (let [forms (read* rdr true opts)]
      {:op ::syntax-quote
       :forms forms
       :val (-> forms peek :val syntax-quote*)})))

(defn- read-namespaced-map
  [rdr _ opts]
  (let [[start-line start-column] (starting-line-col-info rdr)
        token (read-token rdr :namespaced-map (read-char rdr))
        auto? (= \: (first token))
        self? (= token ":")]
    (if-let [ns (cond
                  self?
                  (ns-name *ns*)

                  auto?
                  (some-> token (subs 1) parse-symbol second' symbol resolve-ns ns-name)

                  :else
                  (some-> token parse-symbol second'))]

      (let [[skipped-ws ch] (read-past+skipped whitespace? rdr)]
        (if (identical? ch \{)
          (let [forms (read-delimited :namespaced-map \} rdr opts)
                items (forms->vals forms)
                [end-line end-column] (ending-line-col-info rdr)]
            (when (odd? (count items))
              (err/throw-odd-map rdr nil nil items))
            (let [keys (take-nth 2 items)
                  vals (take-nth 2 (rest items))
                  v (with-meta
                      (RT/map (to-array (mapcat list (namespace-keys (str ns) keys) vals)))
                      (when start-line
                        (assoc
                          (when-let [file (get-file-name rdr)]
                            {:file file})
                          :line start-line
                          :column start-column
                          :end-line end-line
                          :end-column end-column)))]
              {:op (if auto? ::auto-namespaced-map ::namespaced-map)
               :forms (into []
                            (filter identity)
                            [(when-not self?
                               {:op ::symbol
                                :string (cond-> token
                                          auto? (subs 1))
                                ;; unclear what this should be
                                :val (symbol ns)})
                             (when (seq skipped-ws)
                               {:op ::whitespace
                                :string skipped-ws})
                             {:op ::map
                              :forms forms
                              ;; can overwrite conflicts, unsure if possible
                              :val (apply hash-map (mapcat list keys vals))}])
               :val v}))
          (err/throw-ns-map-no-map rdr token)))
      (err/throw-bad-ns rdr token))))

(defn- read-comment+string [op]
  (fn [rdr & _]
    (let [[ws eof] (loop [sb (StringBuilder.)
                          ch (read-char rdr)]
                     (if (newline? ch)
                       (if (nil? ch)
                         [(str sb) true]
                         [(str (doto sb (.append ch))) false])
                       (recur (doto sb (.append ch))
                              (read-char rdr))))]
      (cond-> {:op op
               ;; includes full line, including possible terminating newline.
               ;; omits leading comma/shebang
               :string ws}
        eof (assoc :eof true)))))

(defn- macros [ch]
  (case ch
    \" read-string*
    \: read-keyword
    \; (read-comment+string ::comment)
    \' (wrapping-reader ::quote 'quote)
    \@ (wrapping-reader ::deref 'clojure.core/deref)
    \^ read-meta
    \` read-syntax-quote ;;(wrapping-reader ::syntax-quote 'syntax-quote)
    \~ read-unquote
    \( read-list
    \) read-unmatched-delimiter
    \[ read-vector
    \] read-unmatched-delimiter
    \{ read-map
    \} read-unmatched-delimiter
    \\ read-char*
    \% read-arg
    \# read-dispatch
    nil))

(defn- dispatch-macros [ch]
  (case ch
    ;;\^ read-meta                ;deprecated
    \' (wrapping-reader ::var 'var)
    \( read-fn
    \= read-eval
    \{ read-set
    \< (throwing-reader "Unreadable form")
    \" read-regex
    \! (read-comment+string ::shebang)
    \_ read-discard
    \? read-cond
    \: read-namespaced-map
    \# read-symbolic-value
    nil))

(defn- read-ctor [rdr class-name tag-forms opts]
  (when-not *read-eval*
    (err/reader-error rdr "Record construction syntax can only be used when *read-eval* == true"))
  (let [;; note: LispReader.java uses RT/classForNameNonLoading but tools.namespace
        ;; uses a slightly less dynamic version of Class/forName for compatibility with
        ;; clojure 1.7
        class (RT/classForNameNonLoading (name class-name))
        ch (read-past whitespace? rdr)] ;; differs from clojure
    (if-let [[end-ch form] (case ch
                             \[ [\] :short]
                             \{ [\} :extended]
                             nil)]
      (let [entries-forms (read-delimited :record-ctor end-ch rdr opts)
            entries (to-array (forms->vals entries-forms))
            numargs (count entries)
            all-ctors (.getConstructors class)
            ctors-num (count all-ctors)
            forms (into tag-forms entries-forms)]
        (case form
          :short
          {:op ::ctor
           :kind :short
           :forms forms
           :tag-sym class-name
           :val (loop [i 0]
                  (if (>= i ctors-num)
                    (err/reader-error rdr "Unexpected number of constructor arguments to " (str class)
                                      ": got " numargs)
                    (if (== (count (.getParameterTypes ^Constructor (aget all-ctors i)))
                            numargs)
                      (Reflector/invokeConstructor class entries)
                      (recur (inc i)))))}
          :extended
          {:op ::ctor
           :kind :extended
           :forms forms
           :tag-sym class-name
           :val (let [vals (RT/map entries)]
                  (loop [s (keys vals)]
                    (if s
                      (if-not (keyword? (first s))
                        (err/reader-error rdr "Unreadable ctor form: key must be of type clojure.lang.Keyword")
                        (recur (next s)))))
                  (Reflector/invokeStaticMethod class "create" (object-array [vals])))}))
      (err/reader-error rdr "Invalid reader constructor form"))))

(defn- read-tagged [rdr initch opts]
  (let [tag-forms (read* rdr true opts)
        _ (when-not (and (= 1 (count tag-forms))
                         (= ::symbol (:op (first tag-forms))))
            (throw (ex-info (str "Expected symbol as tag, found " tag-forms)
                            {})))
        tag (-> tag-forms first :val)]
    (when-not (symbol? tag)
      (err/throw-bad-reader-tag rdr tag))
    (if *suppress-read*
      (let [forms (read* rdr true opts)
            [arg-val :as all-vals] (forms->vals forms)]
        (assert (= 1 (count all-vals)))
        {:op ::tagged
         :forms forms
         :tag-sym tag
         :suppressed true
         :val (tagged-literal tag arg-val)})
      (if-let [f (or (*data-readers* tag)
                     (default-data-readers tag))]
        (let [rhs-forms (read* rdr true opts)
              rhs (-> rhs-forms peek :val)
              frm (f rhs)]
          {:op ::tagged
           :tag-sym tag
           :val frm
           :forms (vec (concat tag-forms
                               rhs-forms))})
        (if (.contains (name tag) ".")
          (read-ctor rdr tag tag-forms opts)
          (if-let [f *default-data-reader-fn*]
            (f tag (read* rdr true opts))
            (err/throw-unknown-reader-tag rdr tag)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:dynamic *read-eval*
  "Defaults to true.

   ***WARNING***
   This setting implies that the full power of the reader is in play,
   including syntax that can cause code to execute. It should never be
   used with untrusted sources. See also: clojure.tools.reader.edn/read.

   When set to logical false in the thread-local binding,
   the eval reader (#=) and *record/type literal syntax* are disabled in read/load.
   Example (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))

   When set to :unknown all reads will fail in contexts where *read-eval*
   has not been explicitly bound to either true or false. This setting
   can be a useful diagnostic tool to ensure that all of your reads
   occur in considered contexts."
  true)

(def ^:dynamic *data-readers*
  "Map from reader tag symbols to data reader Vars.
   Reader tags without namespace qualifiers are reserved for Clojure.
   Default reader tags are defined in clojure.tools.reader/default-data-readers
   and may be overridden by binding this Var."
  {})

(def ^:dynamic *default-data-reader-fn*
  "When no data reader is found for a tag and *default-data-reader-fn*
   is non-nil, it will be called with two arguments, the tag and the value.
   If *default-data-reader-fn* is nil (the default value), an exception
   will be thrown for the unknown tag."
  nil)

(def ^:dynamic *suppress-read* false)

(def default-data-readers
  "Default map of data reader functions provided by Clojure.
   May be overridden by binding *data-readers*"
  {'inst #'data-readers/read-instant-date
   'uuid #'data-readers/default-uuid-reader})

;; TODO :top-level flag in AST
(defn ^:private read*
  ([reader eof-error? opts] (read* reader eof-error? nil opts))
  ([reader eof-error? return-on opts]
     (when (= :unknown *read-eval*)
       (err/reader-error "Reading disallowed - *read-eval* bound to :unknown"))
     (try
       (loop [forms []]
         (let [ms (loop [^StringBuilder ws (StringBuilder.)]
                    ;; some acrobatics to avoid recurring over try (via log-source).
                    ;; we need log-source to wrap around the initial read-char that
                    ;; starts a form.
                    (let [continue? (volatile! nil)
                          ws-or-done
                          ;; promise so log-source's return value is the form
                          (log-source reader
                            (let [ch (read-char reader)]
                              (cond
                                (whitespace? ch)
                                (do (vreset! continue? true)
                                    (.append ws ch))
                                :else
                                (let [_ (vreset! continue? false)
                                      ws (str ws)
                                      prefix-form (when (pos? (count ws))
                                                    {:op ::whitespace
                                                     :string ws})
                                      wrap-prefix-form (fn [m]
                                                         (if prefix-form
                                                           [prefix-form m]
                                                           [m]))]
                                  (cond
                                    (nil? ch) (if eof-error?
                                                (err/throw-eof-error reader nil)
                                                (if prefix-form
                                                  [(assoc prefix-form :eof true)]
                                                  [{:op ::forms
                                                    :eof true
                                                    :forms []}]))
                                    (= ch return-on) (if prefix-form
                                                       [(assoc prefix-form :read-finished true)]
                                                       [{:op ::forms
                                                         :read-finished true
                                                         :forms []}])
                                    (number-literal? reader ch) (wrap-prefix-form
                                                                  (read-number reader ch))

                                    :else (wrap-prefix-form
                                            (if-let [f (macros ch)]
                                              (f reader ch opts)
                                              (read-symbol reader ch))))))))
                          continue? @continue?
                          _ (assert (boolean? continue?))]
                      (if continue?
                        (recur ws-or-done)
                        ws-or-done)))
               _ (assert (vector? ms))
               forms (into forms ms)
               pf (peek forms)]
           (if (or (contains? pf :val)
                   (:eof pf)
                   (:read-finished pf))
             (if (and (:eof pf)
                      (not (contains? pf :val))
                      eof-error?)
               (err/throw-eof-error reader nil)
               forms)
             (recur forms))))
        (catch Exception e
          (if (ex-info? e)
            (let [d (ex-data e)]
              (if (= :reader-exception (:type d))
                (throw e)
                (throw (ex-info (.getMessage e)
                                (merge {:type :reader-exception}
                                       d
                                       (if (indexing-reader? reader)
                                         {:line   (get-line-number reader)
                                          :column (get-column-number reader)
                                          :file   (get-file-name reader)}))
                                e))))
            (throw (ex-info (.getMessage e)
                            (merge {:type :reader-exception}
                                   (if (indexing-reader? reader)
                                     {:line   (get-line-number reader)
                                      :column (get-column-number reader)
                                      :file   (get-file-name reader)}))
                            e)))))))

(defn read+ast
  "Reads the first object from an IPushbackReader or a java.io.PushbackReader.
   Returns the AST representing the object read. If EOF, throws if eof-error? is true.
   Otherwise returns AST. If no stream is provided, *in* will be used.

   Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals

   ***WARNING***
   Note that read+ast can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read

   Note that the function signature of clojure.tools.reader/read and
   clojure.tools.reader.edn/read is not the same for eof-handling"
  ([] (read+ast {} true *in*))
  ([reader] (read+ast {} true reader))
  ([opts reader] (read+ast opts true reader))
  ([opts eof-error? reader]
   (when (source-logging-reader? reader)
     (let [^StringBuilder buf (:buffer @(.source-log-frames ^SourceLoggingPushbackReader reader))]
       (.setLength buf 0)))
   (top-level-forms-ast
     (read* reader eof-error? opts))))

(defn read
  "Reads the first object from an IPushbackReader or a java.io.PushbackReader.
   Returns the object read. If EOF, throws if eof-error? is true.
   Otherwise returns sentinel. If no stream is provided, *in* will be used.

   Opts is a persistent map with valid keys:
    :read-cond - :allow to process reader conditionals, or
                 :preserve to keep all branches
    :features - persistent set of feature keywords for reader conditionals
    :eof - on eof, return value unless :eofthrow, then throw.
           if not specified, will throw

   ***WARNING***
   Note that read can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read

   Note that the function signature of clojure.tools.reader/read and
   clojure.tools.reader.edn/read is not the same for eof-handling"
  {:arglists '([] [reader] [opts reader] [reader eof-error? eof-value])}
  ([] (read *in* true nil))
  ([reader] (read reader true nil))
  ([{eof :eof :as opts :or {eof :eofthrow}} reader]
   (let [ast (read+ast opts (= eof :eof-error?) reader)]
     (if (:eof ast)
       eof
       (:val ast))))
  ([reader eof-error? sentinel]
   (let [ast (read+ast {} eof-error? reader)]
     (if (:eof ast)
       sentinel
       (:val ast)))))

(defn read-string+ast
  "Reads one object from the string s.
   Returns the resulting AST.

   ***WARNING***
   Note that read-string+ast can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read-string

   Note that the function signature of clojure.tools.reader/read-string and
   clojure.tools.reader.edn/read-string is not the same for eof-handling"
  ([s] (read-string+ast {} true s))
  ([opts s] (read-string+ast opts true s))
  ([opts eof-error? s] (read+ast opts eof-error? (string-push-back-reader (or s "")))))

(defn read-string
  "Reads one object from the string s.
   Returns nil when s is nil or empty.

   ***WARNING***
   Note that read-string can execute code (controlled by *read-eval*),
   and as such should be used only with trusted sources.

   To read data structures only, use clojure.tools.reader.edn/read-string

   Note that the function signature of clojure.tools.reader/read-string and
   clojure.tools.reader.edn/read-string is not the same for eof-handling"
  ([s] (read-string {} s))
  ([{:keys [eof] :as opts :or {eof :eofthrow}} s]
   (:val (read-string+ast opts (= :eofthrow eof) s))))

;; general approach with AST ops:
;; - don't split up tokens (read-token)
;;   - consider tokens as atomic units
;;   - generally, if it parses to an atomic Clojure value, don't split
;;   - but, no "token" AST node
;; - prefer string tokens over reusing existing ops
;;   - eg., keyword is colon + token, not colon + symbol
;; - tastefully split up related syntax
;;   - eg., : => ::keyword
;;          :: => ::auto-keyword
;;          #? => ::cond
;;          #?@ => ::cond-splicing
;;   - leave numbers like 1M alone since they're a token
;; - use one :forms vector for composite syntax
;;   - each AST is either atomic or composite
;;     - atomic have :string, omit prefix/suffix that can be derived from :op
;;       - eg., {:op ::string, :string "foo"} => "\"foo\""
;;     - composite don't have :string, instead have a single :forms for other AST nodes
;;       - consider "strange" or "special" characters that require special parsing
;;         as implicit
;;         - eg., ::keyword vs ::auto-keyword
;;     - :forms may have multiple values
;;       - eg., ::meta always has 2 forms, ::map an even number of them
;; - presence of :val entry signifies something is a form, otherwise whitespace
;; - {:eof true} on eof
;; - add minimum to AST for roundtrip
;;   - ::keyword + "asdf" over ::keyword + ":asdf"
;;   - don't split up tokens to achieve this
;;     - ::symbol + "a/b" over ::symbol + {:ns "a" :name "b"}
;;   - ::nil => nil, not ::nil + "nil" => nil
;; - use ::forms with empty :form as "nothing"
;;   - useful for propagating :eof
;;     - :eof shouldn't be attached to AST nodes representing values
;;       - don't read ahead
(defn ast->StringBuilder
  "Appends the string representation of ast to StringBuilder sb."
  ([ast] (ast->StringBuilder ast (StringBuilder.)))
  ([ast ^StringBuilder sb]
   (let [append #(.append sb %)]
     (letfn [(forms->string [forms]
               (doseq [form forms]
                 (ast->StringBuilder form)))
             (ast->StringBuilder [ast]
               (case (:op ast)
                 ::forms (forms->string (:forms ast))
                 ::map (do (append \{)
                           (forms->string (:forms ast))
                           (append \}))
                 ::set (do (append "#{")
                           (forms->string (:forms ast))
                           (append \}))
                 ::vector (do (append \[)
                              (forms->string (:forms ast))
                              (append \]))
                 ::list (do (append \()
                            (forms->string (:forms ast))
                            (append \)))
                 ::fn (do (append "#(")
                          (forms->string (:forms ast))
                          (append \)))
                 ::quote (do (append \')
                             (forms->string (:forms ast)))
                 ::discard (do (append "#_")
                               (forms->string (:forms ast)))
                 ::meta (do (append \^)
                            (forms->string (:forms ast)))
                 ::deref (do (append \@)
                             (forms->string (:forms ast)))
                 ::unquote (do (append \~)
                               (forms->string (:forms ast)))
                 ::unquote-splicing (do (append "~@")
                                        (forms->string (:forms ast)))
                 ::syntax-quote (do (append \`)
                                    (forms->string (:forms ast)))

                 (::nil ::symbol ::number ::whitespace ::boolean) (append (:string ast))

                 ::string (do (append \")
                              (append (:string ast))
                              (append \"))
                 ::regex (do (append "#\"")
                             (append (:string ast))
                             (append \"))
                 ::namespaced-map (do (append "#:")
                                      (forms->string (:forms ast)))
                 ::auto-namespaced-map (do (append "#::")
                                           (forms->string (:forms ast)))
                 ::cond (do (append "#?")
                            (forms->string (:forms ast)))
                 ::cond-splicing (do (append "#?@")
                                     (forms->string (:forms ast)))
                 ::shebang (do (append \#)
                               ;; vim doesn't like shebangs here
                               (append \!)
                               (append (:string ast)))
                 ::comment (do (append \;)
                               (append (:string ast)))
                 ::keyword (do (append \:)
                               (append (:string ast)))
                 ::auto-keyword (do (append "::")
                                    (append (:string ast)))
                 ::symbolic-value (do (append "##")
                                      (append (:string ast)))
                 ::character (do (append \\)
                                 (append (:string ast)))
                 ::fn-arg (do (append \%)
                              (append (:string ast)))
                 ::tagged (do (append "#")
                              (forms->string (:forms ast)))
                 ::read-eval (do (append "#=")
                                 (forms->string (:forms ast)))
                 ::ctor (do (append "#")
                            (append (str (:tag-sym ast)))
                            (forms->string (:forms ast)))
                 ::var (do (append "#'")
                           (forms->string (:forms ast)))

                 (throw (ex-info (str "Unknown AST " (pr-str (:op ast)))
                                 {:op (:op ast)}))))]
       (ast->StringBuilder ast)
       sb))))

(defn ast->string
  "Returns the string representation of an AST."
  [ast]
  (str (ast->StringBuilder ast)))

;; TODO add explicit field for this
(defn has-val? [ast]
  (contains? ast :val))
