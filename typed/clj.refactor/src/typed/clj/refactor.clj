(ns typed.clj.refactor
  "Alpha"
  (:refer-clojure :exclude [run!])
  (:require [clojure.string :as str]
            [typed.clj.analyzer :as ana]
            [typed.cljc.analyzer :as ana-cljc]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as ana-env]
            [typed.clj.reader :as rdr]
            [typed.clj.reader.reader-types :refer [indexing-push-back-reader]])
  (:import [java.io StringReader BufferedReader]
           [clojure.lang LineNumberingPushbackReader]))

(set! *warn-on-reflection* true)

(defn update-rdr-children [ast f]
  (if-some [forms (not-empty (:forms ast))]
    (assoc ast :forms (mapv f forms))
    ast))

(defmacro ^:private kw-case
  "case syntax compiling to a cond using identical?."
  [kw & args]
  (assert args)
  (let [gop (gensym 'op)]
    `(let [~gop ~kw
           _# (assert (keyword? ~gop))]
       (cond
         ~@(mapcat (fn [c]
                     (if (= 1 (count c))
                       [:else (first c)]
                       (let [[k-or-ks r] c
                             ks (if (keyword? k-or-ks)
                                  [k-or-ks]
                                  (do
                                    (assert (seq? k-or-ks))
                                    k-or-ks))
                             _ (assert (seq ks))
                             _ (assert (every? keyword? ks) ks)
                             ks-conds (mapv (fn [k]
                                              `(identical? ~gop ~k))
                                            ks)]
                         [(if (= 1 (count ks-conds))
                            (first ks-conds)
                            `(or ~@ks-conds))
                          r])))
                   (partition-all 2 args))
         ~@(when (even? (count args))
             [:else `(throw (ex-info (str "No case for: " ~gop)))])))))

(comment
  (macroexpand `(kw-case 1 :a 2 3))
  (macroexpand `(kw-case 1 :a 2 :b 3))
  (macroexpand `(kw-case 1 :a 2 :b 3 4))
  (macroexpand `(kw-case 1 :a 2 (:b :c) 3 4))
  )

(declare ^:private refactor-form*)

(defmulti ^:private -refactor-form* (fn [expr rdr-ast opt] (::ana-cljc/op expr)))

(defmethod -refactor-form* ::ana-cljc/local
  [{:keys [tag form] sym :name :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:local (cond->
                       {:local (:local expr)
                        :name sym
                        :form form}
                       (and (not (:tag form)) tag)
                       (assoc :inferred-tag tag))])))
  expr)

(defmethod -refactor-form* ::ana-cljc/var
  [{:keys [form var] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:var {:sym (symbol var)}])))
  expr)

(defmethod -refactor-form* ::ana-cljc/the-var
  [{:keys [form var] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta (second form))]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:var {:sym (symbol var)}])))
  expr)

(defmethod -refactor-form* ::ana-cljc/binding
  [{:keys [form local env] sym :name :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [mta (meta form)]
    (when ((every-pred :line :column) mta)
      (swap! file-map-atom assoc
             (select-keys mta [:line :column
                               :end-line :end-column
                               :file])
             [:binding {:local local
                        :name sym
                        :sym sym}])))
  expr)

(defmethod -refactor-form* :default
  [expr rdr-ast opt]
  (-> expr
      (ast/update-children #(refactor-form* % rdr-ast opt))))

(defmulti -refactor-special (fn [op-sym expr rdr-ast opt] op-sym))

(defn ^:private run!
  "Runs the supplied procedure (via reduce), for purposes of side
  effects, on successive items in the collection. Returns nil"
  {:added "1.7"}
  [proc coll]
  (reduce #(do (proc %2) nil) nil coll)
  nil)

(defn record-destructuring [b file-map-atom]
  (cond
    (vector? b) (do
                  (when (seq (meta b))
                    (swap! file-map-atom assoc
                           (select-keys (meta b)
                                        [:line :column
                                         :end-line :end-column
                                         :file])
                           [:vector-destructure {:binding b}]))
                  (run! #(record-destructuring % file-map-atom) b))
    (map? b) (do
               (when (seq (meta b))
                 (swap! file-map-atom assoc
                        (select-keys (meta b)
                                     [:line :column
                                      :end-line :end-column
                                      :file])
                        [:map-destructure {:binding b}]))
               (->> b
                    (run! (fn [[bi v :as orig]]
                            (cond
                              (#{:as} bi) [orig]
                              (and (#{:keys} bi)
                                   (vector? v))
                              (run! (fn [bk]
                                      (when (seq (meta bk))
                                        (swap! file-map-atom assoc
                                               (select-keys (meta bk)
                                                            [:line :column
                                                             :end-line :end-column
                                                             :file])
                                               [:map-destructure-keys-entry {:binding bk}])))
                                    v)
                              ;;TODO namespaced keywords, :syms :strs
                              (keyword? bi) nil
                              :else nil)))))))

(comment
  (let [a (atom {})]
    (record-destructuring
      '{:keys [a b]}
      a)
    @a)
  )

(defmethod -refactor-special 'clojure.core/let
  [_ {:keys [form] :as expr} rdr-ast {:keys [file-map-atom] :as opt}]
  (let [bindings (second form)]
    (when (and (vector? bindings)
               (even? (count bindings)))
      (let [_ (doseq [[b v] (partition 2 bindings)]
                (record-destructuring b file-map-atom))]
        (-> expr
            ana-cljc/analyze-outer
            (refactor-form* rdr-ast opt))))))

(defmethod -refactor-special :default [_ _ _ _] nil)

(defmulti ^:private -post-refactor-form* (fn [expr rdr-ast opt] (::ana-cljc/op expr)))

(defmethod -post-refactor-form* ::ana/instance-call
  [expr _ _]
  #_
  (prn `-post-refactor-form* ::ana/instance-call
       (:tag expr)
       (:class expr)
       (:method expr)
       (count (:args expr))
       (:children expr)
       (-> expr :instance :tag))
  expr)

(defmethod -post-refactor-form* :default [expr  _ _]
  expr)

(defn- refactor-form* [expr rdr-ast {:keys [file-map-atom] :as opt}]
  (let [refactor-form* (fn [expr]
                         (kw-case (::ana-cljc/op expr)
                           ::ana-cljc/unanalyzed (let [{:keys [form env]} expr
                                                       op-sym (ana/resolve-op-sym form env (ana/default-opts))]
                                                   (when op-sym
                                                     (let [mta (meta (first form))]
                                                       (when ((every-pred :line :column) mta)
                                                         (swap! file-map-atom assoc
                                                                (select-keys mta [:line :column
                                                                                  :end-line :end-column
                                                                                  :file])
                                                                [:var {:sym op-sym}]))))
                                                   (or (some-> op-sym (-refactor-special expr rdr-ast opt))
                                                       (recur (ana-cljc/analyze-outer expr))))
                           (-> expr
                               ana-cljc/run-pre-passes
                               (-refactor-form* rdr-ast opt)
                               ana-cljc/run-post-passes
                               ana-cljc/eval-top-level
                               (-post-refactor-form* rdr-ast opt))))
        expr (refactor-form* expr)]
    expr))

(defn- file-map [form rdr-ast opt]
  (let [{:keys [file-map-atom] :as opt}
        (cond-> opt
          (not (:file-map-atom opt)) (assoc :file-map-atom (atom {})))
        env (ana/empty-env (ns-name *ns*))]
    (with-bindings (ana/default-thread-bindings env)
      (let [opts (ana-env/ensure (ana/default-opts) (ana/global-env))]
        (-> form
            (ana-cljc/unanalyzed-top-level env)
            (refactor-form* rdr-ast opt))))
    @file-map-atom))

;; pre pass
(defn fq-rdr-ast [rdr-ast opt]
  (if (::skip-fq-rdr-ast rdr-ast)
    rdr-ast
    (let [;_ (prn `fq-rdr-ast (:op rdr-ast))
          reanalyze (fn [forms]
                      (assert (vector? forms))
                      (assert (seq forms))
                      (-> {:op ::rdr/forms
                           :forms forms}
                          rdr/ast->string
                          (indexing-push-back-reader
                            1
                            "example.clj"
                            (select-keys
                              (meta (-> forms first :val))
                              [:line :column]))
                          rdr/read))
          file-map (or
                     (cond
                       (and (:top-level rdr-ast)
                            (some? (:val rdr-ast)))
                       (file-map (:val rdr-ast) rdr-ast opt)

                       (identical? ::rdr/read-eval (:op rdr-ast))
                       (let [form (reanalyze (:forms rdr-ast))
                             fm (file-map form
                                          rdr-ast
                                          opt)]
                         fm)

                       (or (identical? ::rdr/cond (:op rdr-ast))
                           (identical? ::rdr/cond-splicing (:op rdr-ast)))
                       (let [lst-form (peek (:forms rdr-ast))
                             _ (assert (= ::rdr/list (:op lst-form)))]
                         ;; there's probably some case where no features are matched
                         ;; but we still want to read the form.
                         (when-some [matched-val (some (fn [f]
                                                         (when (:matched-feature-val f)
                                                           f))
                                                       (:forms lst-form))]
                           (let [form (reanalyze [matched-val])
                                 fm (file-map form
                                              rdr-ast
                                              opt)]
                             fm)))

                       :else (::file-map rdr-ast))
                     {})
          assoc-file-map #(assoc % ::file-map file-map)
          rdr-ast (kw-case (:op rdr-ast)
                    ::rdr/symbol
                    (let [{:keys [val]} rdr-ast]
                      (if-let [mapped (file-map (select-keys (meta val) [:line :column
                                                                         :end-line :end-column
                                                                         :file]))]
                        (kw-case (first mapped)
                          :var (cond-> rdr-ast
                                 true assoc-file-map
                                 (:fully-qualify opt) (assoc :string (str (:sym (second mapped)))))
                          (:local :binding)
                          (let [info (second mapped)
                                wrap-discard (fn [sym-ast]
                                               {:op ::rdr/forms
                                                ::file-map file-map
                                                :val (:val sym-ast)
                                                :forms [{:op ::rdr/discard
                                                         ::file-map file-map
                                                         :forms [{:op ::rdr/keyword
                                                                  ::file-map file-map
                                                                  :string (str (symbol (:local info)))
                                                                  :val (keyword (:local info))}]}
                                                        {:op ::rdr/whitespace
                                                         ::file-map file-map
                                                         :string " "}
                                                        (assoc sym-ast
                                                               ::skip-fq-rdr-ast true)]})
                                sym-ast (cond-> rdr-ast
                                          true assoc-file-map
                                          (:hygienic-locals opt) (assoc :string (-> info :name str))
                                          (:add-local-origin opt) wrap-discard)]
                            (if-some [tag (when (:add-inferred-tag opt)
                                            (:inferred-tag info))]
                              {:op ::rdr/meta
                               ::file-map file-map
                               :val (vary-meta (:val sym-ast) assoc :tag tag)
                               :forms [{:op ::rdr/symbol
                                        ::file-map file-map
                                        :string (str tag)
                                        :val tag}
                                       {:op ::rdr/whitespace
                                        ::file-map file-map
                                        :string " "}
                                       (assoc sym-ast
                                              ::skip-fq-rdr-ast true)]}
                              sym-ast)))
                        (-> rdr-ast
                            assoc-file-map
                            (update-rdr-children assoc-file-map))))
                    (-> rdr-ast
                        assoc-file-map
                        (update-rdr-children assoc-file-map)))]
      rdr-ast)))

;; pre pass
(defn indent-by [{:keys [top-level] :as rdr-ast} indent]
  {:post [(:op %)]}
  (if (::skip-indent-by rdr-ast)
    rdr-ast
    (kw-case (:op rdr-ast)
      ::rdr/whitespace (update rdr-ast :string
                               str/replace "\n" (str "\n" (apply str (repeat indent \space))))
      (let [indent-top-level #(do {:op ::rdr/forms
                                   :forms [{:op ::rdr/whitespace
                                            ::skip-indent-by true
                                            :string (apply str (repeat indent \space))}
                                           (assoc % ::skip-indent-by true)]})]
        (cond-> rdr-ast
          top-level
          indent-top-level)))))

;; pre-pass
;; - must be followed by delete-orphan-whitespace
;; FIXME probably better to move to post pass? then we can delete
;; the discard form entirely.
(defn delete-discard [rdr-ast]
  (kw-case (:op rdr-ast)
    ::rdr/discard {:op ::rdr/forms
                   ::delete-preceding-whitespace true
                   :forms []}
    rdr-ast))

(defn compile-passes [pre post]
  (fn r [rdr-ast]
    (-> rdr-ast
        pre
        (update-rdr-children r)
        post)))

;; post pass
;; - depends on delete-discard
(defn delete-orphan-whitespace [{:keys [forms] :as rdr-ast}]
  (let [forms (reduce (fn [forms i]
                        (if (zero? i)
                          ;; propagate up to parent
                          forms
                          (cond-> forms
                            (::delete-preceding-whitespace (nth forms i))
                            (-> (update i dissoc ::delete-preceding-whitespace)
                                (update (dec i)
                                        (fn [maybe-ws]
                                          (kw-case (:op maybe-ws)
                                            ::rdr/whitespace
                                            (update maybe-ws :string str/trimr)
                                            ;; no preceding whitespace
                                            maybe-ws)))))))
                      forms
                      (range (count forms)))]
    (cond-> (assoc rdr-ast :forms forms)
      (and (pos? (count forms))
           (-> forms (nth 0) ::delete-preceding-whitespace))
      (-> (assoc ::delete-preceding-whitespace true)
          (update-in [:forms 0] dissoc ::delete-preceding-whitespace)))))

;; TODO reformat based on macro's args
;; - [foo & body] => case/with-open/cond-> indentation
;;   - exceptions: ->
;; - [& args] => cond indentation
;; TODO how to detect macros like cond-> use even numbered args?
;; TODO check macro's expansion to inherit indentation (eg., thin wrapper around threading macro)
;; TODO detect if something is a kv binding form and indent like let's binding form
;; TODO reformat function calls to (f arg1
;;                                    arg2)
;; TODO prefer update/update-in path on different line to function if newline needed
(defn reformat-rdr-ast-pre [rdr-ast opt]
  (prn (:op rdr-ast))
  (kw-case (:op rdr-ast)
    (::rdr/list ::rdr/fn) (let [forms (reduce (fn [forms i]
                                                (let [rdr-ast (nth forms i)]
                                                  (kw-case (:op rdr-ast)
                                                    ::rdr/whitespace (do
                                                                       ;; ignoring ::rdr/forms in between ws for now...
                                                                       (assert (not= ::rdr/whitespace (:op (nth forms (dec i) nil)))
                                                                               "Didn't expect adjacent whitespace")
                                                                       (update-in forms [i :string]
                                                                                  (fn [s]
                                                                                    (str
                                                                                      (str/trim s)
                                                                                      (when (< 0 i (dec (count forms)))
                                                                                        " ")))))
                                                    ::rdr/forms (do (assert (empty? (:form rdr-ast))
                                                                            "More thinking required if this happens")
                                                                    forms)
                                                    forms)))
                                              (:forms rdr-ast)
                                              (range (count (:forms rdr-ast))))]
                            (assoc rdr-ast :forms forms))
    ;; TODO top-level whitespace
    ::rdr/forms (cond-> rdr-ast
                  (or (:top-level rdr-ast)
                      (::reformat-top-level rdr-ast))
                  (update-rdr-children (fn [ast]
                                         (when (= ::rdr/forms (:op ast))
                                           (assert (empty? (:forms rdr-ast))))
                                         (assoc ast ::reformat-top-level true))))
    rdr-ast))

(defn reformat-rdr-ast-post [rdr-ast opt]
  rdr-ast)

(defn refactor-form-string
  ([s] (refactor-form-string s {}))
  ([s opt]
   {:pre [(string? s)]
    :post [(string? %)]}
   (with-open [rdr (-> s
                       StringReader.
                       LineNumberingPushbackReader.
                       (indexing-push-back-reader 1 "example.clj"))]
     (let [read-next #(rdr/read+ast {:read-cond :allow
                                     :features #{:clj}}
                                    ;; eof-error
                                    false
                                    rdr)
           opt (cond-> opt
                 (not (:file-map-atom opt)) (assoc :file-map-atom (atom {})))
           ;; left-to-right order
           pre-passes (filterv identity
                               [#(fq-rdr-ast % opt)
                                (when (:delete-discard opt)
                                  delete-discard)
                                (when (:indent-by opt)
                                  #(indent-by % 2))
                                (when (:reformat opt)
                                  #(reformat-rdr-ast-pre % opt))])
           ;; left-to-right order
           post-passes (filterv identity
                                [(when (:delete-discard opt)
                                   delete-orphan-whitespace)
                                 (when (:reformat opt)
                                   #(reformat-rdr-ast-post % opt))])
           passes (compile-passes (apply comp (rseq pre-passes))
                                  (apply comp (rseq post-passes)))
           sb (StringBuilder.)]
       (loop [{:keys [eof] form :val :as rdr-ast} (read-next)]
         (let [rdr-ast (passes rdr-ast)
               _ (rdr/ast->StringBuilder rdr-ast sb)]
           (if eof
             (str sb)
             (recur (read-next)))))))))

(comment (scratch-buffer))

(defn scratch-buffer []
  (refactor-form-string "(map identity [1 2 3])"
                        {:fully-qualify true})
  ;=> "(sequence (map identity) [1 2 3])"
  (refactor-form-string "(merge {:a 1 :b 2} {:c 3 :d 4})" {})
  ;=> "(into {:a 1 :b 2} {:c 3 :d 4})"
  (refactor-form-string "(merge {+ 1 :b 2} {:c 3 :d 4})" {})
  (refactor-form-string "(merge {#'+ 1 :b 2} {:c 3 :d 4})" {})
  (refactor-form-string "#'+")
  (refactor-form-string "#'+" {:fully-qualify true})
  (refactor-form-string "1 #!a\n;a")
  (refactor-form-string "(fn [])" {})
  (refactor-form-string "(fn [a] (a))" {:hygienic-locals true
                                        :add-local-origin true})
  (refactor-form-string "(letfn [(a [])] (a))" {:hygienic-locals true
                                                :add-local-origin true})
  ;; FIXME :right overwrites :left, but has no info
  (refactor-form-string "#(letfn [(a [] (prn :left)) (a [] (prn :right))] (a))"
                        {:hygienic-locals true
                         :add-local-origin true})
  (refactor-form-string "(fn [a] (+ a 3))" {})

  (refactor-form-string "+" {:fully-qualify true})
  ;=> "clojure.core/+"
  (refactor-form-string "(defn foo [a] (+ a 3))" {})
  ;=> "(clojure.core/defn foo [a__#0] (clojure.core/+ a__#0 3))"
  ;; TODO expand `+ => `clojure.core/+
  (refactor-form-string "`+" {:fully-qualify true})
  (refactor-form-string "(defmacro foo [a] `(+ ~a 3))"
                        {:fully-qualify true
                         :hygienic-locals true})
  ;=> "(clojure.core/defmacro foo [a__#0] `(+ ~a__#0 3))"
  (refactor-form-string "(defmacro foo [a] `(+ ~@a 3))"
                        {:hygienic-locals true})
  (refactor-form-string "(str (fn []))" {})
  ;;FIXME macro vars don't expand?
  (refactor-form-string "`fn" {:fully-qualify true})
  (let [a (atom {})]
    (refactor-form-string "(fn [{:keys [a]}] a)"
                          {:file-map-atom a})
    @a)
  (refactor-form-string "#(.string 1)" {})
  (refactor-form-string "#(.string %)" {})
  (refactor-form-string "#(.string (identity []))" {})
  (refactor-form-string "#(.string #?(:clj 1) (identity []))" {})
  (refactor-form-string "(let [^Long a {}] a)" {})
  (refactor-form-string "(let [^Long a {} b a] b)" {})
  (refactor-form-string "(let [^Long a {} b a c b] b)" {})
  ;; idea: trim redundant tag
  (refactor-form-string "(let [^Long a {} ^Long b a] b)"
                        {})
  (refactor-form-string "(let [^Long a {} ^Long b a] b)"
                        {:add-local-origin true})
  (refactor-form-string "(do #_1)" {})
  (refactor-form-string "(do #_1)"
                        {:delete-discard true})
  (refactor-form-string " #_1 1"
                        {:delete-discard true})
  (println
    (refactor-form-string "(do \n\n#_1)"
                          {:delete-discard true}))
  (println
    (refactor-form-string "(map identity\n)"
                          {:indent-by true}))
  (println
    (refactor-form-string "(map     #(\n+ 1 2))"
                          {:reformat true}))
  (println
    (refactor-form-string " (  map     #(\n+ 1 2))"
                          {:reformat true}))
  (println
    (refactor-form-string " (;asdf\n map     #(\n+ 1 2))"
                          {:reformat true}))
  (println
    (refactor-form-string "1\n2\n3"))
  (println
    (refactor-form-string "   "))

  ;; FIXME print distinct bindings
  (println
    (refactor-form-string "(let [a #=(let [a 1] a)] a)"))
  ;; FIXME #= should preserve line/column numbers when reanalyzing (see fq-rdr-ast)
  (println
    (refactor-form-string "(let [a #=(let [b 1] b)] a)"))
  (println
    (refactor-form-string "(let [a #?(:clj (let [b 1] b))] a)"))
  (println
    (refactor-form-string "(let [a #?@(:clj [(let [b 1] b)])] a)"))
  (println
    (refactor-form-string "(let [a #?@(:default [(let [b 1] b)])] a)"))
  ;; only expands matched feature
  (println
    (refactor-form-string "(let [a #?@(:clj [(let [b 1] b)] :default [(let [b 1] b)])] a)"))
  (println
    (refactor-form-string "(let [a #?@ (:clj [(let [b 1] b)] :default [(let [b 1] b)])] a)"))

  ;; types as comments
  (println
    (refactor-form-string
      (str/join "\n"
                (vector "(let [; :typed.clj t/Str"
                        "      ; :typed.cljs t/Str"
                        "      a (str 1)]"
                        "  a)"))))
  (println
    (refactor-form-string
      "#(let [f (java.io.File. \"a\")] f)"
      {:add-inferred-tag true}))
  )
