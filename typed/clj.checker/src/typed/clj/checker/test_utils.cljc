(ns ^:typed.clojure typed.clj.checker.test-utils
  (:require #?(:clj [babashka.fs :as fs])
            [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test :refer [is]]
            [typed.clj.checker.check :as chk]
            [typed.clj.checker.parse-unparse :refer [parse-type parse-clj] :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.clj.runtime.env :as clj-env]
            [typed.cljc.checker.test-utils :as common-test]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.clj.checker.utils :refer [->opts]]))

(defn check-opt [opt]
  #_(assert (empty? (set/difference (set (keys opt))
                                    #{:expected :ret}))))

(def ^:dynamic *remove-nsyms* nil)

(defmacro with-delayed-remove-ns [& body]
  `(binding [*remove-nsyms* (atom #{})]
     (let [res# (do ~@body)]
       (run! remove-ns @*remove-nsyms*)
       res#)))

(defn tc-common* [frm {{:keys [syn provided?]} :expected-syntax :keys [expected-ret requires ns-meta check-config] :as opt}]
  (check-opt opt)
  (let [file *file*
        nsym (gensym 'clojure.core.typed.test.temp)
        ns-form 
        `(ns ~nsym
           ~@(when ns-meta [ns-meta])
           (:refer-clojure :exclude 
                           ~'[type defprotocol #_letfn fn loop let dotimes
                              #_def #_filter #_remove defn atom ref cast])
           (:require ~@'[;; TODO use typed.clojure
                         [clojure.core.typed :refer :all :as t]
                         [clojure.core.typed.unsafe :as unsafe]
                         [clojure.core :as core]
                         [clojure.core :as cc]]
                     ~@requires))]
    `(do
       (t/load-if-needed)
       (binding [*ns* *ns*
                 *file* ~file]
         (let [expected-ret# (clj ~expected-ret)
               check-config# ~check-config
               {ex# :ex} (t/check-form-info '~ns-form
                                            :check-config check-config#)
               _# (assert (nil? ex#) ex#)
               res# (t/check-form-info 
                      '~frm
                      :expected-ret expected-ret#
                      :expected '~syn
                      :type-provided? ~provided?
                      :check-config check-config#)]
           (or (some-> *remove-nsyms* (swap! conj '~nsym))
               (remove-ns '~nsym))
           res#)))))

(defmacro tc-e 
  "Type check an an expression in namespace that :refer's
  all of clojure.core.typed (aliased to t) and aliases clojure.core
  to core.

  Takes one form and then options, and returns true if the form checks
  with the expected input/output types according to the provided options.
  
  The first form in the options can be a static type syntax scoped
  in the new namespace. This is disambiguated with a call to keyword?
  (literal keywords aren't valid type syntax).
  
  eg. (tc-e (+ 1 1) Num)
      ;=> Num

  Keyword Options:

    :expected-ret An expected ret, evaluated in the current namespace (not the new
                  one that refers c.c.t). Cannot be provided in combination with the implicit
                  first option as a type, as above.
    :ret          Check the return TCResult of this expression against this ret. Evaluated
                  in the current namespace.
    :ns-meta      Map to use as ns metadata.
    :check-config Map specifying check-config."
  [frm & opts]
  (apply common-test/tc-e tc-common* frm opts))

(comment
  (tc-e 1)
  (tc-e 1 Num)
  (tc-e 1 Num :ret )
  (tc-err (fn [] (inc 'a)))
  (tc-err (fn [] (inc 1)))
  (tc-err (fn [] (inc 1)) Num)
  )

(defmacro tc-err [frm & opts]
  (apply common-test/tc-err tc-common* frm opts))

(defmacro is-tc-e [& body]
  `(test/is (do (tc-e ~@body)
                true)))

(defmacro is-tc-err
  "Returns the result of the tc-err call"
  [& body]
  (let [frm `(tc-err ~@body)]
    `(let [res# ~frm]
       (test/is res# (str '~frm))
       res#)))

(defn extract-error-messages [tc-err-res]
  (let [ungensym-ns (fn [msg] (str/replace msg #"clojure\.core\.typed\.test\.temp\d+/" "clojure.core.typed.temp/"))
        ;;TODO reparse line numbers in relation to is-tc-err-messages
        remove-line-number (fn [msg line] (str/replace msg (str ":" line ":") ":REMOVED_LINE:"))
        cp-relative-file (fn [msg line] (str/replace msg (str ":" line ":") ":REMOVED_LINE:"))
        mask-line-number (fn [m]
                           (let [line (-> m :env :line)
                                 file (-> m :env :file)
                                 file-name (if (= file "NO_SOURCE_FILE")
                                             file
                                             (some-> file #?(:clj fs/file-name :default str)))]
                             (cond-> m
                               (contains? m :env) (-> (update-in [:env :line] #(when (integer? %) "REMOVED_LINE"))
                                                      (assoc-in [:env :file] file-name))
                               (:message m) (-> (update :message remove-line-number line)
                                                (update :message ungensym-ns)
                                                (cond-> file (update :message str/replace file file-name))))))]
    (-> tc-err-res
        (update :type-errors #(mapv mask-line-number %))
        (cond->
          (:ex tc-err-res) (update :ex (fn [ex]
                                         (let [d (ex-data ex)
                                               _ (assert (not (contains? d :message)) d)]
                                           (mask-line-number
                                             (assoc d :message (ex-message ex))))))))))

(defmacro is-tc-err-messages
  "Performs an is-tc-err and returns error messages"
  [& body]
  `(extract-error-messages
     (is-tc-err ~@body)))

(defmacro throws-tc-error? [& body]
  `(err/with-ex-info-handlers
     [err/tc-error? (constantly true)]
     ~@body
     false))

(defmacro sub? [s t]
  `(let [opts# (assoc (clj-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns '~'clojure.core.typed)]
     (subtype? (parse-type '~s opts#)
               (parse-type '~t opts#))))

(defmacro sub?-q [s t]
  `(let [opts# (assoc (clj-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns '~'clojure.core.typed)]
     (subtype? (parse-type ~s opts#)
               (parse-type ~t opts#))))

(defn clj-opts []
  (-> (->opts)
      (assoc :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))
      (prs/with-unparse-ns (ns-name *ns*))))

(defn subtype? [s t]
  (sub/subtype? s t (clj-opts)))

(defn both-subtype? [s t]
  (and (not= r/-error s)
       (not= r/-error t)
       (subtype? s t)
       (subtype? t s)))

(defmacro both-sub? [s t]
  `(both-subtype? (parse-clj '~s)
                  (parse-clj '~t)))

(defn check [& as]
  (apply chk/check-expr as))

(defmacro is-cf [& args]
  `(is (do
         (t/cf ~@args)
         true)))

(defmacro is-clj [& args]
  `(clj (is ~@args)))

(defmacro cljs [& body]
  `(do ~@body))

(defmacro clj [& body]
  `(do ~@body))

;return ret for an expression f
(defmacro eret [f]
  `(let [ret# (-> ~(tc-common* f {}) :ret)]
     (assert (r/TCResult? ret#))
     ret#))

;return type for an expression f
(defmacro ety [f]
  `(-> (eret ~f) r/ret-t))

(defmacro caught-top-level-errors [nfn & body]
  `(err/with-ex-info-handlers
     [err/top-level-error? (fn [data# _#]
                             (~nfn (count (:errors data#))))]
     ~@body
     false))

(defmacro equal-types-noparse [l r]
  `(clj (is (let [l# (ety ~l)
                  r# ~r]
              (or (both-subtype? l# r#)
                  (do (println "Actual" l#)
                      (println "Expected" r#)
                      (println "In" (quote ~l))
                      nil))))))

(defmacro equal-types [l r]
  `(equal-types-noparse ~l (parse-type (quote ~r) (assoc (clj-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns '~'clojure.core.typed))))

(defmacro tc-t [form]
  `(let [{ex# :ex ret# :ret}
         ~(tc-common* form {})]
     (if ex#
       (throw ex#)
       ret#)))

(defmacro tc [form]
  `(t/check-form* '~form))

;; from clojure.test-helper
(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
   (as identified by messages to *err*)."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer ~form))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer ~form))))))

;Aliases used in unit tests
(defmacro is-with-aliases [tst]
  `(do (t/check-ns '~'clojure.core.typed.test.util-aliases)
       (is-clj ~tst)))
