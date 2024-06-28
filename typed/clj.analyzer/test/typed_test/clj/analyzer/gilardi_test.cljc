(ns typed-test.clj.analyzer.gilardi-test
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljc.analyzer.env :as env]
            [typed.clj.analyzer :as jana2]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [clojure.test :refer :all]))

;; An example type system written with typed.clj.analyzer.
;; Able to type check intermediate steps in macroexpansion,
;; and manully preserve evaluation order of top-level forms
;; in such cases.

(declare check-expr)

;; =================
;; example Type system stuff

;; Type syntax:
;; t ::= :any

(def type? keyword?)

(defn subtype?
  "Returns true iff t1 <: t2"
  [t1 t2]
  {:pre [(every? type? [t1 t2])]
   :post [(boolean? %)]}
  (= t1 t2))

(defn verify-expected
  "Checks if actual <: expected"
  [actual expected]
  {:pre [((some-fn nil? type?) expected)
         (type? actual)]
   :post [(type? %)]}
  (if (or (nil? expected)
          (subtype? actual expected))
    (or expected actual)
    (throw (ex-info "Type error" {:expected expected :actual actual}))))

;; =================
;; AST traversal

;; a multimethod that knows how to type check each kind
;; of (fully expanded) AST node.
;; 
;; See typed.clj.checker.check/-check for a real example.
(defmulti -check (fn [expr expected opts] (::ana2/op expr)))

(defn type-for-val [v] :any)

;; type checking constants (eg., symbols, keywords, numbers)
(defmethod -check ::ana2/const [expr expected opts]
  (-> expr
      (assoc ::type (-> (type-for-val (:val expr))
                        (verify-expected expected)))))

;; type check function calls
(defmethod -check ::ana2/invoke [expr expected opts]
  (let [check-fcall (fn [cexpr ftype argtypes]
                      ;; subtyping checks or inference goes here
                      (let [inferred :any]
                        (-> cexpr
                            (assoc ::type (verify-expected inferred expected)))))
        cexpr (-> expr
                  (update :fn check-expr nil opts)
                  (update :args #(mapv (fn [e] (check-expr e nil opts)) %)))]
    (check-fcall cexpr
                 (-> cexpr :fn ::type)
                 (->> cexpr :args (mapv ::type)))))

;; in real implementations, this catch-all case should be type error
(defmethod -check :default [expr expected opts]
  (-> expr
      ;; blindly expand and type check everything.
      ;; just here for demonstration purposes, should handle
      ;; individually each AST op using -check.
      (ast/update-children #(check-expr % nil opts))
      (assoc ::type (verify-expected :any expected))))

(def ^:dynamic *intermediate-forms* nil)
(def ^:dynamic *found-defns* nil)

(defn check-expr
  ([expr expected opts]
   (let [;; important to save ns pre-expansion, as arbitrary effects
         ;; may happen during macroexpansion.
         expr (assoc-in expr [:env :ns] (ns-name *ns*))]
     ;; ::ana2/op maps to qualified keys, prefer over :op (the old tools.analyzer-style)
     (case (::ana2/op expr)
       ;; unanalyzed nodes are specially handled outside of -check.
       ;; this gives us full control over its expansion and evaluation.
       ;; if you don't care about type checking macros pre-expansion,
       ;; you can delete this branch and ana2/run-pre-passes will automatically
       ;; expand them. In both cases, -check does not have an :unanalyzed case.
       ::ana2/unanalyzed
       (let [{:keys [form env]} expr
             ;_ (prn "found form" form)
             ;_ (prn "*ns*" (ns-name *ns*))
             _ (when *intermediate-forms*
                 (swap! *intermediate-forms* conj form))]
         (case (jana2/resolve-op-sym form env opts)
           ;; example of type checking a macro that relies on top-level 'do'
           ;; evaluation order, so the results of evaluating previous expressions
           ;; can be used to macroexpand subsequent ones.
           typed.clj.analyzer.gilardi-test/my-body
           (let [arg-forms (vec (rest form))
                 tl? (ana2/top-level? expr)
                 ; Note: if top-level, must check args in evaluation order
                 cargs (into []
                             (map-indexed
                               (fn [i form]
                                 (let [unanalyzed (if tl? ana2/unanalyzed-top-level ana2/unanalyzed)
                                       expected (when (= (inc i) (count arg-forms))
                                                  expected)]
                                   (-> form
                                       (unanalyzed env opts)
                                       (check-expr expected opts)))))
                             arg-forms)
                 cexpr (-> expr
                           (assoc :form (list* (first form) (map #(emit-form/emit-form % opts) cargs))))
                 ; returns nil on no args
                 final-result (get (peek cargs) :result nil)]
             (-> cexpr
                 ;; we've taken evaluation into our own hands, this says
                 ;; don't reevaluate implicitly via ana2/eval-top-level
                 ana2/unmark-top-level
                 (assoc :result final-result
                        ::type (if (seq cargs)
                                 (::type (peek cargs))
                                 (verify-expected :any expected)))))
           ;; we can interrupt macroexpansion for whatever reason and resume it
           ;; by recurring with ana2/analyze-outer.
           clojure.core/defn (do (some-> *found-defns*
                                         (swap! update (second form) (fnil inc 0)))
                                 (recur (ana2/analyze-outer expr opts) expected opts))
           ;; completely expand this without type checking it via ana2/run-passes.
           clojure.core/ns
           (let [;_ (prn "old ns:" *ns*)
                 expr (ana2/run-passes expr opts)]
             ;(prn "new ns:" *ns*)
             (-> expr
                 (assoc ::type :any)))
           #_:else
           (recur (ana2/analyze-outer expr opts) expected opts)))
       (-> expr
           (ana2/run-pre-passes opts)
           (-check expected opts)
           (ana2/run-post-passes opts)
           (ana2/eval-top-level opts))))))

(defn check-top-level
  ([form expected] (check-top-level form expected (jana2/default-opts)))
  ([form expected opts]
   {:post [(-> % ::type type?)]}
   (let [env (jana2/empty-env (ns-name *ns*))]
     (with-bindings (jana2/default-thread-bindings env)
       (let [opts (env/ensure opts (jana2/global-env))]
         (-> form
             (ana2/unanalyzed-top-level env opts)
             (check-expr expected opts)))))))

(defn check-top-level-fresh-ns [& args]
  (binding [*ns* (create-ns (gensym 'test-ns))]
    (refer-clojure)
    (apply check-top-level args)))

(defn eval-in-fresh-ns [& args]
  (binding [*ns* (create-ns (gensym 'eval-ns))]
    (refer-clojure)
    (apply eval args)))

(defn chk [& args]
  (apply check-top-level-fresh-ns args))

(def this-ns (ns-name *ns*))

;; example macros for typing rules

(defmacro my-body [& body]
  `(do ~@body))

(defmacro change-to-clojure-repl-on-mexpand []
  (require 'clojure.repl)
  (in-ns 'clojure.repl)
  nil)

(defn change-to-clojure-repl-on-eval []
  (require 'clojure.repl)
  (in-ns 'clojure.repl)
  nil)

(def ^:dynamic *expand-counter* nil)
(def ^:dynamic *call-counter* nil)

(defn can-only-call-once []
  (assert *call-counter* "Must bind *call-counter* to (atom 0)")
  (assert (= 0 @*call-counter*)
          "Called can-only-call-once twice!")
  (reset! *call-counter* 1))

(defmacro can-only-expand-once []
  (assert *expand-counter* "Must bind *expand-counter* to (atom 0)")
  (assert (= 0 @*expand-counter*)
          "Expanded can-only-expand-once twice!")
  (reset! *expand-counter* 1))

(deftest gilardi-test
  (is (= 1 (:result (chk 1 nil))))
  (is (= 2 (:result
             (chk `(do (ns ~(gensym 'foo))
                       (require '~'[clojure.core :as core])
                       (assert (#?(:cljr .StartsWith :default .startsWith) (str (ns-name *ns*)) "foo")
                               *ns*)
                       ;(prn (ns-aliases *ns*))
                       ;(println "foo ADSF")
                       ;(prn (ns-name *ns*) (ns-aliases *ns*))
                       (~'core/inc 1))
                  nil))))
  (is (= 2 (:result
             (chk `(do (ns ~(gensym 'foo)
                         ~'(:require [clojure.core :as core]))
                       ;(println (ns-name *ns*) "foo ADSF")
                       (~'core/inc 1))
                  nil))))
  (is (= 'hello
         (:result
           (chk '(do (defmacro blah []
                       `'~'hello)
                     (blah))
                nil))))
  (binding [*intermediate-forms* (atom #{})]
    (is (= '42
           (:result
             (chk '(do (defmacro stage1 []
                         '(stage2))
                       (defmacro stage2 []
                         42)
                       (stage1))
                  nil))))
    (is (contains? @*intermediate-forms* '(stage1)))
    (is (contains? @*intermediate-forms* '(stage2))))
  (binding [*found-defns* (atom {})]
    (is (= '12
           (:result
             (chk '(do (defn ttest []
                         12)
                       (ttest))
                  nil))))
    (is (= {'ttest 1} @*found-defns*)))
  (is (= 1 (:result (chk `(my-body nil 1) nil))))
  (is (= {:result nil} (select-keys (chk `(my-body nil) nil) [:result])))
  (is (= {:result nil} (select-keys (chk `(my-body) nil) [:result])))
  (binding [*call-counter* (atom 0)]
    (is (= 1 (eval-in-fresh-ns `(do (can-only-call-once) 1)))))
  (binding [*expand-counter* (atom 0)]
    (is (= 1 (eval-in-fresh-ns `(do (can-only-expand-once) 1)))))
  (binding [*call-counter* (atom 0)]
    (is (= {:result 1} (select-keys (chk `(my-body (can-only-call-once)) nil) [:result]))))
  (binding [*expand-counter* (atom 0)]
    (is (= {:result 1} (select-keys (chk `(my-body (can-only-expand-once)) nil) [:result]))))

  ; *ns* side effects
  ; - on mexpand
  (is (string? (:result
                 (chk `(do (change-to-clojure-repl-on-mexpand)
                           (~'demunge "a"))
                      nil))))
  (is (string? (:result
                 (chk `(my-body (change-to-clojure-repl-on-mexpand)
                                (~'demunge "a"))
                      nil))))
  (is (= "a" (:result
               (chk `(let* []
                       (my-body (change-to-clojure-repl-on-mexpand)
                                (~'demunge "a")))
                    nil))))
  (is (= "a" (:result
               (chk `(let* []
                       (do (change-to-clojure-repl-on-mexpand)
                           (~'demunge "a")))
                    nil))))
  (is (= "a" (:result
               (chk `(let* [_# (change-to-clojure-repl-on-mexpand)]
                       (~'demunge "a"))
                    nil))))
  (is (= "a" (:result
               (chk `(second [(change-to-clojure-repl-on-mexpand) (~'demunge "a")])
                    nil))))
  (is (fn? (:result
             (chk `(fn* []
                        (my-body (change-to-clojure-repl-on-mexpand)
                                 (~'demunge "a")))
                  nil))))
  (is (= "a" (:result
               (chk `(do (fn* [] (change-to-clojure-repl-on-mexpand))
                         (~'demunge "a"))
                    nil))))
  (is (= "a" (eval-in-fresh-ns `(let* []
                                  (do (change-to-clojure-repl-on-mexpand)
                                      (~'demunge "a"))))))
  (is (fn? (eval-in-fresh-ns `(fn* []
                                   (do (change-to-clojure-repl-on-mexpand)
                                       (~'demunge "a"))))))
  (is (= "a" (eval-in-fresh-ns `(let* []
                                  (my-body (change-to-clojure-repl-on-mexpand)
                                           (~'demunge "a"))))))
  (is (string? (:result
                 (chk `(do (let* []
                             (change-to-clojure-repl-on-mexpand))
                           (~'demunge "a"))
                      nil))))
  ; - on eval
  (is (string? (:result
                 (chk `(do (change-to-clojure-repl-on-eval)
                           (~'demunge "a"))
                      nil))))
  (is (string? (:result
                 (chk `(my-body (change-to-clojure-repl-on-eval)
                                (~'demunge "a"))
                      nil))))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Could not resolve var: demunge"
        (chk `(let* []
                (my-body (change-to-clojure-repl-on-eval)
                         (~'demunge "a")))
             nil)))
  (is (thrown-with-msg?
        #?(:cljr Exception :default RuntimeException)
        #"" ;#"Unable to resolve symbol: demunge in this context"
        (eval-in-fresh-ns `(let* []
                             (my-body (change-to-clojure-repl-on-eval)
                                      (~'demunge "a"))))))
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Could not resolve var: demunge"
        (chk `(let* []
                (do (change-to-clojure-repl-on-eval)
                    (~'demunge "a")))
             nil)))
  (is (thrown-with-msg?
        #?(:cljr Exception :default RuntimeException)
        #"" ;#"Unable to resolve symbol: demunge in this context"
        (eval-in-fresh-ns `(let* []
                             (do (change-to-clojure-repl-on-eval)
                                 (~'demunge "a"))))))
  (is (string? (:result
                 (chk `(do (let* []
                             (change-to-clojure-repl-on-eval))
                           (~'demunge "a"))
                      nil))))

  ; var is interned under let*
  (is (= 1 (:result (chk '(let* [] (def a 1) a) nil))))
  ; locals shadowing (eval in current namespace)
  (is (binding [*ns* (the-ns this-ns)]
        (= {:result 1} (select-keys (check-top-level `(let [~'my-body (constantly 1)] (~'my-body)) nil) [:result]))))
  (is (binding [*ns* (the-ns this-ns)]
        (= {:result nil} (select-keys (check-top-level `(let [my-body# (constantly 1)] (~'my-body)) nil) [:result]))))
  )
