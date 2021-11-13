(ns typed-test.cljs.analyzer.incremental-test
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.ast :as ast]
            [typed.cljs.analyzer :as jsana2]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [clojure.test :refer :all]
            [cljs.analyzer :as cljs-ana]
            [cljs.analyzer.api :as ana-api]
            [cljs.env :as env]
            [cljs.compiler.api :as comp-api]))

(defonce STATE (env/default-compiler-env))

(comment
  (-> @STATE keys)
  (-> @STATE ::cljs-ana/namespaces (get 'foo.bar))
  )

;; An example type system written with typed.cljs.analyzer.
;; Able to type check intermediate steps in macroexpansion.

(defn check-expr
  ([expr] (check-expr expr nil))
  ([expr expected]
   (prn (ana-api/current-ns))
   (case (:op expr)
     :unanalyzed (let [op (doto (jsana2/resolve-op-sym (:form expr) (:env expr))
                            prn)]
                   (case op
                     cljs.core/inc (check-expr
                                     (update expr
                                             :form #(with-meta (apply list (list 'var op) (rest %))
                                                               (meta %)))
                                     expected)
                     (check-expr (jsana2/analyze-outer expr) expected)))
     :do (let [cexpr (-> expr
                         (update :statements #(mapv check-expr %))
                         (update :ret check-expr expected))]
           (assoc cexpr :expr-type (-> cexpr :ret :expr-type)))
     :invoke (case (-> expr :fn :op)
               :the-var (let [op (-> expr :fn :var :name)]
                          (case op
                            cljs.core/inc (do (prn "checking inc....")
                                              expr)
                            (assert nil (str ":invoke " op)))))
     :ns (assoc expr :expr-type :any))))

(defn check-top-level
  ([form] (check-top-level form nil))
  ([form expected] (check-top-level form expected {}))
  ([form expected {:keys [env] :as opts}]
   (let [env (or env (ana-api/empty-env))]
     (with-bindings (jsana2/default-thread-bindings env)
       (env/with-compiler-env STATE
         (-> form
             (jsana2/unanalyzed env)
             (check-expr expected)))))))

(defmacro check-top-levels [& forms]
  `(binding [cljs-ana/*cljs-ns* '~'cljs.user]
     (env/with-compiler-env STATE
       (comp-api/with-core-cljs)
       (mapv check-top-level '~forms)
       :ok)))

(comment
  (check-top-levels
    (ns foo.bar)
    (inc 1)
    (do (inc 1)))
  )
