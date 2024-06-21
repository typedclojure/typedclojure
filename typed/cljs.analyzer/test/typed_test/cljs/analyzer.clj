(ns typed-test.cljs.analyzer
  (:require [clojure.test :refer [is deftest testing]]
            [typed.cljc.analyzer :as ana]
            [typed.cljs.analyzer :as jsana2]
            [cljs.analyzer.api :as ana-api]
            [cljs.env :as env]
            [cljs.analyzer :as cljs-ana]
            [cljs.compiler.api :as comp-api]))

(defonce STATE (env/default-compiler-env))

(comment
  (-> @STATE keys)
  (-> @STATE ::cljs-ana/namespaces keys)
  )

(defmacro wrap [& body]
  {:pre [(seq body)]}
  `(binding [cljs-ana/*cljs-ns* '~'cljs.user]
     (with-bindings (jsana2/default-thread-bindings)
       (let [opts# (jsana2/default-opts)]
         (env/with-compiler-env STATE
           (comp-api/with-core-cljs)
           (jsana2/analyze-outer
             (jsana2/unanalyzed
               '~(list 'ns (identity #_gensym 'foo.bar))
               (ana-api/empty-env)
               opts#)
             opts#)
           ~@body)))))

(defn analyze1 [form]
  (with-bindings (jsana2/default-thread-bindings)
    (let [opts (jsana2/default-opts)]
      (-> form
          (ana/unanalyzed (ana-api/empty-env) opts)
          (ana/analyze-outer-root opts)))))

(defn analyze-outer-root
  "cljs helper"
  [ast]
  (with-bindings (jsana2/default-thread-bindings)
    (ana/analyze-outer-root ast (jsana2/default-opts))))

(defn trim-unanalyzed [ast]
  (select-keys ast [:op :form :body? ::ana/op]))

(defn trim-do-body [ast]
  (-> ast
      (select-keys [:op ::ana/op :form :statements :ret :body?])
      (update :statements #(mapv trim-unanalyzed %))
      (update :ret trim-unanalyzed)))

(defn unanalyzed [form env]
  (with-bindings (jsana2/default-thread-bindings)
    (env/with-compiler-env STATE
      (jsana2/unanalyzed form env (jsana2/default-opts)))))

(defn analyze-outer [ast]
  (with-bindings (jsana2/default-thread-bindings)
    (env/with-compiler-env STATE
      (jsana2/analyze-outer ast (jsana2/default-opts)))))

(deftest unanalyzed-test
  (is (= {:op :unanalyzed
          ::ana/op ::ana/unanalyzed
          :form 1}
         (trim-unanalyzed (unanalyzed 1 (ana-api/empty-env)))))
  (is (= {:op :unanalyzed
          ::ana/op ::ana/unanalyzed
          :form '(do)}
         (trim-unanalyzed (unanalyzed '(do) (ana-api/empty-env))))))

(deftest analyze-outer-test
  (is (= {:op :unanalyzed
          ::ana/op ::ana/unanalyzed
          :form '(let* [])}
         (trim-unanalyzed
           (analyze-outer
             (unanalyzed `(let []) (ana-api/empty-env))))))
  (is (= {:op :let
          ::ana/op ::ana/let
          :form '(let* [])}
         (select-keys
           (analyze-outer
             (analyze-outer
               (unanalyzed `(let []) (ana-api/empty-env))))
           [:op :form])))
  (is (= {:op :unanalyzed
          ::ana/op ::ana/unanalyzed
          :form '(js* "(~{} + ~{})" 1 2)}
         (trim-unanalyzed
           (analyze-outer
             (unanalyzed `(+ 1 2) (ana-api/empty-env)))))))

;;TODO preanalyze :body? true nodes
(deftest analyze-outer-test
  (is (= :const
         (:op (wrap (analyze1 1)))))
  (testing :do
    (let [expr (wrap (analyze1 '(do 42 1)))]
      (is (= {:op :do
              ::ana/op ::ana/do
              :form '(do 42 1)}
             (select-keys expr [:op ::ana/op :form])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 42}]
             (mapv trim-unanalyzed (:statements expr))))
      (is (= {:op :unanalyzed
               ::ana/op ::ana/unanalyzed
              :form 1}
             (-> expr :ret trim-unanalyzed))))
    (let [expr (wrap (analyze1 '(do)))]
      (is (= :do (:op expr)))
      (is (= [] (:statements expr)))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form nil}
             (-> expr :ret trim-unanalyzed)))))
  ;; FIXME fully analyze bindings before putting in env
  (testing :let
    (let [expr (wrap (analyze1 '(let [a 1] (inc 42) (+ a))))]
      (is (= :let (:op expr)))
      (is (= [{:op :binding
               ::ana/op ::ana/binding
               :name 'a
               :init {:op :unanalyzed
                      ::ana/op ::ana/unanalyzed
                      :form 1}}]
             (mapv #(-> %
                        (select-keys [:op ::ana/op :name :init])
                        (update :init trim-unanalyzed))
                   (:bindings expr))))
      (is (= {:op :do
              ::ana/op ::ana/do
              :form '(do (inc 42) (+ a))
              :statements [{:op :unanalyzed
                            ::ana/op ::ana/unanalyzed
                            :form '(inc 42)}]
              :ret {:op :unanalyzed
                    ::ana/op ::ana/unanalyzed
                    :form '(+ a)}
              :body? true}
             (-> expr :body trim-do-body)))))
  (testing :loop
    (let [expr (wrap (analyze1 '(loop [a 1] (inc 42) (+ a))))]
      (is (= {:op :loop
              ::ana/op ::ana/loop
              :form '(loop* [a 1] (inc 42) (+ a))}
             (select-keys expr [:op ::ana/op :form])))
      (is (= [{:op :binding
               ::ana/op ::ana/binding
               :name 'a
               :init {:op :unanalyzed
                      ::ana/op ::ana/unanalyzed
                      :form 1}}]
             (mapv #(-> %
                        (select-keys [:op ::ana/op :name :init])
                        (update :init trim-unanalyzed))
                   (:bindings expr))))
      (is (= {:op :do
              ::ana/op ::ana/do
              :form '(do (inc 42) (+ a))
              :body? true
              :statements [{:op :unanalyzed
                            ::ana/op ::ana/unanalyzed
                            :form '(inc 42)}]
              :ret {:op :unanalyzed
                    ::ana/op ::ana/unanalyzed
                    :form '(+ a)}}
             (-> expr :body trim-do-body)))))
  (testing :recur
    (let [expr (wrap (-> (analyze1 '(loop [a 1] (recur (+ a))))
                         :body
                         analyze-outer-root
                         :ret
                         analyze-outer-root))]
      (is (= {:op :recur
              ::ana/op ::ana/recur}
             (select-keys expr [:op ::ana/op])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '(+ a)}]
             (->> expr :exprs (mapv trim-unanalyzed))))))
  (testing :the-var
    (let [expr (wrap (analyze1 '(var +)))]
      (is (= {:op :the-var
              ::ana/op ::ana/the-var
              :form '(var +)}
             (select-keys expr [:op ::ana/op :form])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form '+}
             (-> expr :var trim-unanalyzed)))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form ''cljs.core/+}
             (-> expr :sym trim-unanalyzed)))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form '{:ns 'cljs.core
                      :name '+}}
             (-> expr :meta trim-unanalyzed
                 (update :form select-keys [:ns :name]))))))
  (testing :var
    (let [expr (wrap (analyze1 '+))]
      (is (= {:op :var
              ::ana/op ::ana/var}
             (select-keys expr [:op ::ana/op]))
          (keys expr))
      (is (= '+ (:form expr)))
      (is (= {:ns 'cljs.core
              :name 'cljs.core/+}
             (select-keys (:info expr) [:ns :name])))))
  (testing :local
    (let [expr (wrap
                 (-> (analyze1 '(let [a 1] a))
                     :body
                     analyze-outer-root
                     :ret
                     analyze-outer-root))]
      (is (= {:op :local
              ::ana/op ::ana/local}
             (select-keys expr [:op ::ana/op])))
      ;FIXME probably should be analyzed
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form '1} (-> expr :init trim-unanalyzed)))))
  (testing :if
    (let [expr (wrap
                 (analyze1 '(if 1 2 3)))]
      (is (= {:op :if
              ::ana/op ::ana/if}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 1}
             (-> expr :test trim-unanalyzed)))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 2} (-> expr :then trim-unanalyzed)))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 3}
             (-> expr :else trim-unanalyzed)))))
  (testing :case
    (let [expr (wrap
                 (-> (analyze1 '(case 1 2 3 4))
                     :body
                     analyze-outer-root
                     :ret
                     analyze-outer-root))]
      (is (= {:op :case
              ::ana/op ::jsana2/case}
             (select-keys expr [:op ::ana/op])))
      (is (= :unanalyzed (-> expr :test :op)))
      ;; (case* sym ...)
      (is (symbol? (-> expr :test :form)))
      (is (= [{:op :case-node
               ::ana/op ::jsana2/case-node
               :tests [{:op :case-test
                        ::ana/op ::jsana2/case-test
                        :test {:op :const
                               ::ana/op ::ana/const
                               :form 2}}]
               :then {:op :case-then
                      ::ana/op ::jsana2/case-then
                      :then {:op :unanalyzed
                             ::ana/op ::ana/unanalyzed
                             :form 3}}}]
             (->> expr :nodes (mapv (fn [case-node]
                                      (-> case-node
                                          (select-keys [:op ::ana/op :tests :then])
                                          (update :tests (fn [case-tests]
                                                           (mapv (fn [case-test]
                                                                   (-> case-test
                                                                       (select-keys [:op ::ana/op :test])
                                                                       (update :test trim-unanalyzed)))
                                                                 case-tests)))
                                          (update :then (fn [case-then]
                                                          (-> case-then
                                                              (select-keys [:op ::ana/op :then])
                                                              (update :then trim-unanalyzed))))))))))))
  (testing :throw
    (let [expr (wrap (analyze1 '(throw 1)))]
      (is (= {:op :throw
              ::ana/op ::ana/throw
              :form '(throw 1)}
             (select-keys expr [:op ::ana/op :form])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 1} (-> expr :exception trim-unanalyzed)))))
  (testing :try
    (let [expr (wrap
                 (-> (analyze1 '(try 1
                                     (catch :default e (inc e))
                                     (finally 3)))
                     (update :catch analyze-outer-root)))]
      (is (= {:op :try
              ::ana/op ::ana/try
              :form '(try 1 (catch :default e (inc e)) (finally 3))}
             (select-keys expr [:op ::ana/op :form])))
      (is (= {:op :do
              ::ana/op ::ana/do
              :statements []
              :ret {:op :unanalyzed
                    ::ana/op ::ana/unanalyzed
                    :form 1}
              :form '(do 1)
              :body? true}
             (-> expr :body trim-do-body)))
      (is (= {:op :do
              ::ana/op ::ana/do
              :body? true
              :statements []
              :ret {:op :unanalyzed
                    ::ana/op ::ana/unanalyzed
                    :form '(inc e)}
              :form '(do (inc e))}
             (-> expr :catch :body trim-do-body)))
      (is (= {:op :do
              ::ana/op ::ana/do
              :statements []
              :ret {:op :unanalyzed
                    ::ana/op ::ana/unanalyzed
                    :form 3}
              :form '(do 3)
              :body? true}
             (-> expr :finally trim-do-body)))))
  (testing :def
    (let [expr (wrap
                 (analyze1 '(def a 1)))]
      (is (= {:op :def
              ::ana/op ::ana/def}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :var
              ::ana/op ::ana/var}
             (-> expr :var (select-keys [:op ::ana/op]))))
      (is (= {:ns 'foo.bar
              :name 'foo.bar/a}
             (-> expr :var :info (select-keys [:ns :name]))))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form '1}
             (-> expr :init trim-unanalyzed)))))
  (testing :fn
    (let [expr (wrap
                 (analyze1 '(fn* [a] 1 (+ a 2))))]
      (is (= {:op :fn
              ::ana/op ::ana/fn
              :form '(fn* [a] 1 (+ a 2))}
             (select-keys expr [:op ::ana/op :form])))
      (is (= [{:op :fn-method
               ::ana/op ::ana/fn-method
               :body {:op :do
                      ::ana/op ::ana/do
                      :form '(do 1 (+ a 2))
                      :body? true
                      :statements [{:form '1
                                    ::ana/op ::ana/unanalyzed
                                    :op :unanalyzed}]
                      :ret {:form '(+ a 2)
                            ::ana/op ::ana/unanalyzed
                            :op :unanalyzed}}}]
             (->> expr :methods
                  (mapv (fn [method]
                          (-> method (select-keys [:op ::ana/op :body])
                              (update :body trim-do-body)))))))))
  (testing :letfn
    (let [expr (wrap
                 (analyze1 '(letfn [(f [a] (inc a))] 1 (f 1))))]
      (is (= {:op :letfn
              ::ana/op ::ana/letfn
              :form '(letfn* [f (clojure.core/fn f [a] (inc a))] 1 (f 1))}
             (select-keys expr [:op ::ana/op :form])))
      (is (= [{:op :binding
               ::ana/op ::ana/binding
               :init {:op :unanalyzed
                      ::ana/op ::ana/unanalyzed
                      :form '(clojure.core/fn f [a] (inc a))}}]
             (->> expr :bindings
                  (mapv (fn [binding]
                          (-> binding (select-keys [:op ::ana/op :init])
                              (update :init trim-unanalyzed)))))))
      (is (= {:op :do
              ::ana/op ::ana/do
              :form '(do 1 (f 1))
              :body? true
              :statements [{:form 1
                            ::ana/op ::ana/unanalyzed
                            :op :unanalyzed}]
              :ret {:form '(f 1)
                    ::ana/op ::ana/unanalyzed
                    :op :unanalyzed}}
             (-> expr :body trim-do-body)))))
  (testing :quote
    (let [expr (wrap (analyze1 ''1))]
      (is (= {:op :quote
              ::ana/op ::ana/quote}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :const
              ::ana/op ::ana/const}
             (-> expr :expr (select-keys [:op ::ana/op]))))
      (is (= 1 (-> expr :expr :val)))))
  (testing :new
    (let [expr (wrap (analyze1 '(Foo. 1)))]
      (is (= {:op :new
              ::ana/op ::ana/new}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 'Foo}
             (-> expr :class (select-keys [:op ::ana/op :form]))))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 1}]
             (->> expr :args (mapv trim-unanalyzed))))))
  (testing :set!
    (let [expr (wrap
                 (analyze1 '(set! + 1)))]
      (is (= {:op :set!
              ::ana/op ::ana/set!}
             (select-keys expr [:op ::ana/op])))
      ;;FIXME safe to expand
      (is (= {:op :var
              :name 'cljs.core/+}
             (-> expr :target (select-keys [:op :name]))))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 1}
             (-> expr :val trim-unanalyzed)))))
  (testing :host-field
    (let [expr (wrap
                 (analyze1 '(.-foo 1)))]
      (is (= {:op :host-field
              ::ana/op ::jsana2/host-field}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 1}
             (-> expr :target trim-unanalyzed)))))
  (testing :host-call
    (let [expr (wrap
                 (analyze1 '(.foo 1 2)))]
      (is (= {:op :host-call
              ::ana/op ::jsana2/host-call}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 1}
             (-> expr :target trim-unanalyzed)))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 2}]
             (->> expr :args (mapv trim-unanalyzed))))))
  (testing :deftype
    (let [expr (wrap
                 (-> (analyze1 '(deftype A [a]
                                  Object
                                  (toString [this] (str a))))
                     :statements
                     first
                     analyze-outer-root
                     (update :body analyze-outer-root)))]
      (is (= {:op :deftype
              ::ana/op ::jsana2/deftype}
             (select-keys expr [:op ::ana/op])))))
  (testing :defrecord
    (let [expr (wrap
                 (-> (analyze1 '(defrecord A [a]
                                  Object
                                  (toString [this] (str a))))
                     :body
                     analyze-outer-root
                     :statements
                     first
                     analyze-outer-root
                     :ret
                     analyze-outer-root))]
      (is (= {:op :defrecord
              ::ana/op ::jsana2/defrecord}
             (select-keys expr [:op ::ana/op])))))
  (testing :invoke
    (let [expr (wrap
                 (-> (analyze1 '(a 1))))]
      (is (= {:op :invoke
              ::ana/op ::ana/invoke}
             (select-keys expr [:op ::ana/op])))
      (is (= {:op :unanalyzed
              ::ana/op ::ana/unanalyzed
              :form 'a}
             (-> expr :fn trim-unanalyzed)))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '1}]
             (->> expr :args (mapv trim-unanalyzed))))))
  (testing :js
    (let [expr (wrap
                 (analyze1 '(js* "1")))]
      (is (= {:op :js
              ::ana/op ::jsana2/js}
             (select-keys expr [:op ::ana/op]))))
    (let [expr (wrap
                 (analyze1 '(js* "1" 2 3)))]
      (is (= {:op :js
              ::ana/op ::jsana2/js}
             (select-keys expr [:op ::ana/op])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '2}
              {:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '3}]
             (->> expr :args (mapv trim-unanalyzed))))))
  (testing :map
    (let [expr (wrap
                 (analyze1 '{1 2}))]
      (is (= {:op :map
              ::ana/op ::ana/map}
             (select-keys expr [:op ::ana/op])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '1}]
             (->> expr :keys (mapv trim-unanalyzed))))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '2}]
             (->> expr :vals (mapv trim-unanalyzed))))))
  (testing :vector
    (let [expr (wrap
                 (analyze1 '[1 a]))]
      (is (= {:op :vector
              ::ana/op ::ana/vector}
             (select-keys expr [:op ::ana/op])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '1}
              {:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 'a}]
             (->> expr :items (mapv trim-unanalyzed))))))
  (testing :set
    (let [expr (wrap
                 (analyze1 '#{a}))]
      (is (= {:op :set
              ::ana/op ::ana/set}
             (select-keys expr [:op ::ana/op])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 'a}]
             (->> expr :items (mapv trim-unanalyzed))))))
  (testing :js-object
    (let [expr (wrap
                 (analyze1 (cljs.tagged_literals.JSValue. {1 2})))]
      (is (= {:op :js-object
              ::ana/op ::jsana2/js-object}
             (select-keys expr [:op ::ana/op])))
      (is (= [1]
             (:keys expr)))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '2}]
             (->> expr :vals (mapv trim-unanalyzed))))))
  (testing :js-array
    (let [expr (wrap
                 (analyze1 (cljs.tagged_literals.JSValue. [1 2])))]
      (is (= {:op :js-array
              ::ana/op ::jsana2/js-array}
             (select-keys expr [:op ::ana/op])))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '1}
              {:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form '2}]
             (->> expr :items (mapv trim-unanalyzed))))))
  (testing :with-meta
    (let [expr (wrap
                 (analyze1 '^{a b} #{a}))]
      (is (= {:op :with-meta
              ::ana/op ::ana/with-meta}
             (select-keys expr [:op ::ana/op])))
      (is (= :set
             (-> expr :expr :op)))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 'a}]
             (->> expr :expr :items (mapv trim-unanalyzed))))
      (is (= :map
             (-> expr :meta :op)))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 'a}]
             (->> expr :meta :keys (mapv trim-unanalyzed))))
      (is (= [{:op :unanalyzed
               ::ana/op ::ana/unanalyzed
               :form 'b}]
             (->> expr :meta :vals (mapv trim-unanalyzed))))))
  (testing :js-var
    (let [expr (wrap
                 (analyze1 'js/document))]
      (is (= {:op :js-var
              ::ana/op ::jsana2/js-var}
             (select-keys expr [:op ::ana/op])))
      (is (= 'js/document (:name expr)))))
  (testing :ns*
    (let [expr (wrap
                 (analyze1 '(ns* (:require '[cljs.core :as asdf]))))]
      (is (= {:op :ns*
              ::ana/op ::jsana2/ns*}
             (select-keys expr [:op ::ana/op])))
      (is (= '{asdf cljs.core, cljs.core cljs.core} (:requires expr)))))
  (testing :ns
    (let [expr (wrap
                 (analyze1 '(ns bar.foo)))]
      (is (= {:op :ns
              ::ana/op ::jsana2/ns}
             (select-keys expr [:op ::ana/op])))
      (is (= 'bar.foo (:name expr))))))
