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
       (env/with-compiler-env STATE
         (comp-api/with-core-cljs)
         (jsana2/analyze-outer
           (jsana2/unanalyzed
             '~(list 'ns (identity #_gensym 'foo.bar))
             (ana-api/empty-env)))
         ~@body))))

(defn analyze1 [form]
  (with-bindings (jsana2/default-thread-bindings)
    (-> (ana/unanalyzed
          form
          (ana-api/empty-env))
        ana/analyze-outer-root)))

(defn analyze-outer-root
  "cljs helper"
  [ast]
  (with-bindings (jsana2/default-thread-bindings)
    (ana/analyze-outer-root ast)))

(defn trim-unanalyzed [ast]
  (select-keys ast [:op :form :body?]))

(defn trim-do-body [ast]
  (-> ast
      (select-keys [:op :form :statements :ret :body?])
      (update :statements #(mapv trim-unanalyzed %))
      (update :ret trim-unanalyzed)))

(defn unanalyzed [form env]
  (with-bindings (jsana2/default-thread-bindings)
    (env/with-compiler-env STATE
      (jsana2/unanalyzed form env))))

(defn analyze-outer [ast]
  (with-bindings (jsana2/default-thread-bindings)
    (env/with-compiler-env STATE
      (jsana2/analyze-outer ast))))

(deftest unanalyzed-test
  (is (= {:op :unanalyzed
          :form 1}
         (trim-unanalyzed (unanalyzed 1 (ana-api/empty-env)))))
  (is (= {:op :unanalyzed
          :form '(do)}
         (trim-unanalyzed (unanalyzed '(do) (ana-api/empty-env))))))

(deftest analyze-outer-test
  (is (= {:op :unanalyzed
          :form '(let* [])}
         (trim-unanalyzed
           (analyze-outer
             (unanalyzed `(let []) (ana-api/empty-env))))))
  (is (= {:op :let
          :form '(let* [])}
         (select-keys
           (analyze-outer
             (analyze-outer
               (unanalyzed `(let []) (ana-api/empty-env))))
           [:op :form])))
  (is (= {:op :unanalyzed
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
      (is (= :do (:op expr)))
      (is (= [{:op :unanalyzed :form 42}] (mapv trim-unanalyzed (:statements expr))))
      (is (= {:op :unanalyzed :form 1} (-> expr :ret trim-unanalyzed))))
    (let [expr (wrap (analyze1 '(do)))]
      (is (= :do (:op expr)))
      (is (= [] (:statements expr)))
      (is (= {:op :unanalyzed :form nil} (-> expr :ret trim-unanalyzed)))))
  ;; FIXME fully analyze bindings before putting in env
  (testing :let
    (let [expr (wrap (analyze1 '(let [a 1] (inc 42) (+ a))))]
      (is (= :let (:op expr)))
      (is (= [{:op :binding
               :name 'a
               :init {:op :unanalyzed
                      :form 1}}]
             (mapv #(-> %
                        (select-keys [:op :name :init])
                        (update :init trim-unanalyzed))
                   (:bindings expr))))
      (is (= {:op :do
              :form '(do (inc 42) (+ a))
              :statements [{:op :unanalyzed
                            :form '(inc 42)}]
              :ret {:op :unanalyzed
                    :form '(+ a)}
              :body? true}
             (-> expr :body trim-do-body)))))
  (testing :loop
    (let [expr (wrap (analyze1 '(loop [a 1] (inc 42) (+ a))))]
      (is (= :loop (:op expr)))
      (is (= [{:op :binding
               :name 'a
               :init {:op :unanalyzed
                      :form 1}}]
             (mapv #(-> %
                        (select-keys [:op :name :init])
                        (update :init trim-unanalyzed))
                   (:bindings expr))))
      (is (= {:op :do
              :form '(do (inc 42) (+ a))
              :body? true
              :statements [{:op :unanalyzed
                            :form '(inc 42)}]
              :ret {:op :unanalyzed
                    :form '(+ a)}}
             (-> expr :body trim-do-body)))))
  (testing :recur
    (let [expr (wrap (-> (analyze1 '(loop [a 1] (recur (+ a))))
                         :body
                         analyze-outer-root
                         :ret
                         analyze-outer-root))]
      (is (= :recur (:op expr)))
      (is (= [{:op :unanalyzed :form '(+ a)}] (->> expr :exprs (mapv trim-unanalyzed))))))
  (testing :the-var
    (let [expr (wrap (analyze1 '(var +)))]
      (is (= :the-var (:op expr)))
      (is (= {:op :unanalyzed :form '+}
             (-> expr :var trim-unanalyzed)))
      (is (= {:op :unanalyzed :form ''cljs.core/+}
             (-> expr :sym trim-unanalyzed)))
      (is (= {:op :unanalyzed :form '{:ns 'cljs.core
                                      :name '+}}
             (-> expr :meta trim-unanalyzed
                 (update :form select-keys [:ns :name]))))))
  (testing :var
    (let [expr (wrap (analyze1 '+))]
      (is (= :var (:op expr)))
      (is (= '+ (:form expr)))
      (is (= {:ns 'cljs.core
              :name 'cljs.core/+}
             (select-keys (:info expr) [:ns :name])))))
  (testing :js-var
    (let [expr (wrap (analyze1 'js/document))]
      (is (= :js-var (:op expr)))
      (is (= 'js/document (:form expr)))))
  (testing :local
    (let [expr (wrap
                 (-> (analyze1 '(let [a 1] a))
                     :body
                     analyze-outer-root
                     :ret
                     analyze-outer-root))]
      (is (= :local (:op expr)))
      ;FIXME probably should be analyzed
      (is (= {:op :unanalyzed
              :form '1} (-> expr :init trim-unanalyzed)))))
  (testing :if
    (let [expr (wrap
                 (analyze1 '(if 1 2 3)))]
      (is (= :if (:op expr)))
      (is (= {:op :unanalyzed :form 1} (-> expr :test trim-unanalyzed)))
      (is (= {:op :unanalyzed :form 2} (-> expr :then trim-unanalyzed)))
      (is (= {:op :unanalyzed :form 3} (-> expr :else trim-unanalyzed)))))
  (testing :case
    (let [expr (wrap
                 (-> (analyze1 '(case 1 2 3 4))
                     :body
                     analyze-outer-root
                     :ret
                     analyze-outer-root))]
      (is (= :case (:op expr)))
      (is (= :unanalyzed (-> expr :test :op)))
      ;; (case* sym ...)
      (is (symbol? (-> expr :test :form)))
      (is (= [{:op :case-node
               :tests [{:op :case-test
                        :test {:op :const
                               :form 2}}]
               :then {:op :case-then
                      :then {:op :unanalyzed
                             :form 3}}}]
             (->> expr :nodes (mapv (fn [case-node]
                                      (-> case-node
                                          (select-keys [:op :tests :then])
                                          (update :tests (fn [case-tests]
                                                           (mapv (fn [case-test]
                                                                   (-> case-test
                                                                       (select-keys [:op :test])
                                                                       (update :test trim-unanalyzed)))
                                                                 case-tests)))
                                          (update :then (fn [case-then]
                                                          (-> case-then
                                                              (select-keys [:op :then])
                                                              (update :then trim-unanalyzed))))))))))))
  (testing :throw
    (let [expr (wrap
                 (analyze1 '(throw 1)))]
      (is (= :throw (:op expr)))
      (is (= {:op :unanalyzed :form 1} (-> expr :exception trim-unanalyzed)))))
  (testing :try
    (let [expr (wrap
                 (-> (analyze1 '(try 1
                                     (catch :default e (inc e))
                                     (finally 3)))
                     (update :catch analyze-outer-root)))]
      (is (= :try (:op expr)))
      (is (= {:op :do
              :statements []
              :ret {:op :unanalyzed
                    :form 1}
              :form '(do 1)
              :body? true}
             (-> expr :body trim-do-body)))
      (is (= {:op :do
              :body? true
              :statements []
              :ret {:op :unanalyzed
                    :form '(inc e)}
              :form '(do (inc e))}
             (-> expr :catch :body trim-do-body)))
      (is (= {:op :do
              :statements []
              :ret {:op :unanalyzed
                    :form 3}
              :form '(do 3)
              :body? true}
             (-> expr :finally trim-do-body)))))
  (testing :def
    (let [expr (wrap
                 (analyze1 '(def a 1)))]
      (is (= :def (:op expr)))
      (is (= :var (-> expr :var :op)))
      (is (= {:ns 'foo.bar
              :name 'foo.bar/a}
             (-> expr :var :info (select-keys [:ns :name]))))
      (is (= {:op :unanalyzed :form '1} (-> expr :init trim-unanalyzed)))))
  (testing :fn
    (let [expr (wrap
                 (analyze1 '(fn* [a] 1 (+ a 2))))]
      (is (= :fn (:op expr)))
      (is (= [{:op :fn-method
               :body {:op :do
                      :form '(do 1 (+ a 2))
                      :body? true
                      :statements [{:form '1, :op :unanalyzed}]
                      :ret {:form '(+ a 2), :op :unanalyzed}}}]
             (->> expr :methods
                  (mapv (fn [method]
                          (-> method (select-keys [:op :body])
                              (update :body trim-do-body)))))))))
  (testing :letfn
    (let [expr (wrap
                 (analyze1 '(letfn [(f [a] (inc a))] 1 (f 1))))]
      (is (= :letfn (:op expr)))
      (is (= [{:op :binding
               :init {:op :unanalyzed
                      :form '(clojure.core/fn f [a] (inc a))}}]
             (->> expr :bindings
                  (mapv (fn [binding]
                          (-> binding (select-keys [:op :init])
                              (update :init trim-unanalyzed)))))))
      (is (= {:op :do
              :form '(do 1 (f 1))
              :body? true
              :statements [{:form 1, :op :unanalyzed}]
              :ret {:form '(f 1), :op :unanalyzed}}
             (-> expr :body trim-do-body)))))
  (testing :quote
    (let [expr (wrap
                 (analyze1 ''1))]
      (is (= :quote (:op expr)))
      (is (= :const (-> expr :expr :op)))
      (is (= 1 (-> expr :expr :val)))))
  (testing :new
    (let [expr (wrap
                 (analyze1 '(Foo. 1)))]
      (is (= :new (:op expr)))
      ;FIXME unsure what we want to do here
      ;;(is (= .. (-> expr :class)))
      (is (= [{:op :unanalyzed :form 1}] (->> expr :args (mapv trim-unanalyzed))))))
  (testing :set!
    (let [expr (wrap
                 (analyze1 '(set! + 1)))]
      (is (= :set! (:op expr)))
      ;;FIXME safe to expand
      (is (= {:op :var
              :name 'cljs.core/+}
             (-> expr :target (select-keys [:op :name]))))
      (is (= {:op :unanalyzed
              :form 1}
             (-> expr :val trim-unanalyzed)))))
  (testing :host-field
    (let [expr (wrap
                 (analyze1 '(.-foo 1)))]
      (is (= :host-field (:op expr)))
      (is (= {:op :unanalyzed
              :form 1}
             (-> expr :target trim-unanalyzed)))))
  (testing :host-call
    (let [expr (wrap
                 (analyze1 '(.foo 1 2)))]
      (is (= :host-call (:op expr)))
      (is (= {:op :unanalyzed
              :form 1}
             (-> expr :target trim-unanalyzed)))
      (is (= [{:op :unanalyzed
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
      (is (= :deftype (:op expr)))))
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
      (is (= :defrecord (:op expr)))))
  (testing :invoke
    (let [expr (wrap
                 (-> (analyze1 '(a 1))))]
      (is (= :invoke (:op expr)))
      (is (= {:op :unanalyzed
              :form 'a}
             (-> expr :fn trim-unanalyzed)))
      (is (= [{:op :unanalyzed
               :form '1}]
             (->> expr :args (mapv trim-unanalyzed))))))
  (testing :js
    (let [expr (wrap
                 (analyze1 '(js* "1")))]
      (is (= :js (:op expr))))
    (let [expr (wrap
                 (analyze1 '(js* "1" 2 3)))]
      (is (= :js (:op expr)))
      (is (= [{:op :unanalyzed
               :form '2}
              {:op :unanalyzed
               :form '3}]
             (->> expr :args (mapv trim-unanalyzed))))))
  (testing :map
    (let [expr (wrap
                 (analyze1 '{1 2}))]
      (is (= :map (:op expr)))
      (is (= [{:op :unanalyzed
               :form '1}]
             (->> expr :keys (mapv trim-unanalyzed))))
      (is (= [{:op :unanalyzed
               :form '2}]
             (->> expr :vals (mapv trim-unanalyzed))))))
  (testing :vector
    (let [expr (wrap
                 (analyze1 '[1 a]))]
      (is (= :vector (:op expr)))
      (is (= [{:op :unanalyzed
               :form '1}
              {:op :unanalyzed
               :form 'a}]
             (->> expr :items (mapv trim-unanalyzed))))))
  (testing :set
    (let [expr (wrap
                 (analyze1 '#{a}))]
      (is (= :set (:op expr)))
      (is (= [{:op :unanalyzed
               :form 'a}]
             (->> expr :items (mapv trim-unanalyzed))))))
  (testing :js-object
    (let [expr (wrap
                 (analyze1 (cljs.tagged_literals.JSValue. {1 2})))]
      (is (= :js-object (:op expr)))
      (is (= [1]
             (:keys expr)))
      (is (= [{:op :unanalyzed
               :form '2}]
             (->> expr :vals (mapv trim-unanalyzed))))))
  (testing :js-array
    (let [expr (wrap
                 (analyze1 (cljs.tagged_literals.JSValue. [1 2])))]
      (is (= :js-array (:op expr)))
      (is (= [{:op :unanalyzed
               :form '1}
              {:op :unanalyzed
               :form '2}]
             (->> expr :items (mapv trim-unanalyzed))))))
  (testing :with-meta
    (let [expr (wrap
                 (analyze1 '^{a b} #{a}))]
      (is (= :with-meta (:op expr)))
      (is (= :set
             (-> expr :expr :op)))
      (is (= [{:op :unanalyzed
               :form 'a}]
             (->> expr :expr :items (mapv trim-unanalyzed))))
      (is (= :map
             (-> expr :meta :op)))
      (is (= [{:op :unanalyzed
               :form 'a}]
             (->> expr :meta :keys (mapv trim-unanalyzed))))
      (is (= [{:op :unanalyzed
               :form 'b}]
             (->> expr :meta :vals (mapv trim-unanalyzed))))))
  (testing :js-var
    (let [expr (wrap
                 (analyze1 'js/document))]
      (is (= :js-var (:op expr)))
      (is (= 'js/document (:name expr)))))
  (testing :ns*
    (let [expr (wrap
                 (analyze1 '(ns* (:require '[cljs.core :as asdf]))))]
      (is (= :ns* (:op expr)))
      (is (= '{asdf cljs.core, cljs.core cljs.core} (:requires expr)))))
  (testing :ns
    (let [expr (wrap
                 (analyze1 '(ns bar.foo)))]
      (is (= :ns (:op expr)))
      (is (= 'bar.foo (:name expr))))))
