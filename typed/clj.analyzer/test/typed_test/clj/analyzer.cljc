(ns typed-test.clj.analyzer
  (:require [clojure.test :refer :all]
            [typed.clj.analyzer :as ana]))

(defmacro ast' [form]
  `(ana/analyze '~form (ana/empty-env (ns-name *ns*)) (ana/default-opts)))

(defmacro ast [form]
  `(ana/analyze+eval '~form (ana/empty-env (ns-name *ns*)) (ana/default-opts)))

(deftest analyzer-test
  (is (= 1
         (:result (ast 1))))
  (is (= 2 
         (:result (ast (-> 1 inc)))))
  (is (= 1
         (:result (ast (let [a 1] a)))))
  (is (= 1
         (:result (ast (loop [a 1] a)))))
  (is (= 1
         (:result (ast (do (def a 1)
                           a)))))
  (is (= 1
         (:result (ast (do (deftype Abc [a])
                           (.a (->Abc 1)))))))
  (is (= true
         (:result (ast (do (ns foo) (= 1 1))))))
  (is (= "a"
         (:result (ast #?(:cljr (.ToString (reify Object (ToString [this] "a")))
                          :default (.toString (reify Object (toString [this] "a"))))))))
  (is (= 2 (:result (ast (#(inc %) 1)))))
  #_
  (is (->
        (ast (do (ns bar
                   (:require [typed.clojure :as t]))
                 (t/ann-form 'foo 'a)))
        :ret))
  #?(:cljr 
     (is (= [:const Int64]
            ((juxt :op :val) (ast Int64))))
     :default 	 
     (is (= [:const Number]
            ((juxt :op :val) (ast Number)))))
  (is (= [:const clojure.lang.Compiler]
         ((juxt :op :val) (ast clojure.lang.Compiler))))

  #?(:cljr
     (is (= [:static-field 'specials]
            ((juxt :op :field) (ast clojure.lang.Compiler/specials))))
     :default   
     (is (= [:static-field 'LOADER]
            ((juxt :op :field) (ast clojure.lang.Compiler/LOADER)))))
  )

(deftest local-tag-test
  (is (= #?(:cljr System.String :default java.lang.String)
         (:tag (ast "asdf"))))
  (is (= [:const #?(:cljr System.String :default java.lang.String)]
         (-> (ast (let [a "asdf"]))
             :bindings
             first
             :init
             ((juxt :op :tag)))))
  (is (= [:binding #?(:cljr System.String :default java.lang.String)]
         (-> (ast (let [a "asdf"]))
             :bindings
             first
             ((juxt :op :tag)))))
  (is (= [:local #?(:cljr System.String :default java.lang.String)]
         (-> (ast (let [a "asdf"]
                    a))
             :body
             :ret
             ((juxt :op :tag)))))
  (is (= #?(:cljr System.String :default java.lang.String)
         (:tag (ast (let [a "asdf"]
                      a)))))
  )

(deftest deftype-test
  (is (some?
        (binding [*ns* *ns*]
          (eval `(ns ~(gensym)))
          (ast
            (deftype A []
              Object
              (#?(:cljr ToString :default toString) [_] (A.) "a")))))))

(deftest uniquify-test
  (let [ret (ast' (let [a 1]
                    (let [a 2]
                      a)))]
    (is (= (let [sym (-> ret :body :ret :bindings first :name)]
             (is (symbol? sym))
             sym)
           (-> ret :body :ret :body :ret :name)))
    (is (not= 'a (-> ret :body :ret :body :ret :name)))))

(deftest def-children-test
  (binding [*ns* (create-ns (gensym 'test))]
    (let [def-ast (ast' (def a))]
      (is (= :def (:op def-ast)))
      (is (nil? (:init def-ast)))
      (is (= [:meta] (:children def-ast))))))

(def array-class-literal-feature? (try (read-string "byte/2")
                                       true
                                       (catch Exception _ false)))

(when array-class-literal-feature?
  (deftest array-class-literal-test
    #?(:cljr (throw (ex-info "TODO array-class-literal-test for cljr" {}))
       :default (is (= [:const (clojure.lang.RT/classForName "[[B")]
                       ;; Using `read-string` here because 'byte/2 is not even a valid
                       ;; symbol for pre-1.12 reader.
                       ((juxt :op :result) (ana/analyze+eval (read-string "byte/2") (ana/empty-env (ns-name *ns*)) (ana/default-opts)))))))

  (deftest method-reference-test
    #?@(:cljr ((throw (ex-info "TODO method-reference-test for cljr" {})))
        :default
        [(is (= [:static-method-reference String 'join]
                ((juxt :op :class :method) (ast' String/join))))
         (is (= [:instance-method-reference java.lang.String 'substring]
                ((juxt :op :class :method) (ast' String/.substring))))])))
