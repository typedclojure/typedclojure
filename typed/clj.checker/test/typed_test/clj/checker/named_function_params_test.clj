(ns typed-test.clj.checker.named-function-params-test
  "Tests for named function parameters support"
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.typed.parse-ast :as parse-ast]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.runtime.env :refer [clj-opts]]
            [typed.clojure :as t]))

(def this-ns (ns-name *ns*))

(deftest function-named-params-test
  (testing "Simple named function parameter"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[x :- t/Int :-> t/Int :object x]))
          arity (first (:arities ast))
          obj (:object arity)]
      ;; Object should reference the named parameter 'x'
      (is (symbol? (:id obj)) "Simple case: object id should be gensym'd symbol")))
  
  (testing "Nested functions with outer parameter reference"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[x :- t/Any :-> [y :- t/Any :-> t/Any :object x]]))
          outer-arity (first (:arities ast))
          inner-fn (:rng outer-arity)
          inner-arity (first (:arities inner-fn))
          inner-obj (:object inner-arity)
          inner-id (:id inner-obj)]
      ;; Inner function's object should reference outer parameter x using Lexical
      (is (map? inner-id) "Inner reference to x should be Lexical object")
      (is (= :Lexical (:op inner-id)))
      (is (= 1 (:depth inner-id)) "Should skip 1 scope to reach outer x")
      (is (= 0 (:index inner-id)) "x is the first parameter of outer function")))
  
  (testing "Mixed named and unnamed parameters"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[x :- t/Int t/Str :-> t/Any]))
          arity (first (:arities ast))
          dom (:dom arity)]
      (is (= 2 (count dom)) "Should have 2 domain types")
      (is (= :Name (:op (first dom))) "First param should be Int (as Name type)")
      (is (= 'typed.clojure/Int (:name (first dom))) "First param resolves to typed.clojure/Int")
      (is (= :Name (:op (second dom))) "Second param should be String (as Name type)")
      (is (= 'typed.clojure/Str (:name (second dom))) "Second param resolves to typed.clojure/Str")))

  (testing "Named parameter with simple object reference"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[x :- t/Any :-> t/Any :object x]))
          arity (first (:arities ast))
          obj (:object arity)
          obj-id (:id obj)]
      ;; The object should reference the parameter x
      (is (symbol? obj-id) "Object should reference parameter x via gensym")
      (is (not= 'x obj-id) "Should use gensym, not literal x"))))

(deftest polymorphic-function-type-variable-and-param-same-name-test
  (testing "Polymorphic function where type variable and parameter share name 'x'"
    ;; Test: (All [x] [x :- x :-> x :object x])
    ;; The type variable 'x' and parameter 'x' are in different namespaces
    ;; Type variables are for types, object parameters are for values
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '(t/All [x] [x :- x :-> x :object x])))
          ;; Get the All/Poly type
          _ (is (= :Poly (:op ast)) "Top level should be Poly")
          ;; The body of All/Poly should be a function type
          fn-type (:type ast)
          _ (is (= :Fn (:op fn-type)))
          arity (first (:arities fn-type))
          ;; Check domain: should reference type variable x
          dom (first (:dom arity))
          _ (is (= :F (:op dom)) "Domain type should reference type variable x")
          ;; Check range: should reference type variable x
          rng (:rng arity)
          _ (is (= :F (:op rng)) "Range type should reference type variable x")
          ;; Check object: should reference parameter x (the value parameter)
          obj (:object arity)
          obj-id (:id obj)]
      ;; The object id should be a gensym (referring to the value parameter x)
      ;; It should NOT conflict with the type variable x
      (is (symbol? obj-id) "Object should reference the value parameter x")
      (is (not= 'x obj-id) "Should use gensym for parameter, not literal x"))))

(deftest three-level-nesting-test
  (testing "Triple nested functions referencing outer parameters"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[x :- t/Any :-> [y :- t/Any :-> [z :- t/Any :-> t/Any :object x]]]))
          outer-arity (first (:arities ast))
          middle-fn (:rng outer-arity)
          middle-arity (first (:arities middle-fn))
          inner-fn (:rng middle-arity)
          inner-arity (first (:arities inner-fn))
          inner-obj (:object inner-arity)
          inner-id (:id inner-obj)]
      ;; Innermost function's object should reference outermost parameter x using Lexical
      (is (map? inner-id) "Inner reference to x should be Lexical object")
      (is (= :Lexical (:op inner-id)))
      (is (= 2 (:depth inner-id)) "Should skip 2 scopes to reach outer x")
      (is (= 0 (:index inner-id)) "x is the first parameter of outer function"))))

(deftest reference-middle-parameter-test
  (testing "Nested function referencing middle-level parameter"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[x :- t/Any :-> [y :- t/Any :-> [z :- t/Any :-> t/Any :object y]]]))
          outer-arity (first (:arities ast))
          middle-fn (:rng outer-arity)
          middle-arity (first (:arities middle-fn))
          inner-fn (:rng middle-arity)
          inner-arity (first (:arities inner-fn))
          inner-obj (:object inner-arity)
          inner-id (:id inner-obj)]
      ;; Innermost function's object should reference middle parameter y using Lexical
      (is (map? inner-id) "Inner reference to y should be Lexical object")
      (is (= :Lexical (:op inner-id)))
      (is (= 1 (:depth inner-id)) "Should skip 1 scope to reach middle y")
      (is (= 0 (:index inner-id)) "y is the first parameter of middle function"))))

(deftest multiple-params-with-index-test
  (testing "Function with multiple named parameters"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[a :- t/Int b :- t/Str c :- t/Bool :-> t/Any :object b]))
          arity (first (:arities ast))
          obj (:object arity)
          obj-id (:id obj)]
      ;; Object should reference the second parameter 'b' (index 1)
      (is (symbol? obj-id) "Object should reference parameter b"))))

(deftest nested-with-multiple-params-test
  (testing "Nested function referencing outer second parameter"
    (let [ast (binding [*ns* (the-ns this-ns)]
                (parse-ast/parse-clj '[a :- t/Int b :- t/Str :-> [c :- t/Bool :-> t/Any :object b]]))
          outer-arity (first (:arities ast))
          inner-fn (:rng outer-arity)
          inner-arity (first (:arities inner-fn))
          inner-obj (:object inner-arity)
          inner-id (:id inner-obj)]
      ;; Inner function's object should reference outer second parameter b using Lexical
      (is (map? inner-id) "Inner reference to b should be Lexical object")
      (is (= :Lexical (:op inner-id)))
      (is (= 1 (:depth inner-id)) "Should skip 1 scope to reach outer b")
      (is (= 1 (:index inner-id)) "b is the second parameter (index 1) of outer function"))))
