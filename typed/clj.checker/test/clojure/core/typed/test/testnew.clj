(ns clojure.core.typed.test.testnew
  (:require [typed.clojure :as t]
            [clojure.test :refer :all]
            [typed.clj.checker.test-utils :refer :all]))

(deftest function?-test
  (is-tc-e (function? function?) t/Bool
           :requires [[clojure.test :refer [function?]]])
  (is-tc-err (function? function?) t/Str
             :requires [[clojure.test :refer [function?]]]))
  
(deftest assert-any-test
  (is-tc-e (assert-any "Hi" (= 4 (+ 2 2))) t/Any
           :requires [[clojure.test :refer [assert-any]]]))
           
(deftest do-report-test
  (is-tc-e #(do-report 1)
           :requires [[clojure.test :refer [do-report]]]))
            
(deftest run-tests-test
  (is-tc-e #(run-tests) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.test :refer [run-tests]]])
  (is-tc-err #(run-tests) [-> t/Str]
             :requires [[clojure.test :refer [run-tests]]]))
            
(deftest run-all-tests-test
  (is-tc-e #(run-all-tests) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.test :refer [run-all-tests]]])
  (is-tc-e #(run-all-tests #"asdf") [-> (t/Map t/Any t/Any)]
           :requires [[clojure.test :refer [run-all-tests]]])
  (is-tc-err #(run-all-tests) [:-> t/Str]
             :requires [[clojure.test :refer [run-all-tests]]]))
            
(deftest successful?-test
  (is-tc-e #(successful? {}) [-> t/Bool]
           :requires [[clojure.test :refer [successful?]]]))
            
(deftest compose-fixtures-test
  (is-tc-e (compose-fixtures (fn [a :- [-> t/Any]] (a)) 
                             (fn [b :- [-> t/Any]] (b)))
           [[-> t/Any] -> t/Any]
           :requires [[clojure.test :refer [compose-fixtures]]]))
           
(deftest testing-vars-str-test
  (is-tc-e #(testing-vars-str {}) [-> t/Str]
           :requires [[clojure.test :refer [testing-vars-str]]])
  (is-tc-err (testing-vars-str 1) t/Int
             :requires [[clojure.test :refer [testing-vars-str]]]))
           
(deftest testing-contexts-str-test
  (is-tc-e (testing-contexts-str) t/Str
           :requires [[clojure.test :refer [testing-contexts-str]]])
  (is-tc-err (testing-contexts-str) (t/Map t/Any t/Any)
             :requires [[clojure.test :refer [testing-contexts-str]]])
  (is-tc-err (testing-contexts-str 1) t/Str
             :requires [[clojure.test :refer [testing-contexts-str]]]))

(deftest test-ns-test
  (is-tc-e #(test-ns 'some-namespace-symbol) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.test :refer [test-ns]]])
  (is-tc-e #(test-ns *ns*) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.test :refer [test-ns]]]))
