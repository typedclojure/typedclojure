(ns ^:typed.clojure typed-test.clj.checker.any-mode-test
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clj.checker.test-utils :refer [tc-e is-tc-err]]))

(deftest any-mode-function-inference-test
  (testing ":any mode allows calling unannotated functions with various arities"
    (is (tc-e
         (do
           (defn unannotated-fn [& args] args)

           (let [r1 (unannotated-fn 1 2)
                 r2 (unannotated-fn "a" "b")
                 r3 (unannotated-fn 1 2 3 4 5)]
             r1))
         :check-config {:unannotated-var :any})))

  (testing ":any mode infers unannotated non-function vars as Any"
    (is (tc-e
         (do
           (def unannotated-var 42)

           (let [x unannotated-var]
             (if (number? x)
               (inc x)
               x)))
         :check-config {:unannotated-var :any})))

  (testing ":error mode throws error for unannotated vars"
    (is-tc-err
     (do
       (def my-unannotated-var 42)
       my-unannotated-var)
     :check-config {:unannotated-var :error}))

  (testing "Functions work with correct arity in :any mode"
    (is (tc-e
         (do
           (defn simple-fn [x] (str x))
           (simple-fn "hello"))
         :check-config {:unannotated-var :any}))))
