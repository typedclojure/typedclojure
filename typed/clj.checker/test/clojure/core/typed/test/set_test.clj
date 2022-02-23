(ns clojure.core.typed.test.set-test
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest construct-hset
  (is-tc-e #{1 2 3}
           :expected
           (t/HSet #{1 2 3}))
  (is-tc-e #{1 nil}
           :expected
           (t/HSet #{nil 1})))

(deftest upcast-hset
  (is-tc-e #{1 2 3}
           :expected
           (t/Set t/Num))
  (is-tc-e (conj #{1 2 3} 1)
           :expected
           (t/Set t/Num)))

(deftest set-pred-test
  (is-tc-e (let [foo :- (t/U false nil ':a ':b), :a]
             (if (#{:a :b false nil} foo)
               (ann-form foo (t/U ':a ':b))
               (ann-form foo (t/U false nil)))))
  (is-tc-e (let [foo :- (t/U false nil ':a ':b), :a]
             (when (#{:a :b nil} foo)
               (ann-form foo (t/U ':a ':b)))))
  (is-tc-e (let [foo :- (t/U nil ':a ':b), :a]
             (when (#{:a :b nil} foo)
               (ann-form foo (t/U ':a ':b))))))
