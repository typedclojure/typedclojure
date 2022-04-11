(ns typed-test.cljc.checker.check.invoke
  (:require [clojure.test :refer :all]
            [typed.clj.checker.test-utils :refer :all]))

(deftest normal-invoke-apply
  (is-tc-e (apply (inst hash-map Number String) 1 ["a"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" [2 "b"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" [2 "b" 3 "c"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" [2 "b" 3 "c" 4 "c"]) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" []) :expected (t/Map Number String))
  (is-tc-e (apply (inst hash-map Number String) 1 "a" nil) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 "a" [2 \c]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 "a" [2 "b" 3 \c]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 \a [2 "c"]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 "a" [2 \c]) :expected (t/Map Number String))
  (is-tc-err (apply (inst hash-map Number String) 1 \a [2 \c]) :expected (t/Map Number String)))

(deftest function-to-args-type-propagation-test
  (is-tc-e (cc/fn [p] (p #(inc %)))
           [[[t/Int :-> t/Int] :-> t/Int] :-> t/Int])
  (is-tc-err (cc/fn [p] (p #(inc %)))
             [[[t/Bool :-> t/Int] :-> t/Int] :-> t/Int])
  ;; resolve types
  (is-tc-e (do (defalias Foo [t/Int :-> t/Int])
               (ann-form (cc/fn [p] (p #(inc %)))
                         [[Foo :-> t/Int] :-> t/Int])))
  (is-tc-err (do (defalias Foo [t/Bool :-> t/Int])
                 (ann-form (cc/fn [p] (p #(inc %)))
                           [[Foo :-> t/Int] :-> t/Int]))))
