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
