(ns clojure.core.typed.test.filter-unit-tests
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.cljc.checker.filter-ops :refer :all]
            [typed.cljc.checker.path-rep :refer :all]
            [typed.cljc.checker.type-ctors :refer :all]))

(deftest refine-branch-test
  (is-tc-e (do
             (defalias M (U '{:a Int}
                            '{:a Sym}))
             (fn [a :- M]
               (if (symbol? (:a a))
                 (ann-form (:a a) Sym)
                 (ann-form (:a a) Int))))))
