(ns clojure.core.typed.test.filter-unit-tests
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.cljc.checker.filter-ops :refer :all]
            [typed.cljc.checker.path-rep :refer :all]
            [typed.cljc.checker.type-ctors :refer :all]))

(deftest refine-branch-test
  (is-tc-e (do
             (defalias M (t/U '{:a t/Int}
                              '{:a t/Sym}))
             (fn [a :- M]
               (if (symbol? (:a a))
                 (ann-form (:a a) t/Sym)
                 (ann-form (:a a) t/Int))))))
