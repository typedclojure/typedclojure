(ns ^:typed.clojure clojure.core.typed.test.ctyp-257
  (:require [clojure.test :refer :all]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.test-utils :refer :all]))

(deftest empty-intersection-test
  (testing "empty intersection should be Top, not Bottom"
    (is (= (c/make-Intersection [] (clj-opts)) r/-any))
    (is (= (c/In [] {}) r/-any)))
  (testing "intersection containing Bottom is Bottom"
    (is (= (c/In [r/-nothing r/-any] {})
           r/-nothing))))
