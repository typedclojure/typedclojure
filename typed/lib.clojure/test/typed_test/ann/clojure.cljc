(ns typed-test.ann.clojure
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :as clj]
            [typed.clj.checker.test-utils :as cljs]))

(deftest seqable?-test
  (clj/is-tc-e (seqable? 1))
  (cljs/is-tc-e (seqable? 1)))

(deftest indexed?-test
  (clj/is-tc-e (indexed? 1))
  (cljs/is-tc-e (indexed? 1)))

(deftest find-test
  (clj/is-tc-e (when-some [e (find {:a 1} :a)]
                 [(key e) (val e)])
               (t/Nilable '[':a t/Int])))
