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
  (clj/is-tc-e (when-some [e (find nil :a)]
                 [(key e) (val e)])
               nil)
  (clj/is-tc-e (when-some [e (find {:a 1} :a)]
                 [(key e) (val e)])
               (t/Nilable '[':a t/Int]))
  (clj/is-tc-e (when-some [e (find (when (< (rand) 0.5) {:a 1}) :a)]
                 [(key e) (val e)])
               (t/Nilable '[':a t/Int]))
  (clj/is-tc-err (when-some [e (find (java.util.HashMap. {:a 1}) :a)]
                   [(key e) (val e)])
                 nil)
  (clj/is-tc-err (when-some [e (find (java.util.HashMap. {:a 1}) :a)]
                   [(key e) (val e)])
                 '[t/Any t/Any])
  (clj/is-tc-e (when-some [e (find (java.util.HashMap. {:a 1}) :a)]
                 [(key e) (val e)])
               (t/Nilable '[t/Any t/Any]))
  (clj/is-tc-err (when-some [e (find (java.util.HashMap. {:a 1}) :a)]
                   [(key e) (val e)])
                 (t/Nilable '[':a t/Int])))

(deftest conj-test
  (clj/is-tc-e (conj {} (java.util.HashMap.))
               (t/Map t/Any t/Any)))
