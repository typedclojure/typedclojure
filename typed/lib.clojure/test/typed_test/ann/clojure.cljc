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
  (clj/is-tc-e (t/ann-form (doto (java.util.HashMap.)
                             (.put "foo" "bar"))
                           (t/MapConjable t/Any t/Any)))
  (clj/is-tc-e (t/ann-form (java.util.HashMap.) (t/MapConjable t/Any t/Any)))
  (clj/is-tc-e (let [m (conj {} (java.util.HashMap.))]
                 (t/ann-form m (t/Map t/Any t/Any)))))

(deftest seqable-test
  (clj/is-tc-e (seq (doto (java.util.HashMap.) (.put "foo" "bar")))
               (t/NilableNonEmptySeq (t/MapEntry t/Any t/Any)))
  ;; TODO
  (clj/is-tc-err (seq (doto (java.util.HashMap.) (.put "foo" "bar")))
                 (t/NilableNonEmptySeq (t/MapEntry t/Str t/Str)))
  (clj/is-tc-e (seq "abc") (t/NilableNonEmptySeq Character))
  (clj/is-tc-e (seq (StringBuffer. "abc")) (t/NilableNonEmptySeq Character)))
