(ns clojure.core.typed.test.multimethods
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clojure :as t]
            [clojure.core.typed :refer [check-ns]]
            [clojure.test :refer :all]))

(deftest multimethod-test
  (is (check-ns 'clojure.core.typed.test.mm
                :max-parallelism 1))
  (is-tc-e (do (ann f [t/Any -> t/Any])
               (defmulti f class)
               (defmethod f Number [n] (inc n))))
  (is-tc-e (do (ann f [t/Any -> t/Any])
               (defmulti f (cc/fn [a] (class a)))
               (defmethod f Number [n] (inc n))))
  (is-tc-e (cc/fn [a] (class a))
           [t/Any :-> (t/Option Class) :object {:id 0 :path [Class]}])
  (is-tc-err (cc/fn [a b] (class b))
             [t/Any t/Any :-> (t/Option Class) :object {:id 0 :path [Class]}])
  (is-tc-e (fn [a] (class a))
           [t/Any :-> (t/Option Class) :object {:id 0 :path [Class]}])
  (is-tc-err (fn [a b] (class b))
             [t/Any t/Any :-> (t/Option Class) :object {:id 0 :path [Class]}])
  (is-tc-e (let [f (fn [a] (class a))]
             f)
           [t/Any :-> (t/Option Class) :object {:id 0 :path [Class]}])
  (is-tc-e (do (ann f [t/Any -> t/Any])
               (defmulti f (fn [a] (class a)))
               (defmethod f Number [n] (inc n))))
  (is-tc-e (do (ann f [t/Any t/Any -> t/Any])
               (defmulti f (fn [a b]
                             [(class a) (class b)]))
               (defmethod f [Number Number] [n1 n2] (+ n1 n2)))))
