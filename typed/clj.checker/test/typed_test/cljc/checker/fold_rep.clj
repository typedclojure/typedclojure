(ns typed-test.cljc.checker.fold-rep
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.cs-gen :as sut]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.fold-rep :as f]
            [typed.clj.checker.parse-unparse :refer [parse-type]]
            [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t])
  (:import (typed.cljc.checker.type_rep F)))

(f/def-derived-fold ICountF count-F* [count-atom])

(f/add-fold-case
  ICountF count-F*
  F
  (fn [t count-atom]
    (swap! count-atom inc)
    t))

(defn count-F [t opts]
  {:pre [(r/AnyType? t)]}
  (let [count-atom (atom 0)
        t' (call-count-F* t opts {:count-atom count-atom})]
    {:t' t' :F-count @count-atom}))

(deftest identical-fold-return-test
  (clj (let [t (r/make-F 'x)
             {:keys [t' F-count]} (count-F t (clj-opts))]
         (is (= 1 F-count))
         (is (identical? t t'))))
  (clj (let [t (r/make-Function [(r/make-F 'x)] (r/make-F 'x))
             {:keys [t' F-count]} (count-F t (clj-opts))]
         (is (= 2 F-count))
         (is (identical? t t'))))
  (clj (let [t (parse-type `(t/All [t#] [[t# :-> [t# :-> t#]] '[t#] :-> (t/Vec t#)]) (clj-opts))
             {:keys [t' F-count]} (count-F t (clj-opts))]
         (is (= 5 F-count))
         (is (identical? t t'))))
  (clj (let [t (parse-type `(t/All [t# s# :..] [[t# :-> [t# :-> t#]] '[t#] :-> (t/Vec t#)]) (clj-opts))
             {:keys [t' F-count]} (count-F t (clj-opts))]
         (is (= 5 F-count))
         (is (identical? t t'))))
  (clj (let [t (parse-type `(t/All [t#] ['{:a t#} :-> (t/HMap :mandatory {:a t#} :optional {:b t#})]) (clj-opts))
             {:keys [t' F-count]} (count-F t (clj-opts))]
         (is (= 3 F-count))
         (is (identical? t t'))))
  (clj (let [t (parse-type `(t/All [t#] [:-> ((t/TFn [x#] t#) t#)]) (clj-opts))
             {:keys [t' F-count]} (count-F t (clj-opts))]
         (is (= 2 F-count))
         (is (identical? t t'))))
  )
