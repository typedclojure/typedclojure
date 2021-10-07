(ns typed-test.clj.malli
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.typed :as t]
            [typed.clj.malli :as sut] ;:as tm
            [clojure.core.typed.test.test-utils :refer [is-tc-e is-tc-err]]
            [malli.core :as m]))

(t/load-if-needed)

(sut/defparser AnyInteger-parser t/AnyInteger)


(deftest malli-ops-test
  (is (true? (sut/validate t/Any 1)))
  (is (false? (sut/validate t/AnyInteger nil)))
  (is (false? ((sut/validator t/AnyInteger) nil)))
  (is (true? ((sut/validator t/AnyInteger) 1)))
  (is (true? ((sut/validator t/Bool) true)))
  (is (true? ((m/validator boolean?) true)))
  (is (true? ((sut/validator Boolean) true)))
  (is (= 1 (AnyInteger-parser 1)))
  (is (= ::m/invalid (AnyInteger-parser nil)))
  (is (= ::m/invalid (AnyInteger-parser nil)))
  (is (= 1
         (sut/parse t/AnyInteger 1)))
  (is (= ::m/invalid
         (sut/parse t/AnyInteger nil)))
  (is (= [:left :left]
         (sut/parse (t/U ^{::sut/name :left} ':left
                         ^{::sut/name :right} ':right)
                    :left)))
  (is (= [:right :right]
         (sut/parse (t/U ^{::sut/name :left} ':left
                         ^{::sut/name :right} ':right)
                    :right)))
  (is (= ::m/invalid
         (sut/parse (t/U ^{::sut/name :left} ':left
                         ^{::sut/name :right} ':right)
                    :middle)))
  (is (= [:int 1]
         (sut/parse (t/U ^{::sut/name :int} t/AnyInteger
                         ^{::sut/name :bool} t/Bool)
                    1)))
  (is (= [:bool true]
         (sut/parse (t/U ^{::sut/name :int} t/AnyInteger
                         ^{::sut/name :bool} t/Bool)
                    true))))

(def Hiccup
  [:schema {:registry {"hiccup" [:orn
                                 [:node [:catn
                                         [:name keyword?]
                                         [:props [:? [:map-of keyword? any?]]]
                                         [:children [:* [:schema [:ref "hiccup"]]]]]]
                                 [:primitive [:orn
                                              [:nil nil?]
                                              [:boolean boolean?]
                                              [:number number?]
                                              [:text string?]]]]}}
   "hiccup"])

(comment
  (m/parse Hiccup [:div [:p "foo"]])
  (m/parse Hiccup [:div [:p :a "foo"]])
  ((requiring-resolve 'clojure.repl/pst) 100)
  )

(deftest malli-type-check-test
  (is-tc-e (fn [a :- t/Any] :- t/AnyInteger
             (assert (tm/validate t/AnyInteger a))
             a)
           :requires [[typed.clj.malli :as tm]])
  (is-tc-e (fn [a :- t/Any] :- t/AnyInteger
             (assert (not (tm/explain t/AnyInteger a)))
             a)
           :requires [[typed.clj.malli :as tm]])
  (is-tc-e (fn [a :- t/Any] :- t/AnyInteger
             (assert ((tm/validator t/AnyInteger) a))
             a)
           :requires [[typed.clj.malli :as tm]])
  (is-tc-err (fn [a :- t/Any] :- t/AnyInteger
               (assert (tm/validate t/Bool a))
               a)
             :requires [[typed.clj.malli :as tm]])
  (is-tc-err (fn [a :- t/Any] :- t/AnyInteger
               (assert (not (tm/explain t/Bool a)))
               a)
             :requires [[typed.clj.malli :as tm]])
  (is-tc-err (fn [a :- t/Any] :- t/AnyInteger
               (assert ((tm/validator t/Bool) a))
               a)
             :requires [[typed.clj.malli :as tm]])
  (is-tc-e (do (tm/defvalidator AnyInteger? t/AnyInteger)
               (fn [a :- t/Any] :- t/AnyInteger
                 (assert (AnyInteger? a))
                 a))
           :requires [[typed.clj.malli :as tm]])
  (is-tc-err (do (tm/defvalidator Bool? t/Bool)
                 (fn [a :- t/Any] :- t/AnyInteger
                   (assert (Bool? a))
                   a))
             :requires [[typed.clj.malli :as tm]])
  (is-tc-e (fn [a :- (t/U t/AnyInteger t/Bool)] :- t/AnyInteger
             (let [prs (tm/parse (t/U ^{::sut/name :int} t/AnyInteger
                                      ^{::sut/name :bool} t/Bool)
                                 a)]
               (assert (not= ::m/invalid prs))
               ;(assert (vector? prs))
               (case (first prs)
                 ;; TODO this would be neat. need to strengthen m/parser :filters
                 ;:int a
                 :int (second prs))))
           :requires [[typed.clj.malli :as tm]]))
