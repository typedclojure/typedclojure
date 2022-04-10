(ns typed-test.clj.ext.clojure.core__fn
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [typed.clj.ext.clojure.core__fn :as sut]))

(deftest prep-tc-meta-test
  (let [[form metas] (sut/prep-tc-meta
                       '(cc/fn [^{:other-meta 2} a] (inc a)))]
    (is (nil? metas))
    (is (= '(cc/fn [a] (inc a)) form))
    (is (= {:other-meta 2, :clojure.core.typed.internal/destructure-blame-form '(cc/fn [a] (inc a))}
           (-> form second first meta))))
  (let [[form metas] (sut/prep-tc-meta
                       '(cc/fn 
                          [^{::t/- t/Int :other-meta 2} a] (inc a)))]
    (is (= '#{{:type :fixed :method-pos 0 :nfixed 1 :has-rest? false :form a :fixed-pos 0 :annotated true}
              {:type :argv :method-pos 0 :nfixed 1 :has-rest? false :form [a]}}
           metas))
    (is (= '(cc/fn [a] (inc a)) form))
    (is (= {:other-meta 2, :clojure.core.typed.internal/destructure-blame-form '(cc/fn [a] (inc a))}
           (-> form second first meta))))
  (let [[form metas] (sut/prep-tc-meta
                       '(cc/fn ^{::t/- t/Bool :yet-more-meta 3}
                          [^{::t/- t/Int :other-meta 2} a] (inc a)))]
    (is (= '#{{:type :fixed :method-pos 0 :nfixed 1 :has-rest? false :form a :fixed-pos 0 :annotated true}
              {:type :argv :method-pos 0 :nfixed 1 :has-rest? false :form [a] :annotated true}}
           metas))
    (is (= '(cc/fn [a] (inc a)) form))
    (is (= {:other-meta 2, :clojure.core.typed.internal/destructure-blame-form '(cc/fn [a] (inc a))}
           (-> form second first meta)))
    (is (= {:yet-more-meta 3}
           (-> form second meta))))
  (let [[form metas] (sut/prep-tc-meta
                       '(cc/fn ^{::t/- [t/Bool :-> t/Int] :even-more-meta 3}
                          _a
                          [a] (inc a)))]
    (is (= '#{{:type :fixed :method-pos 0 :nfixed 1 :has-rest? false :form a :fixed-pos 0}
              {:type :argv :method-pos 0 :nfixed 1 :has-rest? false :form [a]}
              {:type :name :form _a :annotated true}}
           metas))
    (is (= '(cc/fn _a [a] (inc a)) form))
    (is (= {:even-more-meta 3}
           (-> form second meta)))))
