(ns ^:typed.clojure typed-test.clj.spec.tutorial
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

;testing for doc/tutorial.md
(defn f
  "Stringifies an integer"
  [x] (str x))

;; register its spec
(s/def ::f (s/fspec :args (s/cat :x integer?)
                    :ret string?))

;; generatively test implementation based on spec
(deftest f-example-test
  (tu/is-valid ::f f)
  (tu/is-valid ::f (fn [x] (prn "x" x) (str x)))
  (binding [s/*fspec-iterations* 2]
    (tu/is-valid 
      ::f
      (fn [x] (prn "x" x) (str x)))))

(s/def
  ::identity
  (t/all :binder (t/binder :x (t/bind-tv))
         :body (s/fspec :args (s/cat :x (t/tv :x))
                        :ret (t/tv :x))))

(deftest identity-test
  (tu/is-valid ::identity identity)
  (tu/is-valid ::identity (fn [x] x))
  (tu/is-valid ::identity #(-> {}
                               (update % vector %)
                               (update % peek)
                               (get %)))
  (tu/is-valid ::identity (comp first
                                (juxt #(apply % []) identity)
                                (fn [x]
                                  (constantly x)))))


(defn id [input]
  (let [I (fn [a] a)
        K (fn [b]
            (fn [c]
              b))
        D (fn [d]
            (d d))]
    (let [GR ((fn [x]
                (fn [y]
                  ((y (x I))
                   (x K))))
              D)]
      (GR (fn [_]
            (fn [_]
              input))))))

(deftest GR-test
  (is (= 1 (id 1)))
  (tu/is-valid ::identity id))

(deftest inst-test
  (is (s/describe ::identity))
  (is (s/describe (t/inst ::identity {:x any?})))
  (is (s/describe (t/inst ::identity {:x integer?})))
  (is (s/describe (t/inst ::identity {:x #{a}})))
  (is (s/describe (t/inst ::identity {}))
      (is (doall
            (take 10 (map (comp s/describe s/resolve-spec)
                          (repeat `(t/inst ::identity {})))))))
  (is
    (-> (t/binder :x (t/bind-tv))
        s/gen 
        gen/sample))
  (tu/is-valid (t/binder :x (t/bind-tv)) `{:x integer?})
  (tu/is-valid (t/binder :x (t/bind-tv)) `{:x any?})
  (is (gen/sample (s/gen (t/bind-tv))))
  (is (gen/sample (s/gen (t/bind-tv :kind nat-int?))))
  (is (gen/sample (s/gen (t/bind-tv :kind nat-int?)))))
