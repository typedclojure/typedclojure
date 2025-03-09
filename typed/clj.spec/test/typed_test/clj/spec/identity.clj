(ns ^:typed.clojure typed-test.clj.spec.identity
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

(s/def
  ::identity-mono
  (s/fspec :args (s/cat :x any?)
           :ret any?))

(deftest identity-mono-test
  (tu/is-valid ::identity-mono identity)
  ; no dependency of input to output
  (tu/is-valid ::identity-mono (fn [x] nil)))

(s/def ::identity-fspec-fn
  (s/fspec
    :args (s/cat :x any?)
    :fn (fn [{{:keys [x]} :args :keys [ret]}]
          (identical? ret x))
    :ret any?))

(deftest identity-fspec-fn-test
  (tu/is-valid ::identity-fspec-fn identity)
  (tu/is-invalid ::identity-fspec-fn (fn [x] nil)))

(s/def
  ::identity-poly
  (t/all :binder (t/binder :x (t/bind-tv))
         :body
         (s/fspec :args (s/cat :x (t/tv :x))
                  :ret (t/tv :x))))

(deftest identity-poly-test
  (tu/is-valid ::identity-poly identity)
  (tu/is-valid ::identity-poly (comp first
                                     (juxt identity identity)
                                     (fn [x] x)))
  (tu/is-invalid ::identity-poly (fn [x] nil)))
