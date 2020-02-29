(ns typed-test.clj.spec.identity
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [clojure.test :refer :all]))

(s/def
  ::identity-mono
  (s/fspec :args (s/cat :x any?)
           :ret any?))

(deftest identity-mono-test
  (is (s/valid? ::identity-mono identity))
  ; no dependency of input to output
  (is (s/valid? ::identity-mono (fn [x] nil))))

(s/def ::identity-fspec-fn
  (s/fspec
    :args (s/cat :x any?)
    :fn (fn [{{:keys [x]} :args :keys [ret]}]
          (identical? ret x))
    :ret any?))

(deftest identity-fspec-fn-test
  ; this test might be flaky?
  (is (s/valid? ::identity-fspec-fn identity))
  (is (not (s/valid? ::identity-fspec-fn (fn [x] nil)))))

(s/def
  ::identity-poly
  (all :binder (binder :x (tvar-spec))
       :body
       (s/fspec :args (s/cat :x (tvar :x))
                :ret (tvar :x))))

(deftest identity-poly-test
  (is (s/valid? ::identity-poly identity))
  (is (not (s/valid? ::identity-poly (fn [x] nil)))))
