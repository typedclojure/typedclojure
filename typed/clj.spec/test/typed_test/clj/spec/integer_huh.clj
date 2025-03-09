(ns ^:typed.clojure typed-test.clj.spec.integer-huh
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

(s/def ::integer?-fspec-fn
  (s/fspec
    :args (s/cat :n (s/with-gen
                      any?
                      #(gen/one-of
                         [(s/gen (s/resolve-spec `integer?))
                          (s/gen (s/resolve-spec `any?))])))
    :fn (fn [{{:keys [n]} :args :keys [ret]}]
          (= ret (integer? n)))
    :ret boolean?))

(deftest integer?-fspec-fn-test
  (tu/is-valid ::integer?-fspec-fn integer?)
  (tu/is-valid ::integer?-fspec-fn (fn [x]
                                     ;(prn "x" x)
                                     (integer? x)))
  (tu/is-invalid ::integer?-fspec-fn symbol?))

#_
(s/def ::integer?-poly
  (t/all :binder
       (t/binder
         :x (t/bind-tv))
       :body
       (s/fspec :args (s/cat :x (t/tv :x))
                :ret #{(t/tv :x
                           ;FIXME this is wrong since n is a spec, not a value
                             :wrap
                             (fn [n]
                               (or (instance? Integer n)
                                   (instance? Long n)
                                   (instance? clojure.lang.BigInt n)
                                   (instance? BigInteger n)
                                   (instance? Short n)
                                   (instance? Byte n))))})))


#_ ;FIXME reenable once ::integer?-poly is fixed
(deftest integer?-poly-test
  (tu/is-valid ::integer?-poly integer?)
  (tu/is-invalid ::integer?-poly symbol?)
  (tu/is-invalid ::integer?-poly (fn [x]
                                   #_(prn "x" x)
                                   (symbol? x))))
