(ns typed-test.clj.spec.integer-huh
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

(s/def ::integer?-fspec-fn
  (s/fspec
    :args (s/cat :n (s/with-gen
                      any?
                      #(gen/one-of
                         [(s/gen (s/resolve-spec `integer?))
                          (s/gen (s/resolve-spec `any?))])))
    :fn (fn [{{:keys [n]} :args :keys [ret]}]
          (= ret
             (or (instance? Integer n)
                 (instance? Long n)
                 (instance? clojure.lang.BigInt n)
                 (instance? BigInteger n)
                 (instance? Short n)
                 (instance? Byte n))))
    :ret boolean?))

(deftest integer?-fspec-fn-test
  (is (s/valid? ::integer?-fspec-fn integer?))
  (is (not (s/valid? ::integer?-fspec-fn symbol?))))

(s/def ::integer?-poly
  (all :binder
       (binder
         :x (tvar-spec :kind ::t/any-spec?))
       :body
       (s/fspec :args (s/cat :x (tvar :x))
                :ret #{(tvar :x
                             :wrap
                             (fn [n]
                               (or (instance? Integer n)
                                   (instance? Long n)
                                   (instance? clojure.lang.BigInt n)
                                   (instance? BigInteger n)
                                   (instance? Short n)
                                   (instance? Byte n))))})))

;FIXME very poor test coverage due to ::t/any-spec?'s generator
; only generating gensyms
(deftest integer?-poly-test
  (is (s/valid? ::integer?-poly integer?))
  (is (not (s/valid? ::integer?-poly symbol?)))
  (is (not (s/valid? ::integer?-poly (fn [x]
                                       (prn "x" x)
                                       (symbol? x))))))
