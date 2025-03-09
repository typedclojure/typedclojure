(ns ^:typed.clojure typed-test.clj.spec.reduce
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :as t]
            [clojure.test :refer :all]
            [typed.clj.spec.test-utils :as tu]
            [typed-test.clj.spec.transducers :as x]))

(s/def ::reduce3
  (t/all :binder
         (t/binder
           :acc (t/bind-tv)
           :x (t/bind-tv))
         :body
         (s/fspec :args (s/cat :f (s/fspec :args (s/cat :acc (t/tv :acc)
                                                        :x (t/tv :x))
                                           :ret (s/or :reduced (t/reduced-of (t/tv :acc))
                                                      :not-reduced (t/tv :acc)))
                               :acc (t/tv :acc)
                               :xs (s/every (t/tv :x)))
                  :ret (t/tv :acc))))

(deftest reduce3-test
  (is (s/describe (t/inst ::reduce3 {:acc integer? :x integer?})))
  (tu/is-valid (t/inst ::reduce3 {:acc integer? :x integer?}) reduce)
  (tu/is-valid ::reduce3 reduce)
  )

(s/def ::reduce2
  (t/all :binder
         (t/binder
           :acc (t/bind-tv)
           :x (t/bind-tv))
         :body
         (s/fspec :args (s/cat :f (t/fcase
                                    0
                                    (s/fspec :args (s/cat)
                                             :ret (t/tv :acc))
                                    2
                                    (s/fspec :args (s/cat :acc (s/or :acc (t/tv :acc)
                                                                     :init (t/tv :x))
                                                          :x (t/tv :x))
                                             :ret (s/or :reduced (t/reduced-of (t/tv :acc))
                                                        :not-reduced (t/tv :acc))))
                               :xs (s/every (t/tv :x)))
                  :ret (s/or :acc (t/tv :acc)
                             :x (t/tv :x)))))

(deftest reduce2-test
  (is (s/describe (t/inst ::reduce2 {:acc integer? :x integer?})))
  (tu/is-valid (t/inst ::reduce2 {:acc integer? :x integer?}) reduce)
  (tu/is-valid (t/inst ::reduce2 {:acc number? :x integer?}) reduce)
  (tu/is-valid ::reduce2 reduce)
  )

(s/def ::reduce
  (t/all :binder
         (t/binder
           :acc (t/bind-tv)
           :x (t/bind-tv))
         :body
         (t/fcase
           2
           (t/inst ::reduce2 {:acc (t/tv :acc)
                              :x (t/tv :x)})
           3
           (t/inst ::reduce3 {:acc (t/tv :acc)
                              :x (t/tv :x)}))))

(deftest reduce-test
  (is (s/describe (t/inst ::reduce {:acc integer? :x integer?})))
  (tu/is-valid (t/inst ::reduce {:acc integer? :x integer?}) reduce)
  (tu/is-valid (t/inst ::reduce {:acc number? :x integer?}) reduce)
  (tu/is-valid (t/inst ::reduce {:acc (s/tuple number?) :x integer?}) reduce)
  (binding [s/*fspec-iterations* 10]
    (tu/is-valid ::reduce reduce))
  (binding [s/*fspec-iterations* 10]
    (tu/is-valid ::reduce (fn
                            ([f coll] (reduce (fn
                                                ([] (f))
                                                ([acc x]
                                                 (prn {:acc acc :x x})
                                                 (f acc x)))
                                              coll))
                            ([f init coll] (reduce (fn [acc x]
                                                     #_(prn {:acc acc :x x})
                                                     (f acc x))
                                                   init coll)))))
  )
