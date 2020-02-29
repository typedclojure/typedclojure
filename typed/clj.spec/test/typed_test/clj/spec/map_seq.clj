; tests for non xducer arities of clojure.core/map
(ns typed-test.clj.spec.map-seq
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
  ::map1-mono
  (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x any?)
                                     :ret any?)
                        :coll (s/every any?))
           :ret (s/every any?)))

(deftest map1-vanilla-test
  (is (s/valid? ::map1-mono clojure.core/map))
  (is (s/valid? ::map1-mono (comp #(map str %) map))))

(s/def
  ::map1
  (all :binder (binder
                 :x (tvar-spec)
                 :y (tvar-spec))
       :body (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (tvar :x))
                                                :ret (tvar :y))
                                   :coll (s/coll-of (tvar :x)))
                      :ret (s/coll-of (tvar :y)))))

(deftest map1-test
  (is (s/valid? ::map1 map))
  (is (not (s/valid? ::map1 (comp #(map str %) map))))
  (is (not (s/valid? ::map1 (comp next map))))
  ; non-dependent spec, these should validate
  (is (s/valid? ::map1 (comp rest map)))
  (is (s/valid? ::map1 (comp reverse map))))

(s/def
  ::mapN
  (all :binder (binder
                 :x (tvar-spec :kind (s/+ (binder
                                            :x (tvar-spec))))
                 :y (tvar-spec))
       :body
       (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :xs (dotted-pretype (tvar :x)
                                                                           :x))
                                          :ret (tvar :y))
                             :colls (dotted-pretype (s/coll-of (tvar :x))
                                                    :x))
                :ret (s/coll-of (tvar :y)))))

(deftest mapN-test
  (is (s/valid? ::mapN map))
  (is (not (s/valid? ::mapN (comp #(map str %) map))))
  (is (not (s/valid? ::mapN (comp next map))))
  ; non-dependent spec, these should validate
  (is (s/valid? ::mapN (comp rest map)))
  (is (s/valid? ::mapN (comp reverse map))))

(s/def
  ::map1-dependent
  (all :binder (binder
                 :N (tvar-spec :kind nat-int?
                               :gen #(gen/large-integer
                                       {:min 0
                                        :max 100}))
                 :x (tvar-spec :kind ::t/any-spec?)
                 :y (tvar-spec :kind ::t/any-spec?))
       :body
       (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (tvar :x))
                                          :ret (tvar :y))
                             :colls (s/every (tvar :x)
                                             :count (tvar :N)))
                :ret (s/every (tvar :y)
                              :count (tvar :N)))))

(deftest map1-dependent-test
  (is (s/valid? ::map1-dependent map))
  (is (not (s/valid? ::map1-dependent (comp next map))))
  (is (not (s/valid? ::map1-dependent (comp rest map))))
  ; order of return values is not part of ::map1-dependent spec
  (is (s/valid? ::map1-dependent (comp reverse map))))

(s/def
  ::mapN-dependent
  (all :binder (binder
                 :x (tvar-spec :kind (s/+ (binder
                                            :N (tvar-spec :kind (s/with-gen
                                                                  nat-int?
                                                                  #(gen/choose 0 100)))
                                            :x (tvar-spec))))
                 :y (tvar-spec))
       :body
       (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :xs (dotted-pretype (tvar :x) :x))
                                          :ret (tvar :y))
                             :colls (dotted-pretype
                                      (s/every (tvar :x)
                                               :count (tvar :N))
                                      :x))
                :ret (s/every (tvar :y)
                              :count (dotted-pretype (tvar :N) :x
                                                     :wrap #(apply min %))))))

(deftest mapN-dependent-spec
  (is (s/valid? ::mapN-dependent map))
  (is (not (s/valid? ::mapN-dependent (comp next map))))
  (is (not (s/valid? ::mapN-dependent (comp rest map))))
  ;missing coll arities
  (is (not (s/valid? ::mapN-dependent (fn [f coll] (map f coll)))))
  ;coll always provided
  (is (s/valid? ::mapN-dependent (fn [f & colls]
                                   {:pre [colls]}
                                   (apply map f colls))))
  (is (not (s/valid? ::mapN-dependent
                     (fn [& args]
                       #_(prn "args" args)
                       (next (apply map args)))))))

(comment
(s/describe (inst ::mapN-dependent {:x [{}]}))
(gen/generate
  (s/gen
    (binder
      :x (tvar-spec :kind (s/+ (binder
                                 :N (tvar-spec :kind (s/with-gen
                                                       nat-int?
                                                       #(gen/choose 0 100)))
                                 :x (tvar-spec))))
      :y (tvar-spec))))
(gen/sample
  (s/gen
    (s/with-gen
      nat-int?
      #(gen/choose 0 100)))
  1000)
)
