(ns ^:typed.clojure typed-test.clj.spec.map
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [clojure.test :refer :all]
            [typed.clj.spec.test-utils :as tu]
            [typed-test.clj.spec.transducers :as x]))

(s/def
  ::map1-mono
  (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x any?)
                                     :ret any?)
                        :coll (s/every any?))
           :ret (s/every any?)))

(deftest map1-vanilla-test
  (tu/is-valid ::map1-mono clojure.core/map)
  (tu/is-valid ::map1-mono (comp #(map str %) map)))

(s/def
  ::map1-fspec-fn
  (s/fspec :args (s/cat :f (s/fspec :args (s/cat :x any?)
                                    :ret any?)
                        :coll (s/every any?))
           :fn (fn [{{:keys [f coll]} :args :keys [ret]}]
                 (let [rets (set ret)]
                   (prn "rets" rets (map f coll))
                   (every? rets (map f coll))))
           :ret (s/coll-of any?)))

#_
(deftest map1-fspec-fn-test
  (is (s/explain ::map1-fspec-fn clojure.core/map))
  (tu/is-valid ::map1-fspec-fn clojure.core/map)
  (tu/is-valid ::map1-fspec-fn (comp #(map str %) map)))

(s/def
  ::map1
  (all :binder (binder
                 :x (bind-tv)
                 :y (bind-tv))
       :body (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (tv :x))
                                                :ret (tv :y))
                                   :coll (s/coll-of (tv :x)))
                      :ret (s/coll-of (tv :y)))))

(deftest map1-test
  (tu/is-valid ::map1 map)
  (tu/is-invalid ::map1 (comp #(map str %) map))
  (tu/is-invalid ::map1 (comp next map))
  ; non-dependent spec, these should validate
  (tu/is-valid ::map1 (comp rest map))
  (tu/is-valid ::map1 (comp reverse map)))

(s/def
  ::mapN
  (all :binder (binder
                 :x (bind-tv :kind (s/+ (binder :x (bind-tv))))
                 :y (bind-tv))
       :body
       (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :xs (fold-binders (tv :x) :x))
                                          :ret (tv :y))
                             :colls (fold-binders (s/coll-of (tv :x)) :x))
                :ret (s/coll-of (tv :y)))))

(deftest mapN-test
  (tu/is-valid ::mapN map)
  (tu/is-invalid ::mapN (comp #(map str %) map))
  (tu/is-invalid ::mapN (comp next map))
  ; non-dependent spec, these should validate
  (tu/is-valid ::mapN (comp rest map))
  (tu/is-valid ::mapN (comp reverse map)))

(s/def
  ::map1-dependent
  (all :binder (binder
                 :N (bind-tv :kind (s/with-gen
                                     nat-int?
                                     #(gen/large-integer
                                        {:min 0
                                         :max 100})))
                 :x (bind-tv)
                 :y (bind-tv))
       :body
       (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (tv :x))
                                          :ret (tv :y))
                             :colls (s/every (tv :x)
                                             :count (tv :N)))
                :ret (s/every (tv :y)
                              :count (tv :N)))))

(deftest map1-dependent-test
  (tu/is-valid ::map1-dependent map)
  (tu/is-invalid ::map1-dependent (comp next map))
  (tu/is-invalid ::map1-dependent (comp rest map))
  ; order of return values is not part of ::map1-dependent spec
  (tu/is-valid ::map1-dependent (comp reverse map)))

(s/def
  ::mapN-dependent
  (all :binder (binder
                 :x (bind-tv :kind (s/+ (binder
                                          :N (bind-tv :kind (s/with-gen
                                                              nat-int?
                                                              #(gen/one-of
                                                                 [(gen/choose 0 100)
                                                                  (gen/return ##Inf)])))
                                          :x (bind-tv))))
                 :y (bind-tv))
       :body
       (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :xs (fold-binders (tv :x) :x))
                                          :ret (tv :y))
                             :colls (fold-binders
                                      (s/every (tv :x)
                                               :count (tv :N))
                                      :x))
                :ret (s/every (tv :y)
                              :count (fold-binders (tv :N) :x
                                                   :wrap #(apply min %))))))

;related to CLJ-2561, shortcoming of s/every
(deftest mapN-dependent-spec
  (is (= `(s/fspec
            :args
            (s/cat :fn (s/fspec :args (s/cat :xs (s/cat :x0 boolean?))
                                :ret integer?)
                   :colls (s/cat :x0 (s/every boolean? :count 1)))
            :ret
            (s/every integer? :count 1))
         (s/form (inst ::mapN-dependent {:y integer?
                                         :x [{:x boolean?
                                              :N 1}]}))))
  (tu/is-valid ::mapN-dependent map)
  ;not sensitive to order of return collection
  (tu/is-valid ::mapN-dependent (comp reverse map))
  (tu/is-invalid ::mapN-dependent (comp next map))
  (tu/is-invalid ::mapN-dependent (comp rest map))
  ;missing coll arities
  (tu/is-invalid ::mapN-dependent (fn [f coll] (map f coll)))
  ;coll always provided
  (tu/is-valid ::mapN-dependent (fn [f & colls]
                                  {:pre [colls]}
                                  (apply map f colls)))
  ;sensitive to return collection count
  (tu/is-invalid ::mapN-dependent
                 (fn [& args]
                   #_(prn "args" args)
                   (next (apply map args)))))

(comment
  (s/describe (inst ::mapN-dependent {:x [{}]}))
  (gen/generate
    (s/gen
      (binder
        :x (bind-tv :kind (s/+ (binder
                                 :N (bind-tv :kind (s/with-gen
                                                     nat-int?
                                                     #(gen/choose 0 100)))
                                 :x (bind-tv))))
        :y (bind-tv))))
  (gen/sample
    (s/gen
      (s/with-gen
        nat-int?
        #(gen/choose 0 100)))
    1000)
  )

(s/def ::mapT
  (all :binder
       (binder
         :in (bind-tv)
         :out (bind-tv))
       :body
       (s/fspec
         :args (s/cat :f (s/fspec :args (s/cat :in (tv :in))
                                  :ret (tv :out)))
         :ret (tapp ::x/Transducer {:in (tv :in)
                                    :out (tv :out)}))))

(deftest mapT-test
  (binding [s/*fspec-iterations* 2]
    (tu/is-valid ::mapT map)))

(s/def
  ::mapTN-dependent
  (all :binder (binder
                 :in (bind-tv)
                 :out (bind-tv)
                 :x (bind-tv :kind (s/+ (binder
                                          :N (bind-tv :kind (s/with-gen
                                                              nat-int?
                                                              #(gen/choose 0 100)))
                                          :x (bind-tv))))
                 :y (bind-tv))
       :body
       (fcase
         1 (s/fspec
             :args (s/cat :f (s/fspec :args (s/cat :in (tv :in))
                                      :ret (tv :out)))
             :ret (tapp ::x/Transducer {:in (tv :in)
                                        :out (tv :out)}))
         (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :xs (fold-binders (tv :x) :x))
                                            :ret (tv :y))
                               :colls (fold-binders
                                        (s/every (tv :x)
                                                 :count (tv :N))
                                        :x))
                  :ret (s/every (tv :y)
                                :count (fold-binders (tv :N) :x
                                                     :wrap #(apply min %)))))))

(deftest mapTN-dependent-test
  (binding [s/*fspec-iterations* 2]
    (tu/is-valid ::mapTN-dependent map))
  (tu/is-invalid ::mapTN-dependent (comp next map))
  (tu/is-invalid ::mapTN-dependent (comp rest map))
  ;missing coll arities
  (tu/is-invalid ::mapTN-dependent (fn [f coll] (map f coll)))
  ;missing xducer arity
  (tu/is-invalid ::mapTN-dependent (fn [f coll & colls] (apply map f coll colls)))
  (tu/is-invalid ::mapTN-dependent
                 (fn [& args]
                   #_(prn "args" args)
                   (next (apply map args)))))

(comment
  (gen/generate
    (s/gen
      (s/every (s/* integer?))))
  (gen/generate
  (s/gen
    (s/tuple (s/* integer?))))
)

(defn wrap-fn-ret-els-het-fn-args-els [i [tag types]]
  {:pre [(number? i)]}
  (case tag
    ; generalize if ith element is not available
    :hom types
    :het (nth types i)))

(defn wrap-fn-ret-els-hom-args [els [tag types]]
  {:pre [(simple-keyword? els)]}
  (case tag
    :hom types
    :het `(s/or
            ~@(mapcat
                (fn [i t]
                  [(keyword nil (str (name els) i))
                   t])
                (range)
                ; smaller expansion
                (set types)))))

(comment
  (wrap-fn-ret-els-hom-args :els `[:hom integer?])
  (wrap-fn-ret-els-hom-args :els `[:het [integer? boolean?]])
  )

(defn het-or-hom-fspec [colls els [tag types]]
  {:pre [((every-pred simple-keyword?) colls els)]}
  ;FIXME what if the sequence of _inputs_ is infinite?
  (case tag
    ; if the output is infinite, we can combine all arities for :fn 
    ; by unioning arguments pairwise
    :hom
    `(s/fspec
       :args
       (s/cat
         ~els
         ; takes the same number of arguments as there are colls
         (fold-binders
           ; each argument `i` accepts any element of ith input collection
           (tv
             ~els
             :wrap #(wrap-fn-ret-els-hom-args ~els %))
           ~colls))
       ; each invocation of :fn should return some element of
       ; the return collection
       :ret ~types)

    ; if the output collection is heterogeneous (and finite), generate
    ; a sufficiently large function intersection to test all cases
    :het
    `(s/and #_fand ;TODO fand
            ~@(map
                (fn [i t]
                  {:pre [(number? i)]}
                  `(s/fspec
                     ; accepts only the ith element of each colls
                     :args
                     (s/cat
                       ~els
                       (fold-binders
                         (tv
                           ~els
                           :wrap #(wrap-fn-ret-els-het-fn-args-els ~i %))
                         ~colls))
                     ; returns only the ith element of :ret-els
                     :ret ~t))
                (range)
                types))))

(comment
  (wrap-fn-ret-els :colls :els `[:hom integer?])
  (wrap-fn-ret-els :colls :els `[:het [integer?]])
  )

(defn het-or-hom-arg-coll [[tag types]]
  (case tag
    :hom `(s/every ~types)
    :het `(s/spec
            (s/cat
              ~@(mapcat (fn [i t]
                          [(keyword (str "els" i))
                           t])
                        (range)
                        types)))))

(comment
; TODO related to CLJ-2562
; cat spec round trip
(s/form (s/resolve-spec `(s/spec (s/cat :a integer?))))
(s/form (s/resolve-spec `(s/spec integer?)))
(s/form (s/resolve-spec `(s/spec ::map1-mono)))
;(clojure.alpha.spec/cat :a clojure.core/integer?)

(s/resolve-spec (het-or-hom-arg-coll `[:hom integer?]))
(s/resolve-spec (het-or-hom-arg-coll `[:het [integer? boolean?]]))
(s/form (s/resolve-spec `(s/spec integer?)))
(s/form (s/spec integer?))
)

(defn tagged-het-or-hom-coll-spec [N]
  {:pre [((some-fn #{##Inf} nat-int?) N)]}
  (if (#{##Inf} N)
    `(s/tuple
       #{:hom}
       (bind-tv))
    `(s/tuple
       #{:het}
       (s/coll-of
         (bind-tv)
         :count ~N
         :into []))))

(comment
  (tagged-het-or-hom-coll-spec ##Inf)
  (tagged-het-or-hom-coll-spec 10)
  )


(defn reduce-het-or-hom-coll-spec [Ns]
  {:pre [(vector? Ns)
         (every? number? Ns)]}
  (tagged-het-or-hom-coll-spec (apply min Ns)))

(defn coerce-dotted-cat [tv xs]
  {:pre [(simple-keyword? tv)]}
  ;(prn "coerce-dotted-cat" tv xs)
  `(s/cat ~@(mapcat (fn [i x]
                      [(keyword (str (name tv) i)) x])
                    (range)
                    xs)))

(defn hom-or-ret-sequence-spec [tv [tag types]]
  {:pre [(simple-keyword? tv)]}
  (case tag
    :hom `(s/every ~types)
    :het `(s/spec ~(coerce-dotted-cat tv types))))

(s/def ::maybe-inf-count
  #_"Represents the `count` of a potentially infinite collection."
  (s/nonconforming
    (s/or :finite
          (s/with-gen
            nat-int?
            #(gen/choose 0 100))
          :infinite
          #{##Inf})))

(s/def
  ::mapN-dependent-het
  #_""
  (all :binder (binder
                 :colls (bind-tv :kind (s/+ (binder
                                              :N (bind-tv :kind ::maybe-inf-count)
                                              :els (bind-tv :kind (tv :N :wrap tagged-het-or-hom-coll-spec)))))
                 :ret-els (bind-tv :kind (fold-binders (tv :N) :colls :wrap reduce-het-or-hom-coll-spec)))
       :body
       (s/fspec :args (s/cat :fn (tv :ret-els :wrap #(het-or-hom-fspec :colls :els %))
                             :colls (fold-binders (tv :els :wrap het-or-hom-arg-coll) :colls))
                :ret (tv :ret-els :wrap #(hom-or-ret-sequence-spec :ret-els %)))))

(comment
(s/describe (inst ::mapN-dependent-het {:colls [{:N 1}]}))
(s/describe (inst ::mapN-dependent-het {:colls [{:N ##Inf}]}))
;TODO debug this
(s/describe (inst ::mapN-dependent-het {:colls [{:N 1
                                                 :els [:het [integer?]]}]}))
(s/describe (inst ::mapN-dependent-het {:colls [{:N ##Inf
                                                 :els [:het [integer?]]}]}))
(s/describe (inst ::mapN-dependent-het {}))
(gen/generate
  (s/gen
    (s/or :finite nat-int?
          :infinite #{##Inf})))
(gen/sample
  (s/gen
    (bind-tv
      :kind (s/or :finite nat-int?
                  :infinite #{##Inf}))))
(s/valid?
  (bind-tv
    :kind (s/or :finite nat-int?
                :infinite #{##Inf}))
  ##Inf)
(s/valid?
  (bind-tv
    :kind (s/or :finite nat-int?
                :infinite #{##Inf}))
  2)
(s/conform
  (s/or :finite nat-int?
        :infinite #{##Inf})
  1)
(s/conform
  (s/or :integer? integer?)
  1)
(s/conform
  (s/or :integer? integer?)
  1)
)

;; FIXME
#_
(deftest mapN-dependent-het-test
  (is (binding [s/*fspec-iterations* 2]
        (s/conform ::mapN-dependent-het map))))
