(ns typed-test.clj.spec
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

(deftest tapp-test
  (tu/is-valid
    (tapp (tfn :binder (binder :x (bind-tv))
               :body (tv :x))
          {:x integer?})
        1)
  (tu/is-invalid
    (tapp (tfn :binder (binder :x (bind-tv))
               :body (tv :x))
          {:x integer?})
    nil))

(deftest fcase-test
  (is (= `(fcase
            #{0 1} (s/fspec :args (s/cat) :ret integer?)
            (s/fspec :args (s/cat :n integer?) :ret integer?))
         (s/form
           (fcase
             0 (s/fspec :args (s/cat) :ret integer?)
             1 (s/fspec :args (s/cat) :ret integer?)
             (s/fspec :args (s/cat :n integer?) :ret integer?)))))
  (tu/is-valid
    (s/fspec :args (s/cat) :ret integer?)
    (fn [] 1))
  (tu/is-valid
    (fcase
      0 (s/fspec :args (s/cat) :ret integer?))
    (fn [] 1))
  (tu/is-valid
    (fcase
      (s/fspec :args (s/cat) :ret integer?))
    (fn [] 1))
  (tu/is-valid
    (fcase
      (s/fspec :args (s/cat) :ret integer?))
    (fn [] 1))
  (tu/is-valid
    (fcase
      0 (s/fspec :args (s/cat) :ret integer?)
      (s/fspec :args (s/cat :n integer?) :ret integer?))
    (fn
      ([] 1)
      ([x] x)))
  (tu/is-valid
    (fcase
      0 (s/fspec :args (s/cat) :ret integer?)
      1 (s/fspec :args (s/cat :n integer?) :ret integer?))
    (fn
      ([] 1)
      ([x] x)))
  (tu/is-invalid
    (fcase
      0 (s/fspec :args (s/cat) :ret integer?)
      1 (s/fspec :args (s/cat :n integer?) :ret integer?))
    (fn
      ([x] x)))
  (tu/is-valid
    (fcase
      #{0 1} (s/fspec :args (s/cat :n (s/? integer?)) :ret integer?))
    (fn
      ([] 1)
      ([x] x)))
  (tu/is-invalid
    (fcase
      #{0 1} (s/fspec :args (s/cat :n (s/? integer?)) :ret integer?))
    (fn
      ([x] x)))
  (is (every?
        integer?
        ((juxt #(%) #(% 1) #(% 'a :b))
         (gen/generate
           (s/gen
             (fcase
               #{0 1} (s/fspec :args (s/cat :n (s/? integer?)) :ret integer?)
               (s/fspec :args (s/cat :s symbol? :k keyword?) :ret integer?)))))))
  )

(deftest bind-tv-test
  (is (= (s/conform (bind-tv) `integer?)
         `integer?))
  (tu/is-invalid
    (bind-tv :kind integer?)
    :a)
  (is (= (:clojure.spec.alpha/problems
           (s/explain-data (bind-tv :kind integer?)
                           :a))
         `[{:path [:kind], :pred integer?, :val :a, :via [bind-tv], :in []}])))

(comment
  (s/explain (s/* integer?) [1 :a])
  (s/explain (s/* integer?) [1 [:a] 2])
  (s/explain (s/resolve-spec `integer?) nil)
  (s/explain-data (s/resolve-spec `integer?) nil)
  (s/explain-data (s/tuple (s/tuple integer?)) [[nil]])
)

(deftest binder-test
  (is (= (s/conform (binder :x (bind-tv))
                    `{:x integer?})
         `{:x integer?}))
  (is (= (s/conform (binder :x (bind-tv))
                    `{:x integer?})
         `{:x integer?}))
  (is (= (s/conform (binder :x (bind-tv :kind integer?))
                    `{:x 1})
         `{:x 1}))
  (tu/is-invalid
    (binder :x (bind-tv :kind integer?))
    `{:x :b})
  (is (= (:clojure.spec.alpha/problems
           (s/explain-data (binder :x (bind-tv :kind integer?))
                           `{:x :b}))
         `[{:path [:x :kind]
            :pred integer?
            :val :b
            :via [binder bind-tv]
            :in []}])))

(deftest inst-test
  (is (=
       `(s/fspec :args (s/cat :x integer?)
                 :ret integer?)
       (s/form
         (inst (all :binder
                    (binder :x (bind-tv))
                    :body
                    (s/fspec :args (s/cat :x (tv :x))
                             :ret (tv :x)))
               {:x integer?}))))
  (is (=
       `(s/fspec :args (s/cat :x (s/cat :xs0 integer? :xs1 boolean?))
                 :ret (s/tuple (s/cat :xs0 integer? :xs1 boolean?)))
       (s/form
         (inst (all :binder
                    (binder :xs (bind-tv
                                  :kind (s/* (binder
                                               :x (bind-tv)))))
                    :body
                    (s/fspec :args (s/cat :x (fold-binders (tv :x) :xs))
                             :ret (s/tuple (fold-binders (tv :x) :xs))))
               {:xs [{:x integer?}
                     {:x boolean?}]}))))
  ;doesn't respect s/+
  (is (thrown-with-msg?
        clojure.lang.ExceptionInfo
        #"Invalid substitution"
        (inst (all :binder
             (binder :xs (bind-tv
                           :kind (s/+ (binder
                                        :x (bind-tv)))))
             :body
             (s/fspec :args (s/cat :x (fold-binders (tv :x) :xs))
                      :ret (s/tuple (fold-binders (tv :x) :xs))))
        {:xs []}))))

(deftest ^:deprecated reduced-of-test
  (is (every? #(and (reduced? %)
                    (integer? @%))
              (gen/sample
                (s/gen (t/reduced-of integer?)))))
  (tu/is-valid (t/reduced-of integer?) (reduced 1))
  (tu/is-invalid (t/reduced-of symbol?) (reduced 1))
  (tu/is-valid (s/or :reduced (t/reduced-of integer?)
                     :integer integer?)
               (reduced 1))
  (tu/is-invalid (s/or :reduced (t/reduced-of integer?)
                       :integer integer?)
                 (gensym))
  (tu/is-invalid (t/reduced-of integer?)
                 (gensym))
  (is (apply
        (every-pred
          reduced?
          (comp integer? deref))
        (gen/sample
          (s/gen (t/reduced-of integer?)))))
  )

(comment
(s/def ::coll-of-tfn
  (tfn {:x {:variance :covariant}}
       (s/coll-of (tv :x))))

(s/form ::coll-of-tfn)
(s/form (tapp ::coll-of-tfn {:x any?}))
(s/form (s/or))
(comment
  (s/explicate (ns-name *ns*) 'any?)
  (s/explicate (ns-name *ns*) '{:x any?})
  (s/explicate (ns-name *ns*) '(tapp ::coll-of-tfn {:x any?}))

(s/conform (tapp ::coll-of-tfn {:x any?}) [])
(s/conform (tapp ::coll-of-tfn {:x integer?}) [1])
(s/conform (tapp ::coll-of-tfn {:x integer?}) [nil])

(s/conform (tapp ::coll-of-tfn {:x integer?}) [nil])


(s/def ::symbol?
  (s/fspec
    :args (s/cat :x any?)
    :fn (fn [{{:keys [x]} :args :keys [ret]}]
          (= ret
             (instance? clojure.lang.Symbol x)))
    :ret boolean?))

(s/def ::keyword?
  (s/fspec
    :args (s/cat :x any?)
    :fn (fn [{{:keys [x]} :args :keys [ret]}]
          (= ret
             (instance? clojure.lang.Keyword x)))
    :ret boolean?))

(s/def ::get-a
  (s/fspec
    :args (s/cat :m any?)
    :fn (fn [{{:keys [m]} :args :keys [ret]}]
          (= ret
             (:a m)))
    :ret any?))

;clojure.core/gensym (IFn [-> t/Symbol]
;                        [(U t/Symbol String) -> t/Symbol])

(s/def ::gensym
  (s/and- (s/fspec :ret symbol?)
          (s/fspec :args (s/cat :sym-or-str (s/or symbol? string?))
                   :ret symbol?)))


;clojure.core/sorted-set (All [x] [x * -> (PersistentTreeSet x)])

#_
(s/def ::sorted-set
  (poly {:x {:variance :invariant}}
        (s/fspec :args (s/* (tv :x))
                 :ret (tapp clojure.lang.PersistentTreeSet {:x (tv :x)}))))

;clojure.core/list* (All [x] 
;                        (IFn [(U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]))

(s/def ::nilable-non-empty-aseq
  (tfn {:x {:variance :covariant}}
       (s/and- (s/nilable (coll-of (tv :x)))
               seq)))

#_
(s/def ::list*
  (poly {:x {:variance :invariant}}
        (s/fspec :args (s/cat :prefix (s/* (tv :x))
                              :list (s/nilable (s/coll-of (tv :x))))
                 :ret (tapp ::nilable-nonempty-aseq (tv :x)))))

;clojure.core/vector (All [r b :..]
;                         (IFn [b :.. b -> '[b :.. b]]
;                              [r * -> (t/AVec r)]))

#_
(s/def ::AVec
  (tapp {:els {:variance :covariant}}
        (s/and- (s/coll-of (tv :els) :into []))
        ))

#_
(s/def ::vector
  (poly {:r {:variance :invariant}
         :b {:dotted true
             :variance :invariant}}
        (s/and-
          (s/fspec :args (s/cat :els (s/* (... (tv :r) (tv :r))))
                   :ret (hvec :dotted (... (tv :r) (tv :r))))
          (s/fspec :args (s/cat :prefix (s/* (tv :r)))
                   :ret (tapp ::AVec {:els (tv :r)})))))

#_
(t/defn
  :poly {:x {:variance :invariant}}
  vector?
  [& els]
  (tconform (tv :x)
            ))

  )

#_
(s/def ::Associative
  (tfn [:allowed-keys (bind-tv)
        :allowed-vals (bind-tv)
        :keys (bind-tv :upper (tv :allowed-keys))
        :vals (bind-tv :upper (tv :allowed-vals))
        :seq (bind-tv)]
       (jvmclass clojure.lang.Associative
                 :allowed-keys (tv :allowed-keys)
                 :allowed-vals (tv :allowed-vals)
                 :keys (tv :keys)
                 :vals (tv :vals)
                 :seq (tv :seq))))

#_
(s/def ::Map
  (tfn [:keys (bind-tv)
        :vals (bind-tv)]
       (jvmclass clojure.lang.IPersistentMap
                 :keys (tv :keys)
                 :vals (tv :vals))))

#_
(jvmclass-ancestors
  clojure.lang.IPersistentMap
  [:keys (bind-tv :variance :covariant)
   :vals (bind-tv :variance :covariant)]
  :replace {clojure.lang.IPersistentCollection
            (jvmclass clojure.lang.IPersistentCollection
                      :key (clojure.lang.AMapEntry
                             :key (tv :keys)
                             :val (tv :vals)))

            Iterable
            (jvmclass Iterable 
                      :key (clojure.lang.AMapEntry
                             :key (tv :keys)
                             :val (tv :vals)))

            clojure.lang.Seqable
            (jvmclass clojure.lang.Seqable 
                      :key (clojure.lang.AMapEntry
                             :key (tv :keys)
                             :val (tv :vals)))

            clojure.lang.Associative
            (jvmclass clojure.lang.Associative 
                      :keys (tv :keys)
                      :vals (tv :vals)
                      :seq (clojure.lang.AMapEntry
                             :key (tv :keys)
                             :val (tv :vals)))})

#_
(s/def clojure.core/assoc
  (all [:m (bind-tv :upper (tapp ::Associative))
        :kvs (bind-tv :dotted true
                        :binder [:kvs/k (bind-tv :upper (Match [:out]
                                                            (tv :m)
                                                            (Associative :allowed-keys (match-tv :out))))
                                 :kvs/v (bind-tv)])]
       (s/fspec :args (s/cat :m (tv :m)
                             :kvs (s/+
                                    (tv-pretype (s/cat (tv :kvs/k) (tv :kvs/v))
                                                  :kvs)))
                :ret (Assoc (tv :m) (s/+
                                        (tv-pretype (s/cat (tv :kvs/k) (tv :kvs/v))
                                                      :kvs))))))

#_
(s/def clojure.core/update
  (all [:m (tv :upper associative?)
        :k (tv)
        :v (tv)
        :z (tv :dotted true)]
       (s/fspec :args (s/cat :m (tv :m)
                             :k (tv :k)
                             :f (s/fspec :args (s/cat :v (Get (tv :m) (tv :k))
                                                      :rest (tv... (tv :z) :z))
                                         :ret (tv :v))
                             :rest (tv... (tv :z) :z))
                :ret (Assoc (tv :m) (tv :k)  (tv :v)))))

#_
(s/def ::get
  (all [:m (tv :upper associative?)
        :k (tv)
        :default (tv)]
       (s/fspec :args (s/cat :m (tv :m)
                             :k (tv :k)
                             :default (s/? (tv :default)))
                :ret (Get (tv :m) (tv :k) (s/? (tv :default))))))
)
