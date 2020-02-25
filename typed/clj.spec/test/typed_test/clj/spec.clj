(ns typed-test.clj.spec
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [typed.clj.spec :refer :all]
            [clojure.test :refer :all]))

(deftest prewalk-test
  (is (= 2
         (prewalk (fn [config form]
                    {:config config
                     :form (inc form)})
                  {} 1)))
  (is (= [2 [42 4]]
         (prewalk (fn [config form]
                    {:config config
                     :form (if (integer? form)
                             (if (= 2 form)
                               42
                               (inc form))
                             form)})
                  {}
                  [1 [2 3]]))))

(deftest subst-tvar-test
  (is (= 1
         (subst-tvar `(tvar :x)
                     {:x 1})))
  (is (= `(vector 1)
         (subst-tvar `(vector (tvar :x))
                     {:x 1}))))

(comment
(s/def ::coll-of-tfn
  (tfn {:x {:variance :covariant}}
       (s/coll-of (tvar :x))))

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

(s/def ::poly-map
  (all [:a {:variance :invariant}
        :b {:variance :invariant}]
       (s/fspec
         :args (s/cat :fn (s/fspec :args (s/cat :a (tvar :a))
                                   :ret (tvar :b))
                      :coll (s/coll-of (tvar :a)))
         :ret (s/coll-of (tvar :b)))))

(s/describe (inst ::poly-map {:a int? :b boolean?}))

(s/def ::integer?
  (s/fspec
    :args (s/cat :n any?)
    :fn (fn [{{:keys [n]} :args :keys [ret]}]
          (= ret
             (or (instance? Integer n)
                 (instance? Long n)
                 (instance? clojure.lang.BigInt n)
                 (instance? BigInteger n)
                 (instance? Short n)
                 (instance? Byte n))))
    :ret boolean?))

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

(s/def ::identity
  (s/fspec
    :args (s/cat :x any?)
    :fn (fn [{{:keys [x]} :args :keys [ret]}]
          (identical? ret x))
    :ret any?))

;clojure.core/gensym (IFn [-> t/Symbol]
;                        [(U t/Symbol String) -> t/Symbol])

(s/def ::gensym
  (s/and- (s/fspec :ret symbol?)
          (s/fspec :args (s/cat :sym-or-str (s/or symbol? string?))
                   :ret symbol?)))

;clojure.core/memoize (All [x y ...]
;                            [[y ... y -> x] -> [y ... y -> x]])

#_
(s/def ::memoize
  (poly {:x {:variance :invariant}
         :y {:dotted true
             :variance :invariant}}
        (s/fspec :args (s/cat :fn
                              (s/fspec :args
                                       (s/* (... (t/tvar :y) (t/tvar :y)))
                                       :ret
                                       (t/tvar :x)))
                 :ret (s/fspec :args (s/* (... (t/tvar :y) (t/tvar :y)))
                               :ret (t/tvar :x)))))

;clojure.core/sorted-set (All [x] [x * -> (PersistentTreeSet x)])

#_
(s/def ::sorted-set
  (poly {:x {:variance :invariant}}
        (s/fspec :args (s/* (tvar :x))
                 :ret (tapp clojure.lang.PersistentTreeSet {:x (tvar :x)}))))

;clojure.core/list* (All [x] 
;                        (IFn [(U nil (Seqable x)) -> (t/NilableNonEmptyASeq x)]

(s/def ::nilable-non-empty-aseq
  (tfn {:x {:variance :covariant}}
       (s/and- (s/nilable (coll-of (tvar :x)))
               seq)))

#_
(s/def ::list*
  (poly {:x {:variance :invariant}}
        (s/fspec :args (s/cat :prefix (s/* (tvar :x))
                              :list (s/nilable (s/coll-of (tvar :x))))
                 :ret (tapp ::nilable-nonempty-aseq (tvar :x)))))

;clojure.core/vector (All [r b ...]
;                         (IFn [b ... b -> '[b ... b]]
;                              [r * -> (t/AVec r)]))

#_
(s/def ::AVec
  (tapp {:els {:variance :covariant}}
        (s/and- (s/coll-of (tvar :els) :into []))
        ))

#_
(s/def ::vector
  (poly {:r {:variance :invariant}
         :b {:dotted true
             :variance :invariant}}
        (s/and-
          (s/fspec :args (s/cat :els (s/* (... (tvar :r) (tvar :r))))
                   :ret (hvec :dotted (... (tvar :r) (tvar :r))))
          (s/fspec :args (s/cat :prefix (s/* (tvar :r)))
                   :ret (tapp ::AVec {:els (tvar :r)})))))

#_
(t/defn
  :poly {:x {:variance :invariant}}
  vector?
  [& els]
  (tconform (tvar :x)
            ))

  )

#_
(s/def ::Associative
  (tfn [:allowed-keys (tvar-spec)
        :allowed-vals (tvar-spec)
        :keys (tvar-spec :upper (tvar :allowed-keys))
        :vals (tvar-spec :upper (tvar :allowed-vals))
        :seq (tvar-spec)]
       (jvmclass clojure.lang.Associative
                 :allowed-keys (tvar :allowed-keys)
                 :allowed-vals (tvar :allowed-vals)
                 :keys (tvar :keys)
                 :vals (tvar :vals)
                 :seq (tvar :seq))))

#_
(s/def ::Map
  (tfn [:keys (tvar-spec)
        :vals (tvar-spec)]
       (jvmclass clojure.lang.IPersistentMap
                 :keys (tvar :keys)
                 :vals (tvar :vals))))

#_
(jvmclass-ancestors
  clojure.lang.IPersistentMap
  [:keys (tvar-spec :variance :covariant)
   :vals (tvar-spec :variance :covariant)]
  :replace {clojure.lang.IPersistentCollection
            (jvmclass clojure.lang.IPersistentCollection
                      :key (clojure.lang.AMapEntry
                             :key (tvar :keys)
                             :val (tvar :vals)))

            Iterable
            (jvmclass Iterable 
                      :key (clojure.lang.AMapEntry
                             :key (tvar :keys)
                             :val (tvar :vals)))

            clojure.lang.Seqable
            (jvmclass clojure.lang.Seqable 
                      :key (clojure.lang.AMapEntry
                             :key (tvar :keys)
                             :val (tvar :vals)))

            clojure.lang.Associative
            (jvmclass clojure.lang.Associative 
                      :keys (tvar :keys)
                      :vals (tvar :vals)
                      :seq (clojure.lang.AMapEntry
                             :key (tvar :keys)
                             :val (tvar :vals)))})

#_
(s/def clojure.core/assoc
  (all [:m (tvar-spec :upper (tapp ::Associative))
        :kvs (tvar-spec :dotted true
                        :binder [:kvs/k (tvar-spec :upper (Match [:out]
                                                            (tvar :m)
                                                            (Associative :allowed-keys (match-tvar :out))))
                                 :kvs/v (tvar-spec)])]
       (s/fspec :args (s/cat :m (tvar :m)
                             :kvs (s/+
                                    (tvar-pretype (s/cat (tvar :kvs/k) (tvar :kvs/v))
                                                  :kvs)))
                :ret (Assoc (tvar :m) (s/+
                                        (tvar-pretype (s/cat (tvar :kvs/k) (tvar :kvs/v))
                                                      :kvs))))))

#_
(s/def clojure.core/update
  (all [:m (tvar :upper associative?)
        :k (tvar)
        :v (tvar)
        :z (tvar :dotted true)]
       (s/fspec :args (s/cat :m (tvar :m)
                             :k (tvar :k)
                             :f (s/fspec :args (s/cat :v (Get (tvar :m) (tvar :k))
                                                      :rest (tvar... (tvar :z) :z))
                                         :ret (tvar :v))
                             :rest (tvar... (tvar :z) :z))
                :ret (Assoc (tvar :m) (tvar :k)  (tvar :v)))))

#_
(s/def ::get
  (all [:m (tvar :upper associative?)
        :k (tvar)
        :default (tvar)]
       (s/fspec :args (s/cat :m (tvar :m)
                             :k (tvar :k)
                             :default (s/? (tvar :default)))
                :ret (Get (tvar :m) (tvar :k) (s/? (tvar :default))))))
)
