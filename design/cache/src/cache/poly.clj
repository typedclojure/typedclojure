(ns cache.poly
  {:typed.clojure {:experimental #{:cache}}}
  (:require [typed.clojure :as t]))

(t/defalias As (t/Seqable t/Int))
(t/defalias Bs (t/Seqable t/Bool #_t/Int)) ;; changing this alias should trigger rechecking all defn's in this namespace

(t/ann-many [As :-> Bs]
            map-kw1
            map-kw2
            map-kw3
            map-kw4
            map-kw5
            map-kw6
            map-kw7
            map-kw8
            map-kw9
            map-kw10
            map-kw11
            map-kw12
            map-kw13
            map-kw14
            map-kw15
            map-kw16
            map-kw17
            map-kw18
            map-kw19)
(defn map-kw1 [as] ^{::t/dbg "map-kw1"} (map boolean as))
(defn map-kw2 [as] ^{::t/dbg "map-kw2"} (map boolean as))
(defn map-kw3 [as] ^{::t/dbg "map-kw3"} (map boolean as))
(defn map-kw4 [as] ^{::t/dbg "map-kw4"} (map boolean as))
(defn map-kw5 [as] ^{::t/dbg "map-kw5"} (map boolean as))
(defn map-kw6 [as] ^{::t/dbg "map-kw6"} (map boolean as))
(defn map-kw7 [as] ^{::t/dbg "map-kw7"} (map boolean as))
(defn map-kw8 [as] ^{::t/dbg "map-kw8"} (map boolean as))
(defn map-kw9 [as] ^{::t/dbg "map-kw9"} (map boolean as))
(defn map-kw10 [as] ^{::t/dbg "map-kw10"} (map boolean as))
(defn map-kw11 [as] ^{::t/dbg "map-kw11"} (map boolean as))
(defn map-kw12 [as] ^{::t/dbg "map-kw12"} (map boolean as))
(defn map-kw13 [as] ^{::t/dbg "map-kw13"} (map boolean as))
(defn map-kw14 [as] ^{::t/dbg "map-kw14"} (map boolean as))
(defn map-kw15 [as] ^{::t/dbg "map-kw15"} (map boolean as))
(defn map-kw16 [as] ^{::t/dbg "map-kw16"} (map boolean as))
(defn map-kw17 [as] ^{::t/dbg "map-kw17"} (map boolean as))
(defn map-kw18 [as] ^{::t/dbg "map-kw18"} (map boolean as))
(defn map-kw19 [as] ^{::t/dbg "map-kw19"} (map boolean as))

(comment
  (t/cns)
  ;Start checking cache.poly
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (ns cache.poly {:typed.clojure {:experimental #{:cache}}} (:require [typed.clojure :as t]))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/ns
  ;   typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars {ns #'clojure.core/ns},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/defalias As (t/Seqable t/Int))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/defalias #'typed.clojure/defalias,
  ;  clojure.core.typed/defalias #'clojure.core.typed/defalias,
  ;  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
  ;  clojure.core/when #'clojure.core/when,
  ;  clojure.core/= #'clojure.core/=,
  ;  clojure.lang.Util clojure.lang.Util,
  ;  clojure.core/intern #'clojure.core/intern,
  ;  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/defalias Bs (t/Seqable t/Bool))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/defalias #'typed.clojure/defalias,
  ;  clojure.core.typed/defalias #'clojure.core.typed/defalias,
  ;  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
  ;  clojure.core/when #'clojure.core/when,
  ;  clojure.core/= #'clojure.core/=,
  ;  clojure.lang.Util clojure.lang.Util,
  ;  clojure.core/intern #'clojure.core/intern,
  ;  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (t/ann-many [As :-> Bs] map-kw1 map-kw2 map-kw3 map-kw4 map-kw5 map-kw6 map-kw7 map-kw8 ...)
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore}},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/ann-many #'typed.clojure/ann-many,
  ;  clojure.core.typed/ann-many #'clojure.core.typed/ann-many,
  ;  clojure.core.typed/ann #'clojure.core.typed/ann,
  ;  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
  ;  clojure.core.typed/ann* #'clojure.core.typed/ann*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__171767 map-kw1
  ;::t/dbg id=G__171767 (map boolean as)
  ;::t/dbg id=G__171767 expected: cache.poly/Bs
  ;::t/dbg id=G__171767 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw1 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw1 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars #{},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.lang.Comparable
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.lang.Comparable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__172182 map-kw2
  ;::t/dbg id=G__172182 (map boolean as)
  ;::t/dbg id=G__172182 expected: cache.poly/Bs
  ;::t/dbg id=G__172182 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw2 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw2 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__172577 map-kw3
  ;::t/dbg id=G__172577 (map boolean as)
  ;::t/dbg id=G__172577 expected: cache.poly/Bs
  ;::t/dbg id=G__172577 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw3 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw3 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__172978 map-kw4
  ;::t/dbg id=G__172978 (map boolean as)
  ;::t/dbg id=G__172978 expected: cache.poly/Bs
  ;::t/dbg id=G__172978 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw4 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw4 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__173381 map-kw5
  ;::t/dbg id=G__173381 (map boolean as)
  ;::t/dbg id=G__173381 expected: cache.poly/Bs
  ;::t/dbg id=G__173381 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw5 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw5 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__173782 map-kw6
  ;::t/dbg id=G__173782 (map boolean as)
  ;::t/dbg id=G__173782 expected: cache.poly/Bs
  ;::t/dbg id=G__173782 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw6 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw6 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__174179 map-kw7
  ;::t/dbg id=G__174179 (map boolean as)
  ;::t/dbg id=G__174179 expected: cache.poly/Bs
  ;::t/dbg id=G__174179 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw7 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw7 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__174572 map-kw8
  ;::t/dbg id=G__174572 (map boolean as)
  ;::t/dbg id=G__174572 expected: cache.poly/Bs
  ;::t/dbg id=G__174572 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw8 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw8 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__174969 map-kw9
  ;::t/dbg id=G__174969 (map boolean as)
  ;::t/dbg id=G__174969 expected: cache.poly/Bs
  ;::t/dbg id=G__174969 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw9 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw9 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__175368 map-kw10
  ;::t/dbg id=G__175368 (map boolean as)
  ;::t/dbg id=G__175368 expected: cache.poly/Bs
  ;::t/dbg id=G__175368 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw10 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw10 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__175761 map-kw11
  ;::t/dbg id=G__175761 (map boolean as)
  ;::t/dbg id=G__175761 expected: cache.poly/Bs
  ;::t/dbg id=G__175761 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw11 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw11 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__176164 map-kw12
  ;::t/dbg id=G__176164 (map boolean as)
  ;::t/dbg id=G__176164 expected: cache.poly/Bs
  ;::t/dbg id=G__176164 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw12 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw12 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__176553 map-kw13
  ;::t/dbg id=G__176553 (map boolean as)
  ;::t/dbg id=G__176553 expected: cache.poly/Bs
  ;::t/dbg id=G__176553 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw13 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw13 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__176952 map-kw14
  ;::t/dbg id=G__176952 (map boolean as)
  ;::t/dbg id=G__176952 expected: cache.poly/Bs
  ;::t/dbg id=G__176952 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw14 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw14 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__177339 map-kw15
  ;::t/dbg id=G__177339 (map boolean as)
  ;::t/dbg id=G__177339 expected: cache.poly/Bs
  ;::t/dbg id=G__177339 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw15 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw15 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     (java.util.List x)
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__177726 map-kw16
  ;::t/dbg id=G__177726 (map boolean as)
  ;::t/dbg id=G__177726 expected: cache.poly/Bs
  ;::t/dbg id=G__177726 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw16 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw16 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__178139 map-kw17
  ;::t/dbg id=G__178139 (map boolean as)
  ;::t/dbg id=G__178139 expected: cache.poly/Bs
  ;::t/dbg id=G__178139 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw17 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw17 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__178542 map-kw18
  ;::t/dbg id=G__178542 (map boolean as)
  ;::t/dbg id=G__178542 expected: cache.poly/Bs
  ;::t/dbg id=G__178542 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw18 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw18 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     (java.util.List x)
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x))),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;::t/dbg id=G__178927 map-kw19
  ;::t/dbg id=G__178927 (map boolean as)
  ;::t/dbg id=G__178927 expected: cache.poly/Bs
  ;::t/dbg id=G__178927 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (defn map-kw19 [as] (map boolean as))
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/defn
  ;   typed.clj.ext.clojure.core__defn/defuspecial__defn,
  ;   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/current-var-annotations
  ;  {cache.poly/map-kw19 [cache.poly/As :-> cache.poly/Bs],
  ;   clojure.core/map
  ;   (typed.clojure/All
  ;    [c a b :..]
  ;    (typed.clojure/IFn
  ;     [[a :-> c] :-> (typed.clojure/Transducer a c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/NonEmptySeqable a)
  ;      (typed.clojure/NonEmptySeqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/NonEmptyASeq c)]
  ;     [[a b :.. b :-> c]
  ;      (typed.clojure/Seqable a)
  ;      (typed.clojure/Seqable b)
  ;      :..
  ;      b
  ;      :->
  ;      (typed.clojure/ASeq c)])),
  ;   clojure.core/boolean [typed.clojure/Any :-> typed.clojure/Bool]},
  ;  :clojure.core.typed.current-impl/current-used-vars
  ;  #{clojure.core/boolean clojure.core/map},
  ;  :clojure.core.typed.current-impl/current-name-env
  ;  {java.lang.Short {},
  ;   typed.clojure/Bool java.lang.Boolean,
  ;   typed.clojure/Seqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.Collection {},
  ;   cache.poly/As (typed.clojure/Seqable typed.clojure/Int),
  ;   java.lang.Boolean {},
  ;   typed.clojure/Option
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/U nil x)),
  ;   clojure.lang.IPersistentCollection {},
  ;   clojure.lang.Fn {},
  ;   typed.clojure/Fn clojure.lang.Fn,
  ;   typed.clojure/Nilable typed.clojure/Option,
  ;   cache.poly/Bs (typed.clojure/Seqable typed.clojure/Bool),
  ;   typed.clojure/NonEmptyASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (typed.clojure/CountRange 1)
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
  ;   java.util.SequencedCollection {},
  ;   clojure.lang.Seqable {},
  ;   typed.clojure/NilableNonEmptySeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
  ;   typed.clojure/ASeq
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj)),
  ;   typed.clojure/Int typed.clojure/AnyInteger,
  ;   typed.clojure/AnyInteger
  ;   (typed.clojure/U
  ;    java.lang.Short
  ;    java.lang.Byte
  ;    java.math.BigInteger
  ;    java.lang.Integer
  ;    clojure.lang.BigInt
  ;    java.lang.Long),
  ;   java.lang.Iterable {}},
  ;  :clojure.core.typed.current-impl/current-rclass-env
  ;  {java.util.SequencedCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :invariant]]
  ;    (java.util.SequencedCollection a)),
  ;   java.util.Collection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.util.Collection a)),
  ;   java.lang.Iterable
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (java.lang.Iterable a)),
  ;   clojure.lang.Seqable
  ;   (typed.clojure/TFn
  ;    [[a
  ;      :variance
  ;      :covariant
  ;      :<
  ;      (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
  ;    (clojure.lang.Seqable a)),
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a))}},
  ; :typed.cljc.checker.check.cache/vars
  ; {defn #'clojure.core/defn,
  ;  clojure.core/fn #'clojure.core/fn,
  ;  map #'clojure.core/map,
  ;  boolean #'clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;cache: Caching form with cache info
  ;cache: dependencies for form: (comment (t/cns) :ok)
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars {comment #'clojure.core/comment},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {}}
  ;Checked cache.poly in 2765.766896 msecs
  :ok

;=Start checking cache.defn-chain
;=cache: Caching form with cache info
;=cache: dependencies for form: (ns cache.defn-chain {:typed.clojure {:experimental #{:cache}}} (:require [typed.clojure :as t]))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/ns
;=   typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars {ns #'clojure.core/ns},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias A1 (quote :A1))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/defalias #'typed.clojure/defalias,
;=  clojure.core.typed/defalias #'clojure.core.typed/defalias,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core/when #'clojure.core/when,
;=  clojure.core/= #'clojure.core/=,
;=  clojure.lang.Util clojure.lang.Util,
;=  clojure.core/intern #'clojure.core/intern,
;=  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias A2 (quote :A2))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/defalias #'typed.clojure/defalias,
;=  clojure.core.typed/defalias #'clojure.core.typed/defalias,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core/when #'clojure.core/when,
;=  clojure.core/= #'clojure.core/=,
;=  clojure.lang.Util clojure.lang.Util,
;=  clojure.core/intern #'clojure.core/intern,
;=  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias A3 (quote :A3))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/defalias #'typed.clojure/defalias,
;=  clojure.core.typed/defalias #'clojure.core.typed/defalias,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core/when #'clojure.core/when,
;=  clojure.core/= #'clojure.core/=,
;=  clojure.lang.Util clojure.lang.Util,
;=  clojure.core/intern #'clojure.core/intern,
;=  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias A4 (quote :A4))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/defalias #'typed.clojure/defalias,
;=  clojure.core.typed/defalias #'clojure.core.typed/defalias,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core/when #'clojure.core/when,
;=  clojure.core/= #'clojure.core/=,
;=  clojure.lang.Util clojure.lang.Util,
;=  clojure.core/intern #'clojure.core/intern,
;=  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias A5 (quote :A5))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/defalias #'typed.clojure/defalias,
;=  clojure.core.typed/defalias #'clojure.core.typed/defalias,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core/when #'clojure.core/when,
;=  clojure.core/= #'clojure.core/=,
;=  clojure.lang.Util clojure.lang.Util,
;=  clojure.core/intern #'clojure.core/intern,
;=  clojure.core.typed/defalias* #'clojure.core.typed/defalias*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (declare a1 a2 a3 a4 a5)
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars {declare #'clojure.core/declare},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/ann a1 [A1 :-> A5])
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/ann #'typed.clojure/ann,
;=  clojure.core.typed/ann #'clojure.core.typed/ann,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core.typed/ann* #'clojure.core.typed/ann*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__179397 (a2 :A2)
;=::t/dbg id=G__179397 expected: cache.defn-chain/A5
;=::t/dbg id=G__179397 result: cache.defn-chain/A5
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn a1 [x] (a2 :A2))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn
;=   typed.clj.ext.clojure.core__defn/defuspecial__defn,
;=   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure,
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.defn-chain/a1 [cache.defn-chain/A1 :-> cache.defn-chain/A5],
;=   cache.defn-chain/a2 [cache.defn-chain/A2 :-> cache.defn-chain/A5]},
;=  :clojure.core.typed.current-impl/current-used-vars #{},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {cache.defn-chain/A2 (typed.clojure/Val :A2),
;=   typed.clojure/Fn clojure.lang.Fn,
;=   clojure.lang.Fn {}}},
;= :typed.cljc.checker.check.cache/vars
;= {defn #'clojure.core/defn,
;=  clojure.core/fn #'clojure.core/fn,
;=  a2 #'cache.defn-chain/a2},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/ann a2 [A2 :-> A5])
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/ann #'typed.clojure/ann,
;=  clojure.core.typed/ann #'clojure.core.typed/ann,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core.typed/ann* #'clojure.core.typed/ann*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__179403 (a3 :A3)
;=::t/dbg id=G__179403 expected: cache.defn-chain/A5
;=::t/dbg id=G__179403 result: cache.defn-chain/A5
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn a2 [x] (a3 :A3))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn
;=   typed.clj.ext.clojure.core__defn/defuspecial__defn,
;=   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure,
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.defn-chain/a2 [cache.defn-chain/A2 :-> cache.defn-chain/A5],
;=   cache.defn-chain/a3 [cache.defn-chain/A3 :-> cache.defn-chain/A5]},
;=  :clojure.core.typed.current-impl/current-used-vars
;=  #{cache.defn-chain/a2},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {cache.defn-chain/A3 (typed.clojure/Val :A3),
;=   typed.clojure/Fn clojure.lang.Fn,
;=   clojure.lang.Fn {}}},
;= :typed.cljc.checker.check.cache/vars
;= {defn #'clojure.core/defn,
;=  clojure.core/fn #'clojure.core/fn,
;=  a3 #'cache.defn-chain/a3},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/ann a3 [A3 :-> A5])
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/ann #'typed.clojure/ann,
;=  clojure.core.typed/ann #'clojure.core.typed/ann,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core.typed/ann* #'clojure.core.typed/ann*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__179412 (a4 :A4)
;=::t/dbg id=G__179412 expected: cache.defn-chain/A5
;=::t/dbg id=G__179412 result: cache.defn-chain/A5
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn a3 [x] (identity 1) (a4 :A4))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn
;=   typed.clj.ext.clojure.core__defn/defuspecial__defn,
;=   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure,
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.defn-chain/a3 [cache.defn-chain/A3 :-> cache.defn-chain/A5],
;=   clojure.core/identity
;=   (typed.clojure/All
;=    [x]
;=    [x
;=     :->
;=     x
;=     :filters
;=     {:then (! (typed.clojure/U false nil) 0),
;=      :else (is (typed.clojure/U false nil) 0)}
;=     :object
;=     {:id 0}]),
;=   cache.defn-chain/a4 [cache.defn-chain/A4 :-> cache.defn-chain/A5]},
;=  :clojure.core.typed.current-impl/current-used-vars
;=  #{cache.defn-chain/a3 cache.defn-chain/a2},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {cache.defn-chain/A4 (typed.clojure/Val :A4),
;=   typed.clojure/Fn clojure.lang.Fn,
;=   clojure.lang.Fn {}}},
;= :typed.cljc.checker.check.cache/vars
;= {defn #'clojure.core/defn,
;=  clojure.core/fn #'clojure.core/fn,
;=  identity #'clojure.core/identity,
;=  a4 #'cache.defn-chain/a4},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/ann a4 [A4 :-> A5])
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore
;=   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars
;= {t/ann #'typed.clojure/ann,
;=  clojure.core.typed/ann #'clojure.core.typed/ann,
;=  clojure.core.typed/tc-ignore #'clojure.core.typed/tc-ignore,
;=  clojure.core.typed/ann* #'clojure.core.typed/ann*},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__179418 (do :A5)
;=::t/dbg id=G__179418 expected: cache.defn-chain/A5
;=::t/dbg id=G__179418 result: [cache.defn-chain/A5 {:then tt, :else ff}]
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn a4 [x] (do :A5))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn
;=   typed.clj.ext.clojure.core__defn/defuspecial__defn,
;=   clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
;=  :clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure,
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.defn-chain/a4 [cache.defn-chain/A4 :-> cache.defn-chain/A5]},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {cache.defn-chain/A5 (typed.clojure/Val :A5),
;=   typed.clojure/Fn clojure.lang.Fn,
;=   clojure.lang.Fn {}}},
;= :typed.cljc.checker.check.cache/vars
;= {defn #'clojure.core/defn, clojure.core/fn #'clojure.core/fn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (comment (t/cns) :ok)
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/current-impl
;=  :clojure.core.typed.current-impl/clojure},
;= :typed.cljc.checker.check.cache/vars {comment #'clojure.core/comment},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=Checked cache.defn-chain in 221.30706 msecs
:ok
  )
