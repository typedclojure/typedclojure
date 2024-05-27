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
  ;; 3x faster
  (time (t/check-ns-clj *ns* :max-parallelism :available-processors))

  (time (t/cns))
  ;Start checking cache.poly
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core/ns
  ;   typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars {ns clojure.core/ns},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(t/defalias As (t/Seqable t/Int))
  ;<<<<
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/defalias typed.clojure/defalias,
  ;  clojure.core.typed/defalias clojure.core.typed/defalias,
  ;  clojure.core.typed/tc-ignore clojure.core.typed/tc-ignore,
  ;  clojure.core/when clojure.core/when,
  ;  clojure.core/= clojure.core/=,
  ;  clojure.lang.Util clojure.lang.Util,
  ;  clojure.core/intern clojure.core/intern,
  ;  clojure.core.typed/defalias* clojure.core.typed/defalias*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(t/defalias Bs (t/Seqable t/Bool #_t/Int))
  ;<<<<
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore},
  ;  :clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/defalias typed.clojure/defalias,
  ;  clojure.core.typed/defalias clojure.core.typed/defalias,
  ;  clojure.core.typed/tc-ignore clojure.core.typed/tc-ignore,
  ;  clojure.core/when clojure.core/when,
  ;  clojure.core/= clojure.core/=,
  ;  clojure.lang.Util clojure.lang.Util,
  ;  clojure.core/intern clojure.core/intern,
  ;  clojure.core.typed/defalias* clojure.core.typed/defalias*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;;; changing this alias should trigger rechecking all defn's in this namespace
  ;
  ;(t/ann-many [As :-> Bs]
  ;            map-kw1
  ;            map-kw2
  ;            map-kw3
  ;            map-kw4
  ;            map-kw5
  ;            map-kw6
  ;            map-kw7
  ;            map-kw8
  ;            map-kw9
  ;            map-kw10
  ;            map-kw11
  ;            map-kw12
  ;            map-kw13
  ;            map-kw14
  ;            map-kw15
  ;            map-kw16
  ;            map-kw17
  ;            map-kw18
  ;            map-kw19)
  ;<<<<
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure,
  ;  :clojure.core.typed.current-impl/unanalyzed-special
  ;  {clojure.core.typed/tc-ignore
  ;   typed.clj.ext.clojure.core.typed__tc-ignore/defuspecial__tc-ignore}},
  ; :typed.cljc.checker.check.cache/vars
  ; {t/ann-many typed.clojure/ann-many,
  ;  clojure.core.typed/ann-many clojure.core.typed/ann-many,
  ;  clojure.core.typed/ann clojure.core.typed/ann,
  ;  clojure.core.typed/tc-ignore clojure.core.typed/tc-ignore,
  ;  clojure.core.typed/ann* clojure.core.typed/ann*},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__61459 map-kw1
  ;::t/dbg id=G__61459 (map boolean as)
  ;::t/dbg id=G__61459 expected: cache.poly/Bs
  ;::t/dbg id=G__61459 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw1 [as] ^{::t/dbg "map-kw1"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ;   clojure.lang.IPersistentCollection
  ;   (typed.clojure/TFn
  ;    [[a :variance :covariant]]
  ;    (clojure.lang.IPersistentCollection a)),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms
  ; {x x,
  ;  t/Seq typed.clojure/Seq,
  ;  t/NonEmptyASeq typed.clojure/NonEmptyASeq,
  ;  t/NonEmptySeq typed.clojure/NonEmptySeq,
  ;  clojure.lang.ISeq clojure.lang.ISeq,
  ;  t/NonEmptyCount typed.clojure/NonEmptyCount,
  ;  t/NonEmptySeqable typed.clojure/NonEmptySeqable,
  ;  a a,
  ;  t/Nilable typed.clojure/Nilable,
  ;  t/Seqable typed.clojure/Seqable,
  ;  t/Sequential typed.clojure/Sequential,
  ;  Collection java.util.Collection,
  ;  java.util.Collection java.util.Collection,
  ;  Seqable clojure.lang.Seqable,
  ;  t/SequentialSeq typed.clojure/SequentialSeq,
  ;  Iterable java.lang.Iterable,
  ;  java.lang.Boolean java.lang.Boolean,
  ;  t/Transducer typed.clojure/Transducer,
  ;  t/ASeq typed.clojure/ASeq,
  ;  clojure.lang.IObj clojure.lang.IObj,
  ;  t/Option typed.clojure/Option,
  ;  t/NilableNonEmptySeq typed.clojure/NilableNonEmptySeq,
  ;  c c,
  ;  clojure.lang.Sequential clojure.lang.Sequential,
  ;  java.util.SequencedCollection java.util.SequencedCollection,
  ;  clojure.lang.Seqable clojure.lang.Seqable,
  ;  t/Any typed.clojure/Any,
  ;  java.util.List java.util.List,
  ;  b b,
  ;  IPersistentCollection clojure.lang.IPersistentCollection}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__61960 map-kw2
  ;::t/dbg id=G__61960 (map boolean as)
  ;::t/dbg id=G__61960 expected: cache.poly/Bs
  ;::t/dbg id=G__61960 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw2 [as] ^{::t/dbg "map-kw2"} (map boolean as))
  ;<<<<
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
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__62357 map-kw3
  ;::t/dbg id=G__62357 (map boolean as)
  ;::t/dbg id=G__62357 expected: cache.poly/Bs
  ;::t/dbg id=G__62357 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw3 [as] ^{::t/dbg "map-kw3"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__62758 map-kw4
  ;::t/dbg id=G__62758 (map boolean as)
  ;::t/dbg id=G__62758 expected: cache.poly/Bs
  ;::t/dbg id=G__62758 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw4 [as] ^{::t/dbg "map-kw4"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__63159 map-kw5
  ;::t/dbg id=G__63159 (map boolean as)
  ;::t/dbg id=G__63159 expected: cache.poly/Bs
  ;::t/dbg id=G__63159 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw5 [as] ^{::t/dbg "map-kw5"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__63552 map-kw6
  ;::t/dbg id=G__63552 (map boolean as)
  ;::t/dbg id=G__63552 expected: cache.poly/Bs
  ;::t/dbg id=G__63552 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw6 [as] ^{::t/dbg "map-kw6"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__63949 map-kw7
  ;::t/dbg id=G__63949 (map boolean as)
  ;::t/dbg id=G__63949 expected: cache.poly/Bs
  ;::t/dbg id=G__63949 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw7 [as] ^{::t/dbg "map-kw7"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__64342 map-kw8
  ;::t/dbg id=G__64342 (map boolean as)
  ;::t/dbg id=G__64342 expected: cache.poly/Bs
  ;::t/dbg id=G__64342 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw8 [as] ^{::t/dbg "map-kw8"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     (java.util.List x)
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__64743 map-kw9
  ;::t/dbg id=G__64743 (map boolean as)
  ;::t/dbg id=G__64743 expected: cache.poly/Bs
  ;::t/dbg id=G__64743 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw9 [as] ^{::t/dbg "map-kw9"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__65148 map-kw10
  ;::t/dbg id=G__65148 (map boolean as)
  ;::t/dbg id=G__65148 expected: cache.poly/Bs
  ;::t/dbg id=G__65148 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw10 [as] ^{::t/dbg "map-kw10"} (map boolean as))
  ;<<<<
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
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__65545 map-kw11
  ;::t/dbg id=G__65545 (map boolean as)
  ;::t/dbg id=G__65545 expected: cache.poly/Bs
  ;::t/dbg id=G__65545 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw11 [as] ^{::t/dbg "map-kw11"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__65956 map-kw12
  ;::t/dbg id=G__65956 (map boolean as)
  ;::t/dbg id=G__65956 expected: cache.poly/Bs
  ;::t/dbg id=G__65956 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw12 [as] ^{::t/dbg "map-kw12"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__66333 map-kw13
  ;::t/dbg id=G__66333 (map boolean as)
  ;::t/dbg id=G__66333 expected: cache.poly/Bs
  ;::t/dbg id=G__66333 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw13 [as] ^{::t/dbg "map-kw13"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__66734 map-kw14
  ;::t/dbg id=G__66734 (map boolean as)
  ;::t/dbg id=G__66734 expected: cache.poly/Bs
  ;::t/dbg id=G__66734 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw14 [as] ^{::t/dbg "map-kw14"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__67131 map-kw15
  ;::t/dbg id=G__67131 (map boolean as)
  ;::t/dbg id=G__67131 expected: cache.poly/Bs
  ;::t/dbg id=G__67131 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw15 [as] ^{::t/dbg "map-kw15"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__67518 map-kw16
  ;::t/dbg id=G__67518 (map boolean as)
  ;::t/dbg id=G__67518 expected: cache.poly/Bs
  ;::t/dbg id=G__67518 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw16 [as] ^{::t/dbg "map-kw16"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__67903 map-kw17
  ;::t/dbg id=G__67903 (map boolean as)
  ;::t/dbg id=G__67903 expected: cache.poly/Bs
  ;::t/dbg id=G__67903 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw17 [as] ^{::t/dbg "map-kw17"} (map boolean as))
  ;<<<<
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
  ;     (clojure.lang.ISeq x)
  ;     (java.util.List x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__68306 map-kw18
  ;::t/dbg id=G__68306 (map boolean as)
  ;::t/dbg id=G__68306 expected: cache.poly/Bs
  ;::t/dbg id=G__68306 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw18 [as] ^{::t/dbg "map-kw18"} (map boolean as))
  ;<<<<
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
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;::t/dbg id=G__68705 map-kw19
  ;::t/dbg id=G__68705 (map boolean as)
  ;::t/dbg id=G__68705 expected: cache.poly/Bs
  ;::t/dbg id=G__68705 result: cache.poly/Bs
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(defn map-kw19 [as] ^{::t/dbg "map-kw19"} (map boolean as))
  ;<<<<
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
  ;     clojure.lang.Sequential
  ;     clojure.lang.IObj
  ;     (java.util.List x)
  ;     (clojure.lang.ISeq x)
  ;     (typed.clojure/CountRange 1))),
  ;   typed.clojure/NonEmptySeqable
  ;   (typed.clojure/TFn
  ;    [[x :variance :covariant]]
  ;    (typed.clojure/I
  ;     (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
  ;     (typed.clojure/CountRange 1))),
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
  ; {defn clojure.core/defn,
  ;  clojure.core/fn clojure.core/fn,
  ;  map clojure.core/map,
  ;  boolean clojure.core/boolean},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;need-to-check-top-level-expr?: did not find cache info
  ;cache: Caching form with cache info
  ;ns form:
  ;>>>>
  ;(ns cache.poly
  ;  {:typed.clojure {:experimental #{:cache}}}
  ;  (:require [typed.clojure :as t]))
  ;<<<<
  ;cache: on disk:
  ;>>>>
  ;(comment
  ; ...
  ;<<<<
  ;{:typed.cljc.checker.check.cache/types
  ; {:clojure.core.typed.current-impl/current-impl
  ;  :clojure.core.typed.current-impl/clojure},
  ; :typed.cljc.checker.check.cache/vars {comment clojure.core/comment},
  ; :typed.cljc.checker.check.cache/errors false,
  ; :typed.cljc.checker.check.cache/interop {},
  ; :typed.cljc.checker.check.cache/type-syms {}}
  ;Checked cache.poly in 2716.886404 msecs
  :ok
  )
