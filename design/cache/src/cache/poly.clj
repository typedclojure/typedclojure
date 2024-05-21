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
;=Start checking cache.poly
;=cache: Caching form with cache info
;=cache: dependencies for form: (ns cache.poly {:typed.clojure {:experimental #{:cache}}} (:require [typed.clojure :as t]))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/ns {}},
;=  :clojure.core.typed.current-impl/current-impl {}},
;= :typed.cljc.checker.check.cache/vars #{#'clojure.core/ns},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias As (t/Seqable t/Int))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore {}},
;=  :clojure.core.typed.current-impl/current-impl {}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/when
;=   #'clojure.core/=
;=   #'clojure.core.typed/defalias
;=   #'clojure.core.typed/defalias*
;=   #'typed.clojure/defalias
;=   #'clojure.core.typed/tc-ignore
;=   #'clojure.core/intern},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/defalias Bs (t/Seqable t/Bool))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore {}},
;=  :clojure.core.typed.current-impl/current-impl {}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/when
;=   #'clojure.core/=
;=   #'clojure.core.typed/defalias
;=   #'clojure.core.typed/defalias*
;=   #'typed.clojure/defalias
;=   #'clojure.core.typed/tc-ignore
;=   #'clojure.core/intern},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (t/ann-many [As :-> Bs] map-kw1 map-kw2 map-kw3 map-kw4 map-kw5 map-kw6 map-kw7 map-kw8 ...)
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core.typed/tc-ignore {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core.typed/ann*
;=   #'clojure.core.typed/ann-many
;=   #'clojure.core.typed/ann
;=   #'typed.clojure/ann-many
;=   #'clojure.core.typed/tc-ignore},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__149911 map-kw1
;=::t/dbg id=G__149911 (map boolean as)
;=::t/dbg id=G__149911 expected: cache.poly/Bs
;=::t/dbg id=G__149911 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw1 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw1 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.Seqable {},
;=   java.lang.Comparable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__150265 map-kw2
;=::t/dbg id=G__150265 (map boolean as)
;=::t/dbg id=G__150265 expected: cache.poly/Bs
;=::t/dbg id=G__150265 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw2 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw2 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__150586 map-kw3
;=::t/dbg id=G__150586 (map boolean as)
;=::t/dbg id=G__150586 expected: cache.poly/Bs
;=::t/dbg id=G__150586 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw3 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw3 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__150897 map-kw4
;=::t/dbg id=G__150897 (map boolean as)
;=::t/dbg id=G__150897 expected: cache.poly/Bs
;=::t/dbg id=G__150897 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw4 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw4 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__151232 map-kw5
;=::t/dbg id=G__151232 (map boolean as)
;=::t/dbg id=G__151232 expected: cache.poly/Bs
;=::t/dbg id=G__151232 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw5 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw5 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__151561 map-kw6
;=::t/dbg id=G__151561 (map boolean as)
;=::t/dbg id=G__151561 expected: cache.poly/Bs
;=::t/dbg id=G__151561 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw6 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw6 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__151882 map-kw7
;=::t/dbg id=G__151882 (map boolean as)
;=::t/dbg id=G__151882 expected: cache.poly/Bs
;=::t/dbg id=G__151882 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw7 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw7 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__152217 map-kw8
;=::t/dbg id=G__152217 (map boolean as)
;=::t/dbg id=G__152217 expected: cache.poly/Bs
;=::t/dbg id=G__152217 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw8 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw8 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__152546 map-kw9
;=::t/dbg id=G__152546 (map boolean as)
;=::t/dbg id=G__152546 expected: cache.poly/Bs
;=::t/dbg id=G__152546 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw9 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw9 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__152879 map-kw10
;=::t/dbg id=G__152879 (map boolean as)
;=::t/dbg id=G__152879 expected: cache.poly/Bs
;=::t/dbg id=G__152879 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw10 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw10 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__153208 map-kw11
;=::t/dbg id=G__153208 (map boolean as)
;=::t/dbg id=G__153208 expected: cache.poly/Bs
;=::t/dbg id=G__153208 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw11 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw11 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__153535 map-kw12
;=::t/dbg id=G__153535 (map boolean as)
;=::t/dbg id=G__153535 expected: cache.poly/Bs
;=::t/dbg id=G__153535 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw12 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw12 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__153870 map-kw13
;=::t/dbg id=G__153870 (map boolean as)
;=::t/dbg id=G__153870 expected: cache.poly/Bs
;=::t/dbg id=G__153870 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw13 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw13 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__154181 map-kw14
;=::t/dbg id=G__154181 (map boolean as)
;=::t/dbg id=G__154181 expected: cache.poly/Bs
;=::t/dbg id=G__154181 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw14 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw14 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {clojure.lang.IPersistentCollection {},
;=   clojure.lang.Seqable {},
;=   java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__154524 map-kw15
;=::t/dbg id=G__154524 (map boolean as)
;=::t/dbg id=G__154524 expected: cache.poly/Bs
;=::t/dbg id=G__154524 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw15 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw15 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__154847 map-kw16
;=::t/dbg id=G__154847 (map boolean as)
;=::t/dbg id=G__154847 expected: cache.poly/Bs
;=::t/dbg id=G__154847 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw16 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw16 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__155174 map-kw17
;=::t/dbg id=G__155174 (map boolean as)
;=::t/dbg id=G__155174 expected: cache.poly/Bs
;=::t/dbg id=G__155174 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw17 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw17 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__155501 map-kw18
;=::t/dbg id=G__155501 (map boolean as)
;=::t/dbg id=G__155501 expected: cache.poly/Bs
;=::t/dbg id=G__155501 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw18 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw18 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=::t/dbg id=G__155838 map-kw19
;=::t/dbg id=G__155838 (map boolean as)
;=::t/dbg id=G__155838 expected: cache.poly/Bs
;=::t/dbg id=G__155838 result: cache.poly/Bs
;=cache: Caching form with cache info
;=cache: dependencies for form: (defn map-kw19 [as] (map boolean as))
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/unanalyzed-special
;=  {clojure.core/defn {}, clojure.core/fn {}},
;=  :clojure.core.typed.current-impl/current-impl {},
;=  :clojure.core.typed.current-impl/current-var-annotations
;=  {cache.poly/map-kw19 {},
;=   clojure.core/map {},
;=   clojure.core/boolean {}},
;=  :clojure.core.typed.current-impl/current-nocheck-var? {},
;=  :clojure.core.typed.current-impl/current-used-vars {},
;=  :clojure.core.typed.current-impl/current-name-env
;=  {java.lang.Short {},
;=   typed.clojure/Bool {},
;=   typed.clojure/Seqable {},
;=   java.util.Collection {},
;=   cache.poly/As {},
;=   java.lang.Boolean {},
;=   typed.clojure/Option {},
;=   clojure.lang.IPersistentCollection {},
;=   clojure.lang.Fn {},
;=   typed.clojure/Fn {},
;=   typed.clojure/Nilable {},
;=   cache.poly/Bs {},
;=   typed.clojure/NonEmptyASeq {},
;=   typed.clojure/NonEmptySeqable {},
;=   java.util.SequencedCollection {},
;=   clojure.lang.Seqable {},
;=   typed.clojure/NilableNonEmptySeq {},
;=   typed.clojure/ASeq {},
;=   typed.clojure/Int {},
;=   typed.clojure/AnyInteger {},
;=   java.lang.Iterable {}},
;=  :clojure.core.typed.current-impl/current-rclass-env
;=  {java.util.SequencedCollection {},
;=   java.util.Collection {},
;=   java.lang.Iterable {},
;=   clojure.lang.Seqable {},
;=   clojure.lang.IPersistentCollection {}}},
;= :typed.cljc.checker.check.cache/vars
;= #{#'clojure.core/boolean
;=   #'clojure.core/fn
;=   #'clojure.core/map
;=   #'clojure.core/defn},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=cache: Caching form with cache info
;=cache: dependencies for form: (comment (t/cns) :ok)
;={:typed.cljc.checker.check.cache/types
;= {:clojure.core.typed.current-impl/current-impl {}},
;= :typed.cljc.checker.check.cache/vars #{#'clojure.core/comment},
;= :typed.cljc.checker.check.cache/errors false,
;= :typed.cljc.checker.check.cache/interop {}}
;=Checked cache.poly in 1092.187963 msecs
  :ok
  )
