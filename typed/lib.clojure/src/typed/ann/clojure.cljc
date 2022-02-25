;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.ann.clojure
  "Type annotations for the base Clojure distribution."
  (:require [#?(:clj clojure.core :cljs cljs.core) :as cc]
            [typed.clojure :refer [defalias] :as t]
            #?(:clj clojure.core.typed))
  #?(:clj
     (:import (clojure.lang Named IMapEntry AMapEntry
                            LazySeq PersistentHashSet PersistentTreeSet PersistentList
                            APersistentMap ISeq IPersistentCollection
                            ILookup Indexed Associative #_ITransientSet
                            IRef Reduced)
              (java.util Comparator Collection))))

(defalias
  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms '[AnyInteger]}
  t/AnyInteger
  #?(:clj (t/U Integer
               Long
               clojure.lang.BigInt
               BigInteger
               Short
               Byte)
     :cljs t/CLJSInteger))

(defalias
  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms '[Int]}
  t/Int
  t/AnyInteger)

(defalias
  ^{:doc "A type that returns true for clojure.core/number?"
    :forms '[Num]}
  t/Num
  #?(:clj Number
     :cljs t/JSnumber))

(defalias
  ^{:doc "A keyword"
    :forms '[Keyword]}
  t/Keyword
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core/Keyword))

(defalias
  ^{:doc "A keyword"
    :forms '[Kw]}
  t/Kw
  t/Keyword)

(defalias
  ^{:doc "A symbol"
    :forms '[Symbol]}
  t/Symbol
  #?(:clj clojure.lang.Symbol
     :cljs cljs.core/Symbol))

(defalias
  ^{:doc "A symbol"
    :forms '[Sym]}
  t/Sym
  t/Symbol)

(defalias
  ^{:doc "A string"
    :forms '[Str]}
  t/Str
  #?(:clj java.lang.String
     :cljs t/JSstring))

(defalias
  ^{:doc "A boolean"
    :forms '[Bool]}
  t/Bool
  #?(:clj java.lang.Boolean
     :cljs t/JSboolean))

(defalias
  ^{:doc "A namespace"
    :forms '[Namespace]}
  t/Namespace
  #?(:clj clojure.lang.Namespace
     ;; nilable?
     :cljs cljs.core/Namespace))

#?(:clj
   (defalias
     ^{:doc "An atom that can write type w and read type r."
       :forms '[(Atom2 t)]}
     t/Atom2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Atom w r))))

#?(:clj
   (defalias
     ^{:doc "An atom that can read and write type x."
       :forms '[(Atom1 t)]}
     t/Atom1
     (t/TFn [[x :variance :invariant]]
            (t/Atom2 x x))))

(defalias
  ^{:doc "An var that can write type w and read type r."
    :forms '[(Var2 w r)]}
  t/Var2 
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]] 
         #?(:clj (clojure.lang.Var w r)
            :cljs (cljs.core/Var w r))))

(defalias
  ^{:doc "An var that can read and write type x."
    :forms '[(Var1 t)]}
  t/Var1 
  (t/TFn [[x :variance :invariant]] 
         (t/Var2 x x)))

#?(:clj
   (defalias
     ^{:doc "A ref that can write type w and read type r."
       :forms '[(Ref2 w r)]}
     t/Ref2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Ref w r))))

#?(:clj
   (defalias
     ^{:doc "A ref that can read and write type x."
       :forms '[(Ref1 t)]}
     t/Ref1
     (t/TFn [[x :variance :invariant]]
            (t/Ref2 x x))))

#?(:clj
   (defalias
     ^{:doc "An agent that can write type w and read type r."
       :forms '[(Agent2 t t)]}
     t/Agent2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Agent w r))))

#?(:clj
   (defalias
     ^{:doc "An agent that can read and write type x."
       :forms '[(Agent1 t)]}
     t/Agent1
     (t/TFn [[x :variance :invariant]] 
            (t/Agent2 x x))))

(defalias
  ^{:doc "A union of x and nil."
    :forms '[(Option t)]}
  t/Option
  (t/TFn [[x :variance :covariant]] (t/U nil x)))

(defalias
  ^{:doc "A union of x and nil."
    :forms '[(Nilable t)]}
  t/Nilable
  t/Option)

(defalias
  ^{:doc "The identity function at the type level."
    :forms '[Id]}
  t/Id
  (t/TFn [[x :variance :covariant]] x))

(defalias
  ^{:doc "A type that returns true for clojure.core/seqable?, with members t."
    :forms '[(Seqable t)]}
  t/Seqable
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           #?(:clj (clojure.lang.Seqable x)
              :cljs (cljs.core/ISeqable x)))))

(defalias
  ^{:doc "A type that returns true for clojure.core/coll?, with members t."
    :forms '[(Coll t)]}
  t/Coll
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentCollection x)
            :cljs (t/I (t/Seqable x)
                       (cljs.core/ICollection x)
                       cljs.core/ICounted
                       cljs.core/IEmptyableCollection
                       cljs.core/IEquiv))))

(defalias
  ^{:doc "The type of all things with count 0. Use as part of an intersection.
         eg. See EmptySeqable."
    :forms '[EmptyCount]}
  t/EmptyCount
  (t/ExactCount 0))

(defalias
  ^{:doc "The type of all things with count greater than 0. Use as part of an intersection.
         eg. See NonEmptySeq"
    :forms '[NonEmptyCount]}
  t/NonEmptyCount
  (t/CountRange 1))

(defalias
  ^{:doc "A persistent collection with member type x and count greater than 0."
    :forms '[(NonEmptyColl t)]}
  t/NonEmptyColl
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Coll x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "An associative persistent collection with members of type m
         and supporting associative operations on keys type k and values type v."
    :forms '[(Associative k v)]}
  t/Associative
  (t/TFn [[m :variance :covariant]
          [k :variance :covariant]
          [v :variance :covariant]]
         #?(:clj (clojure.lang.Associative m k v)
            :cljs (t/I (cljs.core/IAssociative m k v)
                       (t/Coll m)
                       (cljs.core/ILookup k v)))))

(defalias
  ^{:doc "A Clojure reversible collection."
    :forms '[(Reversible t)]}
  t/Reversible
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Reversible x)
            :cljs (cljs.core/Reversible x))))

(defalias
  ^{:doc "A persistent vector with member type x."
    :forms '[(Vec t)]}
  t/Vec
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentVector x)
            :cljs (t/I (cljs.core/IVector x)
                       (t/Associative x t/Int x)
                       cljs.core/ISequential
                       (cljs.core/IStack x)
                       (t/Reversible x)
                       (cljs.core/IIndexed x)))))

(defalias
  ^{:doc "A persistent vector with member type x and count greater than 0."
    :forms '[(NonEmptyVec t)]}
  t/NonEmptyVec
  (t/TFn [[x :variance :covariant]]
       (t/I (t/Vec x)
            t/NonEmptyCount)))

(defalias
  ^{:doc "A persistent vector returned from clojure.core/vector (and others)"
    :forms '[(AVec t)]}
  t/AVec
  (t/TFn [[x :variance :covariant]]
         #?(:clj (t/I ; is this type useful enough? c.l.APV implements a lot more
                      (t/Vec x)
                      (java.lang.Iterable x)
                      (java.util.Collection x)
                      (java.util.List x)
                      clojure.lang.IObj)
            :cljs (t/I (t/Vec x)
                       cljs.core/APersistentVector
                       ; TODO cljs equivalent
                       ))))

;;TODO from here, add :cljs equivalents

(defalias
  ^{:doc "A persistent vector returned from clojure.core/vector (and others) and count greater than 0."
    :forms '[(NonEmptyAVec t)]}
  t/NonEmptyAVec
  (t/TFn [[x :variance :covariant]]
       (t/I (t/AVec x)
            t/NonEmptyCount)))

(defalias
  ^{:doc "A non-empty lazy sequence of type t"
    :forms '[(NonEmptyLazySeq t)]}
  t/NonEmptyLazySeq
  (t/TFn [[t :variance :covariant]]
       (t/I (clojure.lang.LazySeq t)
            t/NonEmptyCount)))

(defalias
  ^{:doc "A persistent map with keys k and vals v."
    :forms '[(Map t t)]}
  t/Map
  (t/TFn [[k :variance :covariant]
        [v :variance :covariant]]
       (clojure.lang.IPersistentMap k v)))

(defalias
  ^{:doc "A persistent set with member type x"
    :forms '[(Set t)]}
  t/Set
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IPersistentSet x)))

(defalias
  ^{:doc "A sorted persistent set with member type x"
    :forms '[(SortedSet t)]}
  t/SortedSet
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Set x)
              clojure.lang.Sorted)))

(defalias
  ^{:doc "A type that can be used to create a sequence of member type x
         with count greater than 0."
    :forms '[(NonEmptySeqable t)]}
  t/NonEmptySeqable 
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seqable x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "A type that can be used to create a sequence of member type x
         with count 0."
    :forms '[(EmptySeqable t)]}
  t/EmptySeqable
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seqable x)
              t/EmptyCount)))

(defalias
  ^{:doc "A persistent sequence of member type x."
    :forms '[(Seq t)]}
  t/Seq
  (t/TFn [[x :variance :covariant]]
         (clojure.lang.ISeq x)))

(defalias
  ^{:doc "A persistent sequence of member type x with count greater than 0."
    :forms '[(NonEmptySeq t)]}
  t/NonEmptySeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seq x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "A persistent sequence of member type x with count greater than 0, or nil."
    :forms '[(NilableNonEmptySeq t)]}
  t/NilableNonEmptySeq
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           (t/NonEmptySeq x))))

(defalias
  ^{:doc "A hierarchy for use with derive, isa? etc."
    :forms '[Hierarchy]}
  t/Hierarchy
  '{:parents (t/Map t/Any t/Any)
    :ancestors (t/Map t/Any t/Any)
    :descendants (t/Map t/Any t/Any)})

(defalias
  ^{:doc "A Clojure derefable (see clojure.core/deref)."
    :forms '[(Deref t)]}
  t/Deref
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IDeref x)))

(defalias
  ^{:doc "A Clojure future (see clojure.core/{future-call,future})."
    :forms '[(Future t)]}
  t/Future 
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Deref x)
              (clojure.lang.IBlockingDeref x)
              clojure.lang.IPending
              java.util.concurrent.Future)))

(defalias
  ^{:doc "A Clojure promise (see clojure.core/{promise,deliver})."
    :forms '[(Promise t)]}
  t/Promise 
  (t/TFn [[x :variance :invariant]]
         (t/Rec [p]
                (t/I (t/Deref x)
                     (clojure.lang.IBlockingDeref x)
                     clojure.lang.IPending
                     ;; FIXME I think this might be an implementation detail.
                     [x -> (t/U nil p)]))))

(defalias
  ^{:doc "A Clojure delay (see clojure.core/{delay,force})."
    :forms '[(Delay t)]}
  t/Delay
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Delay x)
            :cljs (cljs.core.Delay x))))

(defalias
  ^{:doc "A Clojure blocking derefable (see clojure.core/deref)."
    :forms '[(BlockingDeref t)]}
  t/BlockingDeref
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IBlockingDeref x)
            :cljs (cljs.core/IDerefWithTimeout x))))

(defalias
  ^{:doc "A Clojure persistent list."
    :forms '[(List t)]}
  t/List
  (t/TFn [[x :variance :covariant]]
         (clojure.lang.IPersistentList x)))

(defalias
  ^{:doc "A Clojure custom exception type."
    :forms '[ExInfo]}
  t/ExInfo
  (t/I clojure.lang.IExceptionInfo
     RuntimeException))

(defalias
  ^{:doc "A Clojure proxy."
    :forms '[Proxy]}
  t/Proxy
  clojure.lang.IProxy)

; Should c.l.Sorted be parameterised? Is it immutable?
;    ^{:doc "A sorted Clojure collection."
;      :forms '[Sorted]}
;Sorted
;              clojure.lang.Sorted

(defalias
  ^{:doc "A Clojure stack."
    :forms '[(Stack t)]}
  t/Stack
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IPersistentStack x)))

(defalias
  ^{:doc "A sequential collection."
    :forms '[Sequential]}
  t/Sequential
  clojure.lang.Sequential)

(defalias
  ^{:doc "A sequential, seqable collection. Seq's aren't always Sequential."
    :forms '[(SequentialSeqable t)]}
  t/SequentialSeqable
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (t/Seqable x))))

(defalias
  ^{:doc "A Clojure sequential sequence. Seq's aren't always Sequential."
    :forms '[(SequentialSeq t)]}
  t/SequentialSeq
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (clojure.lang.ISeq x))))

(defalias
  ^{:doc "A sequential seq returned from clojure.core/seq"
    :forms '[(ASeq t)]}
  t/ASeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/SequentialSeq x)
              (Iterable x)
              (java.util.Collection x)
              (java.util.List x)
              clojure.lang.IObj)))

(defalias
  ^{:doc "A sequential non-empty seq retured from clojure.core/seq"
    :forms '[(NonEmptyASeq t)]}
  t/NonEmptyASeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/ASeq x)
              t/NonEmptyCount)))

(defalias
  ^{:doc "The result of clojure.core/seq."
    :forms '[(NilableNonEmptyASeq t)]}
  t/NilableNonEmptyASeq
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           (t/NonEmptyASeq x))))

(defalias
  ^{:doc "A type that returns true for clojure.core/fn?"
    :forms '[Fn]}
  t/Fn
  clojure.lang.Fn)

(defalias
  ^{:doc "A Clojure multimethod."
    :forms '[Multi]}
  t/Multi
  clojure.lang.MultiFn)

(defalias
  ^{:doc "A reducer function with accumulator a and reduces over collections of b"
    :forms '[(Reducer a b)]}
  t/Reducer
  (t/TFn [[a :variance :contravariant]
          [b :variance :invariant]]
         (t/IFn 
           ;init
           [:-> b]
           ;complete
           [b :-> b]
           ;step
           [b a :-> (t/U b (clojure.lang.Reduced b))])))

(defalias
  ^{:doc "A transducer function that transforms in to out."
    :forms '[(Transducer in out)]}
  t/Transducer
  (t/TFn [[in :variance :contravariant]
          [out :variance :covariant]]
         (t/All [r]
                [(t/Reducer out r) :-> (t/Reducer in r)])))

;; Predicate support for common classes

#?(:clj
   (clojure.core.typed/rclass-preds
     ;  clojure.lang.Seqable 
     ;  {:pred (fn [this a?]
     ;           (cond 
     ;             (string? this) (every? a? this)
     ;             (coll? this) (every? a? this)))}
     clojure.lang.IPersistentCollection
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.ISeq
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.IPersistentSet
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.APersistentSet
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.PersistentHashSet
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.PersistentTreeSet
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.Associative
     {:args #{2}
      :pred (fn [this a? b?]
              `(cond
                 (vector? ~this) (and (every? ~a? (range (count ~this)))
                                      (every? ~b? ~this))
                 (map? ~this) (and (every? ~a? (keys ~this))
                                   (every? ~b? (vals ~this)))))}
     clojure.lang.IPersistentStack
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.IPersistentVector
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.APersistentVector
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.PersistentVector
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.IMapEntry
     {:args #{2}
      :pred (fn [this a? b?] 
              `(and (~a? (key ~this)) (~b? (val ~this))))}
     clojure.lang.AMapEntry
     {:args #{2}
      :pred (fn [this a? b?] 
              `(and (~a? (key ~this)) (~b? (val ~this))))}
     clojure.lang.MapEntry
     {:args #{2}
      :pred (fn [this a? b?] 
              `(and (~a? (key ~this)) (~b? (val ~this))))}
     clojure.lang.IPersistentMap
     {:args #{2}
      :pred (fn [this a? b?] 
              `(and (every? ~a? (keys ~this))
                    (every? ~b? (vals ~this))))}
     clojure.lang.ASeq
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.APersistentMap
     {:args #{2}
      :pred (fn [this a? b?] 
              `(and (every? ~a? (keys ~this))
                    (every? ~b? (vals ~this))))}
     clojure.lang.PersistentHashMap
     {:args #{2}
      :pred (fn [this a? b?] 
              `(and (every? ~a? (keys ~this))
                    (every? ~b? (vals ~this))))}
     clojure.lang.Cons
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.IPersistentList
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.PersistentList
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.LazySeq
     {:args #{1}
      :pred (fn [this a?] 
              `(every? ~a? ~this))}
     clojure.lang.Reduced
     {:args #{1}
      :pred (fn [this a?] 
              `(~a? (deref ~this)))}))

;; Var annotations

(defmacro ^:private anns [& body]
  (let [pairs (partition 2 body)
        _ (assert (even? (count body))
                  (vec (first (filter (comp (complement qualified-symbol?) first)
                                      pairs))))]
    `(do ~@(map (fn [[n t]]
                  (assert (qualified-symbol? n) [n t])
                  `(t/ann ~n ~t))
                pairs))))

(anns
#?@(:cljs [] :default [
clojure.core.typed/check-ns (t/IFn [t/Sym -> t/Any]
                                   [-> t/Any])
;; Internal annotations
;clojure.core.typed.current-impl/*current-impl* t/Any
clojure.core.typed.current-impl/clojure t/Any
clojure.core.typed.current-impl/clojurescript t/Any
clojure.core.typed/ann* [t/Any t/Any t/Any -> t/Any]
clojure.core.typed/untyped-var* [t/Any t/Any -> t/Any]
clojure.core.typed/declare-names* [t/Any -> t/Any]
clojure.core.typed/typed-deps* [t/Any -> t/Any]
clojure.core.typed/warn-on-unannotated-vars* [-> t/Any]
clojure.core.typed/ann-datatype* [t/Any t/Any t/Any t/Any -> t/Any]
clojure.core.typed/ann-protocol* [t/Any t/Any t/Any -> t/Any]
clojure.core.typed/ann-record* [t/Any t/Any t/Any t/Any -> t/Any]
clojure.core.typed/ann-pdatatype* [t/Any t/Any t/Any t/Any -> t/Any]
clojure.core.typed/declare-datatypes* [t/Any -> t/Any]
clojure.core.typed/declare-protocols* [t/Any -> t/Any]
clojure.core.typed/non-nil-return* [t/Any t/Any -> t/Any]
clojure.core.typed/nilable-param* [t/Any t/Any -> t/Any]
clojure.core.typed/override-constructor* [t/Any t/Any -> t/Any]
clojure.core.typed/override-method* [t/Any t/Any -> t/Any]
clojure.core.typed/typed-deps* [t/Any -> t/Any]
clojure.core.typed/load-if-needed [-> t/Any]
clojure.core.typed/*collect-on-eval* t/Any
; should always be special cased
;clojure.core.typed/var>* [t/Any -> (t/Var2 t/Nothing t/Any)]
])

;; core annotations

cc/*ns* t/Namespace

#?@(:cljs [] :default [
cc/pop-thread-bindings [-> t/Any]
cc/load [t/Str * -> t/Any]
cc/read-string [t/Str -> t/Any]
cc/read (t/IFn [-> t/Any]
               [java.io.Reader -> t/Any]
               [java.io.Reader t/Bool t/Any -> t/Any]
               [java.io.Reader t/Bool t/Any t/Bool -> t/Any])
cc/read-line [-> (t/U nil t/Str)]
cc/add-classpath [(t/U t/Str java.net.URL) -> nil]
])

cc/*1 t/Any
cc/*2 t/Any
cc/*3 t/Any
cc/*e (t/U nil Throwable)
cc/*agent* (t/U nil (t/Agent2 t/Nothing t/Any))
cc/*allow-unresolved-vars* t/Any
cc/*assert* t/Any
cc/*data-readers* (t/Map t/Sym (t/Var2 t/Nothing t/Any))
cc/*default-data-reader-fn* (t/U nil [t/Any t/Any -> t/Any])
cc/*fn-loader* t/Any
cc/*math-context* t/Any
cc/*source-path* t/Str
cc/*use-context-classloader* t/Any

cc/alength [(ReadOnlyArray t/Any) -> t/AnyInteger]
cc/aclone (t/All [x] [(ReadOnlyArray x) -> (Array x)])
cc/aget (t/All [x]
               (t/IFn [(ReadOnlyArray x) 
                       t/AnyInteger -> x]
                      [(ReadOnlyArray (ReadOnlyArray x)) 
                       t/AnyInteger t/AnyInteger -> x]
                      [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x))) 
                       t/AnyInteger t/AnyInteger t/AnyInteger -> x]
                      [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))) 
                       t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger -> x]
                      ; don't support unsound cases
                      [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))))
                       t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger -> x]))

cc/aset
(t/All [x]
  (t/IFn
    [(Array x) t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x -> x]))

cc/macroexpand-1 [t/Any -> t/Any]
cc/macroexpand [t/Any -> t/Any]

cc/create-struct [t/Any * -> (t/Map t/Any t/Any)]

cc/find-ns [t/Sym -> (t/Nilable t/Namespace)]
cc/create-ns [t/Sym -> t/Namespace]
#?@(:cljs [] :default [
cc/remove-ns [t/Sym -> t/Namespace]
])
cc/namespace [(t/U t/Sym t/Keyword) -> (t/Nilable t/Str)]
cc/ns-name [(t/U t/Sym t/Namespace) -> t/Sym]
cc/ns-map [(t/U t/Sym t/Namespace) -> t/Sym]
cc/ns-aliases [(t/U t/Sym t/Namespace) -> (t/Map t/Sym t/Namespace)]
cc/name [(t/U t/Str Named) -> t/Str]
cc/the-ns [(t/U t/Sym t/Namespace) -> t/Namespace]
cc/in-ns [t/Sym -> nil]
cc/import [t/Any * -> nil]
cc/identity (t/All [x] [x -> x
                        :filters {:then (! (t/U nil false) 0)
                                  :else (is (t/U nil false) 0)}
                        :object {:id 0}])
cc/gensym (t/IFn [-> t/Sym]
                 [(t/U t/Sym t/Str) -> t/Sym])
#?@(:cljs [] :default [
cc/intern (t/IFn [(t/U t/Sym t/Namespace) t/Sym -> (t/Var2 t/Nothing t/Any)]
                 [(t/U t/Sym t/Namespace) t/Sym t/Any -> (t/Var2 t/Nothing t/Any)])
])


cc/doall (t/All [[c :< (t/U nil (t/Seqable t/Any))]]
                (t/IFn [c -> c]
                       [t/AnyInteger c -> c]))
cc/dorun (t/IFn [(t/Seqable t/Any) -> nil]
                [t/AnyInteger (t/Seqable t/Any) -> nil])
cc/iterate (t/All [x]
                  [[x -> x] x -> (t/ASeq x)])
cc/memoize (t/All [x y ...]
                  [[y ... y -> x] -> [y ... y -> x]])

cc/key (t/All [x] [(IMapEntry x t/Any) -> x])
cc/val (t/All [x] [(IMapEntry t/Any x) -> x])

;cc/juxt
;(t/All [b1 ...]
;(t/All [x r b2 ...]
;     (Fn [[b1 ... b1 -> b2] ... b2 -> [b1 ... b1 -> '[b2 ... b2]]]
;         [[b1 ... b1 -> r] * -> [b1 ... b1 -> (t/Vec r)]]
;         [[x * -> b2] ... b2 -> [x * -> '[b2 ... b2]]]
;         [[x * -> r] * -> [x * -> (t/Vec r)]])))


;TODO flip filters
cc/complement (t/All [x] [[x -> t/Any] -> [x -> t/Bool]])
; should preserve filters
cc/boolean [t/Any -> t/Bool]

cc/filter (t/All [x y] (t/IFn
                         [[x -> t/Any :filters {:then (is y 0)}] (t/Seqable x) -> (t/ASeq y)]
                         [[x -> t/Any :filters {:then (! y 0)}] (t/Seqable x) -> (t/ASeq (t/I x (t/Not y)))]
                         [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)]))
cc/filterv (t/All [x y] (t/IFn
                          [[x -> t/Any :filters {:then (is y 0)}] (t/Seqable x) -> (t/AVec y)]
                          [[x -> t/Any] (t/Seqable x) -> (t/AVec x)]))
cc/remove (t/All [x y] (t/IFn
                         [[x -> t/Any :filters {:else (is y 0)}] (t/Seqable x) -> (t/ASeq y)]
                         [[x -> t/Any :filters {:else (! y 0)}] (t/Seqable x) -> (t/ASeq (t/I x (t/Not y)))]
                         [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)]))


cc/take-while (t/All [x y] [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)])
cc/drop-while (t/All [x] [[x -> t/Any] (t/Seqable x) -> (t/ASeq x)])

cc/split-with (t/All [x y z] (t/IFn
                               [[x -> t/Any :filters {:then (is y 0), :else (is z 0)}] (t/Seqable x) -> '[(t/ASeq y) (t/ASeq z)]]
                               [[x -> t/Any] (t/Seqable x) -> '[(t/ASeq x) (t/ASeq x)]]))

cc/split-at (t/All [x y z] [t/AnyInteger (t/Seqable x) -> '[(t/ASeq x) (t/ASeq x)]])

cc/partition-all (t/All [x] (t/IFn [t/Int (t/Seqable x) -> (t/ASeq (t/ASeq x))] 
                                   [t/Int t/Int (t/Seqable x) -> (t/ASeq (t/ASeq x))]))

cc/partition (All [a] (IFn [t/AnyInteger (t/Seqable a) -> (t/ASeq (t/ASeq a))]
                           [t/AnyInteger t/AnyInteger (t/Seqable a) -> (t/ASeq (t/ASeq a))]
                           [t/AnyInteger t/AnyInteger t/AnyInteger (t/Seqable a) -> (t/ASeq (t/ASeq a))]))

cc/repeatedly (t/All [x] (t/IFn [[-> x] -> (t/ASeq x)]
                                [t/AnyInteger [-> x] -> (t/ASeq x)]))


cc/some (t/All [x y] [[x -> y] (t/Seqable x) -> (t/Option y)])

; Unions need to support dots for this to work:
;
; (t/All [t0 b ...]
;    (t/IFn [[t/Any -> t/Any :filters {:then (is t0 0) :else (! t0 0)}] 
;         [t/Any -> t/Any :filters {:then (is b 0) :else (! b 0)}] ... b
;         -> (t/IFn [t/Any -> t/Any :filters {:then (is (t/U t0 b ... b) 0) :else (! (t/U t0 b ... b) 0)}]
;                [t/Any * -> t/Any])]))
cc/some-fn 
(t/All [t0 t1 t2 t3 t4 t5]
       (t/IFn [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1) 0) :else (! (t/U t0 t1) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2) 0) :else (! (t/U t0 t1 t2) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2 t3) 0) :else (! (t/U t0 t1 t2 t3) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any -> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2 t3 t4) 0) :else (! (t/U t0 t1 t2 t3 t4) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any -> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               [t/Any -> t/Bool :filters {:then (is t5 0) :else (! t5 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/U t0 t1 t2 t3 t4 t5) 0) :else (! (t/U t0 t1 t2 t3 t4 t5) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Any] [t/Any -> t/Any] * -> [t/Any * -> t/Any]]))
cc/every-pred
(t/All [t0 t1 t2 t3 t4 t5]
       (t/IFn [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1) 0) :else (! (t/I t0 t1) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2) 0) :else (! (t/I t0 t1 t2) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2 t3) 0) :else (! (t/I t0 t1 t2 t3) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any -> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2 t3 t4) 0) :else (! (t/I t0 t1 t2 t3 t4) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Any :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any -> t/Any :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any -> t/Any :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any -> t/Any :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any -> t/Any :filters {:then (is t4 0) :else (! t4 0)}]
               [t/Any -> t/Any :filters {:then (is t5 0) :else (! t5 0)}]
               -> (t/IFn [t/Any -> t/Bool :filters {:then (is (t/I t0 t1 t2 t3 t4 t5) 0) :else (! (t/I t0 t1 t2 t3 t4 t5) 0)}]
                         [t/Any * -> t/Any])]
              [[t/Any -> t/Any] [t/Any -> t/Any] * -> [t/Any * -> t/Any]]))

cc/concat (t/All [x] [(t/Seqable x) * -> (t/ASeq x)])

cc/set (t/All [x] [(t/Seqable x) -> #?(:cljs (t/Set x)
                                       :default (PersistentHashSet x))])
cc/hash-set (t/All [x] [x * -> #?(:cljs (t/Set x)
                                  :default (PersistentHashSet x))])
cc/hash-map (t/All [x y] [(t/HSequential [x y] :repeat true) <* -> (t/Map x y)])
cc/sorted-map (t/All [x y] [(t/HSequential [x y] :repeat true) <* -> (t/Map x y)])
cc/sorted-set (t/All [x] [x * -> #?(:cljs (t/SortedSet x)
                                    :default (PersistentTreeSet x))])
cc/sorted-set-by (t/All [x] [[x x -> t/AnyInteger] x * -> #?(:cljs (t/Set x)
                                                             :default (PersistentTreeSet x))])
cc/list (t/All [x] [x * -> (PersistentList x)])
cc/list* (t/All [x] (t/IFn [(t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]
                           [x x x x x x x x x x (t/Seqable x) -> (t/NilableNonEmptyASeq x)]))

cc/list? (t/Pred (t/List t/Any))
#?@(:cljs [] :default [
cc/load-reader [java.io.Reader -> t/Any]
])

cc/methods [t/Multi -> (t/Map t/Any t/Any)]

cc/munge (t/IFn [t/Sym -> t/Sym]
                       [t/Any -> t/Any])

cc/pos? [t/Num -> t/Bool]
cc/neg? [t/Num -> t/Bool]

cc/nthrest (t/All [x] [(t/Seqable x) t/AnyInteger 
                               -> (t/ASeq x)])

cc/vector (t/All [r b ...]
                           (t/IFn [b ... b -> '[b ... b]]
                                  [r * -> (t/AVec r)]))
cc/vec (t/All [x] [(t/Seqable x) -> (t/AVec x)])

cc/not [t/Any -> t/Bool]
cc/constantly (t/All [x] [x -> [t/Any * -> x]])

#?@(:cljs [] :default [
cc/bound? [(t/Var2 t/Nothing t/Any) * -> t/Bool]
cc/thread-bound? [(t/Var2 t/Nothing t/Any) * -> t/Bool]
cc/bases [(t/Nilable Class) -> (t/NilableNonEmptyASeq Class)]
])

cc/make-hierarchy [-> t/Hierarchy]
cc/isa? (t/IFn [t/Any t/Any -> t/Bool]
               [t/Hierarchy t/Any t/Any -> t/Bool])

cc/disj (t/All [x] (t/IFn [(t/SortedSet x) t/Any t/Any * -> (t/SortedSet x)]
                          [(t/Set x) t/Any t/Any * -> (t/Set x)]))

cc/assoc
(t/All [m k v c ...] (t/IFn [m k v (t/HSeq [c c] :repeat true) <... c -> (t/Assoc m k v c ... c)]
                            ;[m k v (t/HSeq [k v] :repeat true) <* -> (t/Assoc m k v)]
                            [nil k v (t/HSeq [c c] :repeat true) <... c -> (t/Assoc nil k v c ... c)]
                            [nil k v (t/HSeq [k v] :repeat true) <* -> (t/Map k v)]))
;     (t/All [b c d]
;       (Fn [(t/Map b c) b c -> (t/Map b c)]
;           [(t/Vec d) t/AnyInteger d -> (t/Vec d)]
;           [d b c (t/HSequential [b c] :repeat true) <* -> (t/Assoc d b c)]))

cc/dissoc (t/All [k v] [(t/Map k v) t/Any * -> (t/Map k v)])

cc/zipmap (t/All [k v] [(t/Seqable k) (t/Seqable v) -> #?(:cljs (t/Map k v)
                                                          :default (APersistentMap k v))])

cc/keys (t/All [k] [(t/Map k t/Any) -> (t/ASeq k) :object {:id 0 :path [Keys]}])
cc/vals (t/All [v] [(t/Map t/Any v) -> (t/ASeq v) :object {:id 0 :path [Vals]}])

;most useful case
cc/comp (t/All [x y b ...] [[x -> y] [b ... b -> x] -> [b ... b -> y]])


;apply: wishful thinking
;     (t/All [b1 ...]
;     (t/All [y b2 ...]
;          (t/IFn [[b1 ... b1 b2 ... b2 -> y] b1 ... b1 (t/HSequential [b2 ... b2]) -> y]
;              [[b1 ... b1 r * -> y] b1 ... b1 (t/Seqable r) -> y])))

cc/apply
(t/All [y a b c d r z ...]
       (t/IFn [[z ... z -> y] (t/U nil (t/HSequential [z ... z])) -> y]
              [[a z ... z -> y] a (t/U nil (t/HSequential [z ... z])) -> y]
              [[a b z ... z -> y] a b (t/U nil (t/HSequential [z ... z])) -> y]
              [[a b c z ... z -> y] a b c (t/U nil (t/HSequential [z ... z])) -> y]
              [[a b c d z ... z -> y] a b c d (t/U nil (t/HSequential [z ... z])) -> y]
              [[r * -> y] (t/Seqable r) -> y]
              [[a r * -> y] a (t/Seqable r) -> y]
              [[a b r * -> y] a b (t/Seqable r) -> y]
              [[a b c r * -> y] a b c (t/Seqable r) -> y]
              [[a b c d r * -> y] a b c d (t/Seqable r) -> y]))

;partial: wishful thinking (replaces the first 4 arities)
; (t/All [b1 ...]
; (t/All [r b2 ...]
;    [[b1 ... b1 b2 ... b2 -> r] b1 ... b1 -> [b2 ... b2 -> r]]))

cc/partial
(t/All [y a b c d z ...]
       (t/IFn [[z ... z -> y] -> [z ... z -> y]]
              [[a z ... z -> y] a -> [z ... z -> y]]
              [[a b z ... z -> y] a b -> [z ... z -> y]]
              [[a b c z ... z -> y] a b c -> [z ... z -> y]]
              [[a b c d z ... z -> y] a b c d -> [z ... z -> y]]
              [[a * -> y] a * -> [a * -> y]]))

cc/str [t/Any * -> t/Str]
cc/prn-str [t/Any * -> t/Str]
cc/pr-str [t/Any * -> t/Str]
cc/newline [-> nil]

cc/print [t/Any * -> nil]
cc/println [t/Any * -> nil]
cc/print-str [t/Any * -> t/Str]
cc/println-str [t/Any * -> t/Str]
cc/printf [t/Str t/Any * -> nil]
cc/format [t/Str t/Any  * -> t/Str]
cc/pr [t/Any * -> nil]
cc/prn [t/Any * -> nil]
cc/flush [-> nil]
cc/*print-length* (t/U nil false t/AnyInteger)
cc/*print-level* (t/U nil false t/AnyInteger)
cc/*verbose-defrecords* t/Bool
cc/print-ctor [Object [Object java.io.Writer -> t/Any] java.io.Writer -> nil]

cc/prefer-method [t/Multi t/Any t/Any -> t/Any]
#?@(:cljs [] :default [
cc/print-simple [t/Any java.io.Writer -> nil]
cc/char-escape-string (t/Map Character t/Str)
cc/char-name-string (t/Map Character t/Str)
cc/primitives-classnames (t/Map Class t/Str)
cc/namespace-munge [(t/U t/Sym t/Namespace) -> t/Str]
;cc/find-protocol-impl ['{:on-interface Class
;                                   :impls ?}]

cc/re-matcher [java.util.regex.Pattern t/Str -> java.util.regex.Matcher]
cc/re-groups [java.util.regex.Matcher -> (t/U nil t/Str (t/Vec (t/Option t/Str)))]
])

cc/re-find (t/IFn [java.util.regex.Matcher -> (t/U nil t/Str (t/Vec (t/Option t/Str)))]
                  [java.util.regex.Pattern t/Str -> (t/U nil t/Str (t/Vec (t/Option t/Str)))])
cc/re-seq [java.util.regex.Pattern t/Str -> (t/ASeq (t/U nil t/Str (t/Vec (t/Option t/Str))))]

cc/subs (t/IFn [t/Str t/AnyInteger -> t/Str]
               [t/Str t/AnyInteger t/AnyInteger -> t/Str])

;TODO
;cc/spit [java.io.Writer t/Any]

#?@(:cljs [] :default [
cc/future-call (t/All [x] [[-> x] -> (t/Future x)])
])

cc/atom (t/All [x] [x & :optional {:validator (t/Nilable [x -> t/Any]) :meta t/Any} -> (t/Atom2 x x)])

cc/deref (t/All [x y] (t/IFn 
                        [(t/Deref x) -> x]
                        #?(:clj [(t/U (t/Deref t/Any) java.util.concurrent.Future) -> t/Any])
                        [(t/BlockingDeref x) t/AnyInteger y -> (t/U x y)]
                        #?(:clj [(t/U java.util.concurrent.Future (t/BlockingDeref t/Any)) t/AnyInteger t/Any -> t/Any])))

cc/delay? (t/Pred (t/Delay t/Any))

#?@(:cljs [] :default [
cc/future-cancelled? [java.util.concurrent.Future -> t/Bool]
cc/future-cancel [java.util.concurrent.Future -> t/Any]
cc/future? (t/Pred java.util.concurrent.Future)
cc/future-done? [java.util.concurrent.Future -> t/Bool]
])

cc/force (t/All [x] (t/IFn [(t/Delay x) -> x]
                           [t/Any -> t/Any]))

;; TODO cljs
cc/realized? [clojure.lang.IPending -> t/Bool]

cc/select-keys (t/All [k v] [(t/Map k v) (t/Seqable t/Any) -> (t/Map k v)])

cc/sort (t/All [x] (t/IFn [(t/Seqable x) -> (t/ASeq x)]
                          [(t/I Comparator [x x -> t/AnyInteger]) (t/Seqable x) -> (t/ASeq x)]))
cc/sort-by (t/All [a] (t/IFn [(t/Seqable a) -> (t/ASeq a)]
                             [[a -> Number] (t/Seqable a) -> (ASeq a)]))
cc/replicate (t/All [a] [t/AnyInteger a -> (t/ASeq a)])

cc/reset! (t/All [w r] [(t/Atom2 w r) w -> w])
cc/swap! (t/All [w r b ...] [(t/Atom2 w r) [r b ... b -> w] b ... b -> w])
cc/compare-and-set! (t/All [w] [(t/Atom2 w t/Any) t/Any w -> t/Bool])

cc/set-validator! (t/All [x] [(clojure.lang.IRef x x) (t/Nilable [x -> t/Any]) -> t/Any])
cc/get-validator (t/All [x] [(clojure.lang.IRef x x) -> (t/Nilable [x -> t/Any])])

#?@(:cljs [] :default [
cc/alter-var-root (t/All [w r b ...] [(t/Var2 w r) [r b ... b -> w] b ... b -> w])

cc/method-sig [java.lang.reflect.Method -> '[t/Any (t/NilableNonEmptySeqable t/Any) t/Any]]
cc/proxy-name [Class (t/Seqable Class) -> t/Str]
cc/get-proxy-class [Class * -> Class]
cc/construct-proxy [Class t/Any * -> t/Any]
cc/init-proxy [t/Proxy (t/Map t/Str t/Any) -> t/Proxy]
cc/update-proxy [t/Proxy (t/Map t/Str t/Any) -> t/Proxy]
cc/proxy-mappings [t/Proxy -> (t/Map t/Str t/Any)]
cc/proxy-call-with-super (t/All [x] [[-> x] t/Proxy t/Str -> x])
cc/bean [Object -> (t/Map t/Any t/Any)]
])

cc/fnil (t/All [x y z a b ...] (t/IFn [[x b ... b -> a] x -> [(t/Nilable x) b ... b -> a]]
                                      [[x y b ... b -> a] x y -> [(t/Nilable x) (t/Nilable y) b ... b -> a]]
                                      [[x y z b ... b -> a] x y z -> [(t/Nilable x) (t/Nilable y) (t/Nilable z) b ... b -> a]]))

cc/symbol (t/IFn [(t/U t/Sym t/Str) -> t/Sym]
                 [(t/U nil t/Str) t/Str -> t/Sym])

cc/keyword
(t/IFn [(t/U t/Keyword t/Sym t/Str) -> t/Keyword 
        :object {:id 0 :path [Keyword]}
        :filters {:then tt
                  :else ff}]
       [nil -> nil 
        :object {:id 0 :path [Keyword]}
        :filters {:then ff
                  :else tt}]
       [t/Any -> (t/U nil t/Keyword) 
        :object {:id 0 :path [Keyword]}
        :filters {:then (is (t/U t/Keyword t/Sym t/Str) 0)
                  :else (! (t/U t/Keyword t/Sym t/Str) 0)}]
       [t/Str t/Str -> t/Keyword
        :filters {:then tt
                  :else ff}])

#?@(:cljs [] :default [
cc/find-keyword
(t/IFn [(t/U t/Keyword t/Sym t/Str) -> (t/Option t/Keyword)]
       [t/Str t/Str -> (t/Option t/Keyword)])
])

cc/derive (t/IFn [(t/U t/Sym t/Keyword Class) (t/U t/Sym t/Keyword) -> nil]
                 [t/Hierarchy (t/U t/Sym t/Keyword Class) (t/U t/Sym t/Keyword) -> t/Hierarchy])

cc/compare [t/Any t/Any -> t/Num]

#?@(:cljs [] :default [
cc/require [t/Any * -> nil]
cc/use [t/Any * -> nil]
cc/refer [t/Sym & :optional {:exclude (t/Seqable t/Sym)
                                        :only (t/Seqable t/Sym)
                                        :rename (t/Map t/Sym t/Sym)}
                    -> nil]
cc/*loaded-libs* (t/Ref1 (t/Set t/Sym))
])

cc/seq? (t/Pred (t/Seq t/Any))
cc/set? (t/Pred (t/Set t/Any))
cc/vector? (t/Pred (t/Vec t/Any))
cc/nil? (t/Pred nil)
cc/false? (t/Pred false)
cc/true? (t/Pred true)
cc/symbol? (t/Pred t/Sym)
cc/keyword? (t/Pred t/Keyword)
cc/map? (t/Pred (t/Map t/Any t/Any))
cc/boolean? (t/Pred t/Bool)

; would be nice
; (t/Pred (t/Not nil))
cc/some? [t/Any -> t/Bool :filters {:then (! nil 0)
                                    :else (is nil 0)}]

#?@(:cljs [] :default [
cc/cast (t/All [x] [Class x -> x])
])

cc/associative? (t/Pred (t/Associative t/Any t/Any t/Any))
cc/coll? (t/Pred (t/Coll t/Any))
      ;TODO should these be parameterized?
cc/sequential? (t/Pred t/Sequential)
;cc/sorted? (t/Pred Sorted)
cc/meta [t/Any -> (t/Nilable (t/Map t/Any t/Any))]
cc/with-meta (t/All [[x :< clojure.lang.IObj]]
                    [x (t/Nilable (t/Map t/Any t/Any)) -> x])
cc/vary-meta (t/All [[x :< clojure.lang.IObj] b ...]
                    [x [(t/Nilable (t/Map t/Any t/Any)) b ... b -> (t/Nilable (t/Map t/Any t/Any))] b ... b -> x])

cc/reset-meta! [clojure.lang.IReference (t/Nilable (t/Map t/Any t/Any)) -> (t/Nilable (t/Map t/Any t/Any))]
cc/alter-meta! 
(t/All [b ...]
       [clojure.lang.IReference
        [(t/Nilable (t/Map t/Any t/Any)) b ... b ->
         (t/Nilable (t/Map t/Any t/Any))] b ... b -> (t/Nilable (t/Map t/Any t/Any))])

#?@(:cljs [] :default [
cc/commute (t/All [w r b ...] [(t/Ref2 w r) [r b ... b -> w] b ... b -> w])
cc/alter (t/All [w r b ...] [(t/Ref2 w r) [r b ... b -> w] b ... b -> w])
])

cc/cycle (t/All [x] [(t/Seqable x) -> (t/ASeq x)])

#?@(:cljs [] :default [
cc/compile [t/Sym -> t/Sym]
])

cc/comparator (t/All [x y] [[x y -> t/Any] -> (t/I Comparator [x y -> t/AnyInteger])])

#?@(:cljs [] :default [
cc/destructure [t/Any -> t/Any]
])

cc/distinct (t/All [x] [(t/Seqable x) -> (t/ASeq x)])

cc/string? (t/Pred t/Str)
cc/char? (t/Pred Character)

clojure.string/split
(t/IFn [t/Str java.util.regex.Pattern -> (t/AVec t/Str)]
       [t/Str java.util.regex.Pattern t/AnyInteger -> (t/AVec t/Str)])

clojure.string/join
(t/IFn [(t/Seqable t/Any) -> t/Str]
       [t/Any (t/Seqable t/Any) -> t/Str])

clojure.string/upper-case [CharSequence -> t/Str]

clojure.string/blank? [(t/U nil t/Str) -> t/Bool]
clojure.string/capitalize [t/Str -> t/Str]
clojure.string/lower-case [t/Str -> t/Str]
clojure.string/replace (t/IFn [t/Str t/Str t/Str -> t/Str]  [t/Str Character Character -> t/Str]
                              [t/Str java.util.regex.Pattern (t/U t/Str [t/Str -> t/Str]) -> t/Str])
clojure.string/replace-first (t/IFn [t/Str t/Str t/Str -> t/Str]
                                    [t/Str Character Character -> t/Str]
                                    [t/Str java.util.regex.Pattern (t/U t/Str [t/Str -> t/Str]) -> t/Str])
clojure.string/reverse [t/Str -> t/Str]
clojure.string/trim [t/Str -> t/Str]
clojure.string/trimr [t/Str -> t/Str]
clojure.string/triml [t/Str -> t/Str]

clojure.data/diff [t/Any t/Any -> '[t/Any t/Any t/Any]]

#?@(:cljs [] :default [
cljs.instant/read-instant-instant [CharSequence -> java.sql.Timestamp] ;; clj macros file
clojure.instant/read-instant-date [t/Str -> java.util.Date]
clojure.instant/read-instant-calendar [t/Str -> java.util.GregorianCalendar]
clojure.instant/read-instant-timestamp [t/Str -> java.sql.Timestamp]
])

#?@(:cljs [] :default [
clojure.repl/apropos [(t/U t/Str java.util.regex.Pattern) -> (t/Seq t/Sym)]
clojure.repl/demunge [t/Str -> t/Str]
clojure.repl/source-fn [t/Sym -> (t/U t/Str nil)]
])

#?@(:cljs [] :default [
clojure.template/apply-template [(t/Vec t/Any) t/Any (t/Seqable t/Any) -> t/Any]
])

clojure.set/subset? [(t/Set t/Any) (t/Set t/Any) -> t/Bool]
clojure.set/superset? [(t/Set t/Any) (t/Set t/Any) -> t/Bool]
clojure.set/join (t/IFn [(t/Set (t/Map t/Any t/Any)) (t/Set (t/Map t/Any t/Any)) -> (t/Set (t/Map t/Any t/Any))]
                        [(t/Set (t/Map t/Any t/Any)) (t/Set (t/Map t/Any t/Any)) (t/Map t/Any t/Any) -> (t/Set (t/Map t/Any t/Any))])

 ; would be nice
;(t/All [[m :> (t/Map t/Any t/Any)] k]
;     [(t/Set m) (t/Seqable k) -> (t/Map (t/Map k (Get m k)) (t/Set m))]
;     )
clojure.set/index (t/All [x y] [(t/Set (t/Map x y)) (t/Seqable t/Any) -> (t/Map (t/Map t/Any t/Any) (t/Set (t/Map x y)))])
clojure.set/map-invert (t/All [a b] [(t/Map a b) -> (t/Map b a)])

 ;would be nice, not quite correct though
; (t/All [x y [m :< (t/Map x y)] k]
;    [(t/Set m) (t/Vec k) -> (t/Set (t/Map k (Get m k)))])
clojure.set/project (t/All [x y] [(t/Set (t/Map x y)) (t/Vec t/Any) -> (t/Set (t/Map x y))])
clojure.set/rename (t/All [x y] [(t/Set (t/Map x y)) (t/Map t/Any x) -> (t/Set (t/Map x y))])
clojure.set/rename-keys (t/All [x y] [(t/Map x y) (t/Map t/Any x) -> (t/Map x y)])
 ;like filter
clojure.set/select (t/All [x y] (t/IFn [[x -> t/Any :filters {:then (is y 0)}] (t/Set x) -> (t/Set y)]
                                       [[x -> t/Any :filters {:then (! y 0)}] (t/Set x) -> (t/Set (t/I x (t/Not y)))]
                                       [[x -> t/Any] (t/Set x) -> (t/Set x)]))
clojure.set/union (t/All [x] [(t/Set x) * -> (t/Set x)])
clojure.set/intersection (t/All [x] [(t/Set x) (t/Set x) * -> (t/Set x)])
clojure.set/difference (t/All [x] [(t/Set x) (t/Set t/Any) * -> (t/Set x)])
 
; FIXME should be [t/Str [t/Any -> t/Any] -> t/Str]
clojure.string/escape [t/Str (t/U (t/Map t/Any t/Any) [t/Any -> t/Any]) -> t/Str]
clojure.string/split-lines [t/Str -> (t/Vec t/Str)]

#?@(:cljs [] :default [
clojure.test/function? [t/Any -> t/Bool]
clojure.test/assert-any [t/Any t/Any -> t/Any]
clojure.test/do-report [t/Any -> t/Any]
clojure.test/run-tests [t/Sym * -> (t/Map t/Any t/Any)]
clojure.test/run-all-tests (t/IFn [-> (t/Map t/Any t/Any)]
                                [java.util.regex.Pattern * -> (t/Map t/Any t/Any)])
clojure.test/successful? [(t/U nil (t/Map t/Any t/Any)) -> t/Bool]
clojure.test/compose-fixtures [[[-> t/Any] -> t/Any] [[-> t/Any] -> t/Any] -> [[-> t/Any] -> t/Any]]
clojure.test/testing-vars-str [(t/Map t/Any t/Any) -> t/Str]
clojure.test/testing-contexts-str [-> t/Str]
clojure.test/test-ns [(t/U t/Namespace t/Sym) -> (t/Map t/Any t/Any)]

clojure.test.tap/print-tap-plan [t/Any -> t/Any]
clojure.test.tap/print-tap-diagnostic [t/Str -> t/Any]
clojure.test.tap/print-tap-pass [t/Any -> t/Any]
clojure.test.tap/print-tap-fail [t/Any -> t/Any]

clojure.java.javadoc/add-local-javadoc [t/Any -> (t/List t/Any)]
clojure.java.javadoc/add-remote-javadoc [t/Str t/Any -> (t/Map t/Any t/Any)]
clojure.java.javadoc/javadoc [t/Any -> t/Any]
])

clojure.edn/read-string [(t/U t/Str nil) -> t/Any]

#?@(:cljs [] :default [
clojure.java.shell/sh [t/Any *
                       ;would be nice (combine * and kw args)
                       ; t/Str *
                       ;& :optional {:in t/Any  ;; any valid input to clojure.java.io/copy
                       ;             :inc-enc t/Str :out-env (t/U ':bytes t/Str)
                       ;             :env (t/U (Array t/Str) (t/Map t/Any t/Any))
                       ;             :dir (t/U t/Str java.io.File)}
                       -> '{:exit t/Str
                            :out (t/U (Array byte) t/Str)
                            :err t/Str}]
clojure.java.browse/browse-url [t/Any -> t/Any]
clojure.java.io/delete-file (t/IFn [t/Any
                                    ;; FIXME any arg that c.j.io/file accepts
                                    #_String 
                                    -> t/Any]
                                   [t/Any t/Any -> t/Any])
clojure.stacktrace/e [-> t/Any]
clojure.stacktrace/print-cause-trace [Throwable -> t/Any]
clojure.stacktrace/print-stack-trace [Throwable -> t/Any]
clojure.stacktrace/print-throwable [Throwable -> t/Any]
clojure.stacktrace/root-cause [Throwable -> Throwable]
;; FIXME keyword arguments
clojure.reflect/reflect [t/Any t/Any * -> (t/Map t/Any t/Any)]
clojure.inspector/atom? [t/Any -> t/Bool]
clojure.inspector/collection-tag [t/Any -> t/Keyword]
clojure.inspector/tree-model [t/Any -> t/Any]
clojure.inspector/old-table-model [(t/Seqable t/Any) -> t/Any]
clojure.inspector/inspect [t/Any -> javax.swing.JFrame]
clojure.inspector/inspect-tree [t/Any -> javax.swing.JFrame]
clojure.inspector/inspect-table [(t/Seqable t/Any) -> javax.swing.JFrame]
])

#?@(:cljs [] :default [
clojure.main/demunge [t/Str -> t/Str]
clojure.main/repl-prompt [-> t/Any]
clojure.main/repl-read [t/Any t/Any -> t/Any]
clojure.main/repl-caught [Throwable -> t/Any]
clojure.main/repl-exception [Throwable -> t/Any]
clojure.main/root-cause [Throwable -> Exception]
clojure.main/repl [& :optional {:init [-> t/Any]
                                :need-prompt [-> t/Any]
                                :prompt [-> t/Any]
                                :flush [-> t/Any]
                                :read [t/Any t/Any -> t/Any]
                                :eval [t/Any -> t/Any]
                                :print [t/Any -> t/Any]
                                :caught [Throwable -> t/Any]}
                   -> t/Any]
clojure.main/main [t/Any * -> t/Any]
clojure.main/load-script [t/Str -> t/Any]
])

clojure.walk/keywordize-keys [t/Any -> t/Any]
clojure.walk/macroexpand-all [t/Any -> t/Any]
clojure.walk/postwalk [[t/Any -> t/Any] t/Any -> t/Any]
clojure.walk/postwalk-demo [t/Any -> t/Any]
clojure.walk/postwalk-replace [(t/Map t/Any t/Any) t/Any -> t/Any]
clojure.walk/prewalk [[t/Any -> t/Any] t/Any -> t/Any]
clojure.walk/prewalk-demo [t/Any -> t/Any]
clojure.walk/prewalk-replace [(t/Map t/Any t/Any) t/Any -> t/Any]
clojure.walk/stringify-keys [t/Any -> t/Any]
clojure.walk/walk [[t/Any -> t/Any] [t/Any -> t/Any] t/Any -> t/Any]

clojure.zip/zipper [[t/Any -> t/Any] [(t/Seqable t/Any) -> (t/U nil (t/Seq t/Any))] 
                    [t/Any (t/U nil (t/Seq t/Any)) -> t/Any]
                    t/Any 
                    -> (t/Vec t/Any)]
clojure.zip/seq-zip [t/Any -> (t/Vec t/Any)]
clojure.zip/vector-zip [t/Any -> (t/Vec t/Any)]
clojure.zip/xml-zip [t/Any -> (t/Vec t/Any)]
clojure.zip/node [(t/Vec t/Any) -> t/Any]
clojure.zip/branch? [(t/Vec t/Any) -> t/Bool]
clojure.zip/children [(t/Vec t/Any) -> (t/U nil (t/Seq t/Any))]
clojure.zip/root [(t/Vec t/Any) -> t/Any]
clojure.zip/rightmost [(t/Vec t/Any) -> (t/Vec t/Any)]
clojure.zip/right [(t/Vec t/Any) -> t/Any]
clojure.zip/up [(t/Vec t/Any) -> (t/U nil (t/Vec t/Any))]
clojure.zip/rights [(t/Vec t/Any) -> t/Any]
clojure.zip/replace [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/down [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/left [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/lefts [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/leftmost [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
clojure.zip/append-child [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/branch? [(t/Vec t/Any) -> t/Bool]
clojure.zip/end? [(t/Vec t/Any) -> t/Bool]
clojure.zip/insert-child [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/insert-left [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/insert-right [(t/Vec t/Any) t/Any -> (t/Vec t/Any)]
clojure.zip/next [(t/Vec t/Any) -> (t/Vec t/Any)]
clojure.zip/prev [(t/Vec t/Any) -> (t/U (t/Vec t/Any) nil)]
;; more to say here
clojure.zip/path [(t/Vec t/Any) -> t/Any]
clojure.zip/remove [(t/Vec t/Any) -> (t/Vec t/Any)]

cc/interpose (t/All [x] [x (t/Seqable x) -> (t/ASeq x)])
cc/interleave (t/All [x] [(t/Seqable x) (t/Seqable x) (t/Seqable x) * -> (t/ASeq x)])

cc/repeat (t/All [x] (t/IFn [x -> (t/ASeq x)]
                            [t/AnyInteger x -> (t/ASeq x)]))

;cc/every? (t/All [x y] 
;                         (t/IFn [[x -> t/Any :filters {:then (is y 0)}] (t/Coll x) -> t/Bool
;                              :filters {:then (is (t/Coll (t/I x y)) 1)}]
;                             ; argument could be nil
;                             [[x -> t/Any :filters {:then (is y 0)}] (t/U nil (t/Coll x)) -> t/Bool
;                              :filters {:then (is (t/U nil (t/Coll (t/I x y))) 1)}]
;                             [[x -> t/Any] (t/Seqable x) -> t/Bool]))
cc/every? (t/All [x y] (t/IFn [[x -> t/Any :filters {:then (is y 0)}] (t/Coll x) -> t/Bool
                               :filters {:then (is (t/Coll y) 1)}]
                              ; argument could be nil
                              [[x -> t/Any :filters {:then (is y 0)}] (t/U nil (t/Coll x)) -> t/Bool
                               :filters {:then (is (t/U nil (t/Coll y)) 1)}]
                              [[x -> t/Any] (t/Seqable x) -> t/Bool]))

cc/range (t/IFn [-> (t/ASeq t/AnyInteger)]
                [t/Num -> (t/ASeq t/AnyInteger)]
                [t/AnyInteger t/Num -> (t/ASeq t/AnyInteger)]
                [t/Num t/Num -> (t/ASeq t/Num)]
                [t/AnyInteger t/Num t/AnyInteger -> (t/ASeq t/AnyInteger)]
                [t/Num t/Num t/Num -> (t/ASeq t/Num)])

#?@(:cljs [] :default [
cc/class (t/IFn [nil -> nil :object {:id 0 :path [Class]}]
                [Object -> Class :object {:id 0 :path [Class]}]
                [t/Any -> (t/Option Class) :object {:id 0 :path [Class]}])
])

; need better metadata support if this even has a chance of working
; like class
cc/type [t/Any -> t/Any]

cc/seq (t/All [x] (t/IFn [(t/NonEmptyColl x) -> (t/NonEmptyASeq x)
                          :filters {:then tt
                                    :else ff}]
                         [(t/Option (t/Coll x)) -> (t/NilableNonEmptyASeq x)
                          :filters {:then (& (is t/NonEmptyCount 0)
                                             (! nil 0))
                                    :else (| (is nil 0)
                                             (is t/EmptyCount 0))}]
                         [(t/Seqable x) -> (t/NilableNonEmptyASeq x)]))

; t/Seqable [[x :variance :covariant]
;          :count [l :variance :covariant :< AnyCountRange]
;          :to-seq [sfn :kind (t/TFn [[x :variance :covariant]]
;                               (t/I IWithMeta (IMeta nil) (ISeq x) (ICollection x) 
;                                  IEmptyableCollection ISequential))]]

; cc/seq (t/All [x
;                        [sfn :kind [* -> *]]
;                    (t/IFn
;                      [(t/Seqable x :count (t/CountRange 1) :to-seq sfn) -> (sfn x)]
;                      [(t/Seqable x :count AnyCountRange :to-seq sfn) -> (t/U nil (sfn x))]))

cc/empty? (t/IFn [(t/Option (t/HSequential [t/Any *])) -> t/Bool
                  :filters {:then (| (is t/EmptyCount 0)
                                     (is nil 0))
                            :else (is t/NonEmptyCount 0)}]
                 [(t/Option (t/Coll t/Any)) -> t/Bool
                  :filters {:then (| (is t/EmptyCount 0)
                                     (is nil 0))
                            :else (is t/NonEmptyCount 0)}]
                 [(t/Seqable t/Any) -> t/Bool])

cc/map (t/All [c a b ...] (t/IFn [[a :-> c] :-> (t/Transducer a c)]
                                 [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
                                 [[a b ... b -> c] (t/Seqable a) (t/Seqable b) ... b -> (t/ASeq c)]))
cc/mapv (t/All [c a b ...] (t/IFn [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyAVec c)]
                                  [[a b ... b -> c] (t/Seqable a) (t/Seqable b) ... b -> (t/AVec c)]))
cc/mapcat (t/All [c a b ...] (t/IFn
                               [[a :-> (t/Seqable c)] :-> (t/Transducer a c)]
                               [[a b ... b -> (t/Seqable c)] (t/Seqable a) (t/Seqable b) ... b -> (t/ASeq c)]))
#?@(:cljs [] :default [
cc/pmap (t/All [c a b ...] (t/IFn [[a b ... b -> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) ... b -> (t/NonEmptyASeq c)]
                                  [[a b ... b -> c] (t/Seqable a) (t/Seqable b) ... b -> (t/ASeq c)]))
cc/pcalls (t/All [r] [[-> r] * -> (t/ASeq r)])
])

#_#_
cc/halt-when
(t/All [a d]
  [[a :-> t/Any] :-> (t/Transducer a a)]
  [[a :-> t/Any] (t/U nil [t/Any a :-> a]) :-> (t/Transducer a a)])

cc/frequencies (t/All [a] [(t/Seqable a) -> (t/Map a t/AnyInteger)])

#?(:cljs cc/*clojurescript-version* :default cc/*clojure-version*)
'{:major t/Any :minor t/Any :incremental t/Any :qualifier t/Any}

#?@(:cljs [] :default [
cc/clojure-version [-> t/Str]
])

#?@(:cljs [] :default [
cc/promise (t/All [x] [-> (t/Promise x)])
cc/deliver (t/All [x] [(t/Promise x) x -> (t/Nilable (t/Promise x))])
])

cc/flatten [(t/Seqable t/Any) -> (t/Seq t/Any)]

cc/map-indexed (t/All [x y] [[t/AnyInteger x -> y] (t/Seqable x) -> (t/ASeq y)])
cc/keep-indexed (t/All [a c] [[t/Num a -> (t/U nil c)] (t/Seqable a) -> (t/Seq c)])
cc/keep (t/All [a b] [[a -> (t/Option b)] (t/Coll a) -> (t/Option (t/ASeq b))])

cc/seqable? (t/Pred (t/Seqable Any))

cc/merge-with (t/All [k v] (t/IFn [[v v -> v] nil * -> nil]
                                  [[v v -> v] (t/Map k v) * -> (t/Map k v)]
                                  [[v v -> v] (t/Option (t/Map k v)) * -> (t/Option (t/Map k v))]))

cc/reduce (t/All [a c] (t/IFn 
                         ;Without accumulator
                         ; default
                         ; (reduce + my-coll)
                         [[a c -> (t/U (Reduced a) a)] (t/NonEmptySeqable c) -> a]
                         [(t/IFn [a c -> (t/U (Reduced a) a)] [-> (t/U (Reduced a) a)]) (t/Seqable c) -> a]
                         ; default
                         ; (reduce + 3 my-coll)
                         ; (reduce (fn [a b] a) (reduced 1) nil) 
                         ; ;=> (reduced 1)
                         [[a c -> (t/U (Reduced a) a)] a (t/Seqable c) -> a]))
cc/reduce-kv (t/All [a c k v] [[a k v -> (t/U (Reduced a) a)] a (t/Option (Associative t/Any k v)) -> a])
cc/reductions (t/All [a b] (t/IFn [[a b -> a] (t/Seqable b) -> (t/ASeq a)]
                                  [[a b -> a] a (t/Seqable b) -> (t/ASeq a)]))
cc/reduced (t/All [x] [x -> (#?(:clj clojure.lang.Reduced :cljs cljs.core.Reduced) x)])

#_(comment
  cc/reduce
       (t/All [a c d]
            (t/IFn 
              ;Without accumulator
              ; empty coll, f takes no args
              ; (reduce + []) => 0, (reduce + nil) => 0
              [[-> c] (t/U nil (t/I (ExactCount 0) (t/Seqable c))) -> c]
              ; coll count = 1, f is not called
              ; (reduce + [1]) => 1
              [t/Any (t/I (ExactCount 1) (t/Seqable c)) -> c]
              ; coll count >= 2
              ; (reduce + [1 2]) => 3
              [[c c -> c] (t/I (t/CountRange 2) (t/Seqable c)) -> c]
              ; default
              ; (reduce + my-coll)
              [(t/IFn [c c -> c] [-> c]) (t/Seqable c) -> c]
              ;With accumulator
              ; empty coll, f not called, returns accumulator
              ; (reduce + 3 []) => 3
              [t/Any a (t/U nil (t/I (ExactCount 0) (t/Seqable t/Any))) -> a]
              ; default
              ; (reduce + 3 my-coll)
              [[a c -> a] a (t/Seqable c) -> a]))
  )

;should be special cased
cc/not= [t/Any t/Any * -> t/Bool]

cc/first (t/All [x] (t/IFn [(t/HSequential [x t/Any *]) -> x
                            :object {:id 0 :path [(Nth 0)]}]
                           [(t/Option (t/EmptySeqable x)) -> nil]
                           [(t/NonEmptySeqable x) -> x]
                           [(t/Seqable x) -> (t/Option x)]))
cc/second (t/All [x] (t/IFn [(t/HSequential [t/Any x t/Any *]) -> x
                             :object {:id 0 :path [(Nth 1)]}]
                            [(t/Option (t/I (t/Seqable x) (t/CountRange 0 1))) -> nil]
                            [(t/I (t/Seqable x) (t/CountRange 2)) -> x]
                            [(t/Seqable x) -> (t/Option x)]))
cc/ffirst (t/All [x] [(t/Seqable (t/Seqable x)) -> (t/Nilable x)])
cc/nfirst (t/All [x] [(t/Seqable (t/Seqable x)) -> (t/NilableNonEmptyASeq x)])
cc/group-by (t/All [x y] [[x -> y] (t/Seqable x) -> (t/Map y (t/Vec x))])
cc/fnext (t/All [x] [(t/Seqable x) -> (t/Option x)])
cc/nnext (t/All [x] [(t/Seqable x) -> (t/NilableNonEmptyASeq x)])
cc/nthnext (t/All [x] (t/IFn [nil t/AnyInteger -> nil]
                             [(t/Seqable x) t/AnyInteger -> (t/NilableNonEmptyASeq x)]))
cc/rest (t/All [x] [(t/Seqable x) -> (t/ASeq x)])
cc/last (t/All [x] (t/IFn [(t/NonEmptySeqable x) -> x]
                          [(t/Seqable x) -> (t/U nil x)]))
cc/butlast (t/All [x] [(t/Seqable x) -> (t/ASeq x)])
cc/next (t/All [x] (t/IFn [(t/Option (t/Coll x)) -> (t/NilableNonEmptyASeq x)
                           :filters {:then (& (is (t/CountRange 2) 0)
                                              (! nil 0))
                                     :else (| (is (t/CountRange 0 1) 0)
                                              (is nil 0))}]
                          [(t/Seqable x) -> (t/NilableNonEmptyASeq x)]))

cc/into
(t/All [x y :named [a]]
       (t/IFn [(t/Map x y) (t/Seqable (t/Seqable (IMapEntry x y))) -> (t/Map x y)]
              [(t/Vec x) (t/Seqable x) -> (t/Vec x)]
              [(t/Set x) (t/Seqable x) -> (t/Set x)]
              [(t/Coll t/Any) (t/Seqable t/Any) -> (t/Coll t/Any)]
              ; transducer arities
              [(t/Map x y) (t/Transducer a (t/Nilable '[x y])) (t/Seqable a) -> (t/Map x y)]
              [(t/Vec x) (t/Transducer y x) (t/Seqable y) -> (t/Vec x)]
              [(t/Set x) (t/Transducer y x) (t/Nilable (t/Seqable y)) -> (t/Set x)]
              [(t/Coll t/Any) (t/Transducer y t/Any) (t/Nilable (t/Seqable y)) -> (t/Coll t/Any)]))

cc/conj
;     (t/All [e
;           [Arg :< (t/TFn [[x :variance :covariant]] t/Any)]
;           [Res :< (t/TFn [[x :variance :covariant]]
;                     (t/Coll t/Any))]]
;          (t/IFn [(clojure.lang.IPersistentCollection e Arg Res) (Arg e) (Arg e) * -> (Res e)]
;              [nil e e * -> (clojure.lang.PersistentList e)]))
(t/All [x y] (t/IFn [(t/Vec x) x x * -> (t/Vec x)]
                    [(APersistentMap x y)
                     (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
                     (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) *
                     -> (APersistentMap x y)]
                    [(t/Map x y)
                     (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y])
                     (t/U nil (t/Seqable (IMapEntry x y)) (IMapEntry x y) '[x y]) * -> (t/Map x y)]
                    [(t/Set x) x x * -> (t/Set x)]
                    [(t/ASeq x) x x * -> (t/ASeq x)]
                    [nil x x * -> (clojure.lang.PersistentList x)]
                    [(t/Coll t/Any) t/Any t/Any * -> (t/Coll t/Any)]))

; IPersistentCollection [[x :variance :covariant]
;                        :conj-fn [conj-fn :kind (t/TFn [[x :variance :covariant]] (IPersistentCollection x))]
;                        :empty-fn [empty-fn :kind (t/TFn [] (IPersistentCollection t/Nothing :count (ExactCount 0)))]]

; cc/conj
;   (t/All [x conj-fn]
;     [(IPersistentCollection x :conj-fn conj-fn) x -> (conj-fn x)]
;     [nil x -> (PersistentList x)]
;     [(t/U nil (IPersistentCollection x :conj-fn conj-fn)) x -> (t/U nil (conj-fn x))])

; cc/empty
;   (t/All [x empty-fn]
;      [(IPersistentCollection t/Any :empty-fn empty-fn) -> (empty-fn)]
;      [nil -> nil]
;      [(t/U nil (IPersistentCollection t/Any :empty-fn empty-fn)) -> (t/U nil (empty-fn))])

cc/sequence (t/All [a b] (t/IFn [(t/Nilable (t/Seqable a)) -> (t/Seq a)]
                                [(t/Transducer a b) (t/Nilable (t/Seqable a)) :-> (t/Seqable b)]))
cc/find (t/All [x y] [(t/Nilable (clojure.lang.Associative t/Any x y)) t/Any -> (t/Nilable (t/HVec [x y]))])

cc/get-in (t/IFn [t/Any (t/Nilable (t/Seqable t/Any)) -> t/Any]
                 [t/Any (t/Nilable (t/Seqable t/Any)) t/Any -> t/Any])

cc/assoc-in [(t/Nilable (Associative t/Any t/Any t/Any)) (t/Seqable t/Any) t/Any -> t/Any]

;FIXME maps after the first can always be nil
cc/merge (t/All [k v] (t/IFn [nil * -> nil]
                             [(t/Map k v) (t/Map k v) * -> (t/Map k v)]
                             [(t/Option (t/Map k v)) * -> (t/Option (t/Map k v))]))

;more to be said here?
cc/contains? [(t/Option (t/Seqable t/Any)) t/Any -> t/Bool]

cc/= [t/Any t/Any * -> t/Bool]
cc/identical? [t/Any t/Any -> t/Bool]
#?(:cljs [
cc/keyword-identical? [t/Any t/Any -> t/Bool]
])
cc/distinct? [t/Any t/Any * -> t/Bool]

#?@(:cljs [] :default [
cc/decimal? (t/Pred BigDecimal)
cc/denominator [clojure.lang.Ratio -> t/Num]
])

cc/mod (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
              [t/Num t/Num -> t/Num])

#?@(:cljs [] :default [
cc/var-get (t/All [r] [(t/Var2 t/Nothing r) -> r])
cc/var-set (t/All [w] [(t/Var2 w t/Any) w -> w])

cc/supers [Class -> (t/U nil (t/I t/NonEmptyCount (t/Set Class)))]
])

cc/take-nth (t/All [x] [t/AnyInteger (t/U nil (t/Seqable x)) -> (t/ASeq x)])

cc/shuffle (t/All [x] (t/IFn [(t/I (Collection x) (t/Seqable x)) -> (t/Vec x)]
                             [(Collection x) -> (t/Vec x)]))

cc/special-symbol? [t/Any -> t/Bool]

cc/integer? (t/Pred t/AnyInteger)
cc/number? (t/Pred t/Num)
cc/var? (t/Pred (t/Var2 t/Nothing t/Any))
cc/class? (t/Pred Class)

#?@(:cljs [] :default [
cc/resolve (t/IFn [t/Sym -> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
                  ; should &env arg be more accurate?
                  [t/Any t/Sym -> (t/U (t/Var2 t/Nothing t/Any) Class nil)])
cc/ns-resolve (t/IFn [(t/U t/Sym t/Namespace) t/Sym -> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
                     ; should &env arg be more accurate?
                     [(t/U t/Sym t/Namespace) t/Any t/Sym -> (t/U (t/Var2 t/Nothing t/Any) Class nil)])
cc/extenders [t/Any -> (t/U nil (t/Seqable (t/U Class nil)))]
])

cc/+ (t/IFn #?(:clj [Long * -> Long])
            #?(:clj [(t/U Long Double) * -> Double])
            [t/AnyInteger * -> t/AnyInteger]
            [t/Num * -> t/Num])
cc/- (t/IFn #?(:clj [Long Long * -> Long])
            #?(:clj [(t/U Long Double) (t/U Long Double) * -> Double])
            [t/AnyInteger t/AnyInteger * -> t/AnyInteger]
            [t/Num t/Num * -> t/Num])
cc/* (t/IFn #?(:clj [Long * -> Long])
            #?(:clj [(t/U Long Double) * -> Double])
            [t/AnyInteger * -> t/AnyInteger]
            [t/Num * -> t/Num])
cc// (t/IFn #?(:clj [Double Double * -> Double])
            [t/Num t/Num * -> t/Num])

cc/+' (t/IFn [t/AnyInteger * -> t/AnyInteger]
             [t/Num * -> t/Num])
cc/-' (t/IFn [t/AnyInteger t/AnyInteger * -> t/AnyInteger]
             [t/Num t/Num * -> t/Num])
cc/*' (t/IFn [t/AnyInteger * -> t/AnyInteger]
             [t/Num * -> t/Num])
cc/quot (t/IFn #?(:clj [Long Long -> Long])
               #?(:clj [(t/U Long Double) (t/U Long Double) -> Double])
               [t/AnyInteger t/AnyInteger -> t/AnyInteger] 
               [t/Num t/Num -> t/Num])

cc/unchecked-inc (t/IFn [t/AnyInteger -> t/AnyInteger]
                        [t/Num -> t/Num])
cc/unchecked-inc-int [t/Num -> t/AnyInteger]
cc/unchecked-dec (t/IFn [t/AnyInteger -> t/AnyInteger]
                        [t/Num -> t/Num])
cc/unchecked-dec-int [t/Num -> t/AnyInteger]
cc/unchecked-subtract (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                             [t/Num t/Num -> t/Num])
cc/unchecked-subtract-int [t/Num t/Num -> t/AnyInteger]
cc/unchecked-negate (t/IFn [t/AnyInteger -> t/AnyInteger]
                           [t/Num -> t/Num])
cc/unchecked-negate-int [t/Num -> t/AnyInteger]
cc/unchecked-add (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                        [t/Num t/Num -> t/Num])
cc/unchecked-add-int [t/Num t/Num -> t/AnyInteger]
cc/unchecked-multiply (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                             [t/Num t/Num -> t/Num])
cc/unchecked-multiply-int [t/Num t/Num -> t/AnyInteger]
cc/unchecked-divide-int [t/Num t/Num -> t/AnyInteger]
cc/unchecked-remainder-int [t/Num t/Num -> t/AnyInteger]
cc/rem (t/IFn [t/AnyInteger t/AnyInteger -> t/AnyInteger]
              [t/Num t/Num -> t/Num])
cc/inc (t/IFn #?(:clj [Long -> Long])
              #?(:clj [Double -> Double])
              [t/AnyInteger -> t/AnyInteger]
              [t/Num -> t/Num])
cc/dec (t/IFn #?(:clj [Long -> Long])
              #?(:clj [Double -> Double])
              [t/AnyInteger -> t/AnyInteger]
              [t/Num -> t/Num])

cc/inc' (t/IFn [t/AnyInteger -> t/AnyInteger]
               [t/Num -> t/Num])
cc/dec' (t/IFn [t/AnyInteger -> t/AnyInteger]
               [t/Num -> t/Num])

#?@(:cljs [] :default [
cc/rationalize [t/Num -> t/Num]
])

cc/bit-not [t/AnyInteger -> t/AnyInteger]
cc/bit-and [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
cc/bit-or [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
cc/bit-xor [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
cc/bit-and-not [t/AnyInteger t/AnyInteger t/AnyInteger * -> t/AnyInteger]
cc/bit-clear [t/AnyInteger t/AnyInteger -> t/AnyInteger]
cc/bit-set [t/AnyInteger t/AnyInteger -> t/AnyInteger]
cc/bit-flip [t/AnyInteger t/AnyInteger -> t/AnyInteger]
cc/bit-test [t/AnyInteger t/AnyInteger -> t/AnyInteger]
cc/bit-shift-left [t/AnyInteger t/AnyInteger -> t/AnyInteger]
cc/bit-shift-right [t/AnyInteger t/AnyInteger -> t/AnyInteger]
cc/unsigned-bit-shift-right [t/AnyInteger t/AnyInteger -> t/AnyInteger]

cc/even? [t/AnyInteger -> t/Bool]
cc/odd? [t/AnyInteger -> t/Bool]

cc/peek (t/All [x] (t/IFn [(t/I t/NonEmptyCount (t/Stack x)) -> x]
                          [(t/Stack x) -> x]))
cc/pop (t/All [x] (t/IFn [(t/List x) -> (t/List x)]
                         [(t/Vec x) -> (t/Vec x)]
                         [(t/Stack x) -> (t/Stack x)]))

#?@(:cljs [] :default [
cc/get-thread-bindings [-> (t/Map (t/Var2 t/Nothing t/Any) t/Any)]
cc/bound-fn*
    (t/All [r b ...]
         [[b ... b -> r] -> [b ... b -> r]])
cc/find-var
    [t/Sym -> (t/U nil (t/Var2 t/Nothing t/Any))]
cc/agent
    (t/All [x] [x & :optional {:validator (t/U nil [x -> t/Any]) :meta t/Any
                             :error-handler (t/U nil [(t/Agent1 x) Throwable -> t/Any])
                             :error-mode (t/U ':continue ':fail)} 
              -> (t/Agent1 x)])
cc/set-agent-send-executor! [java.util.concurrent.ExecutorService -> t/Any]
cc/set-agent-send-off-executor! [java.util.concurrent.ExecutorService -> t/Any]
cc/send-via (t/All [w r b ...] [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])
cc/send (t/All [w r b ...] [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])
cc/send-off (t/All [w r b ...] [(t/Agent2 w r) [r b ... b -> w] b ... b -> (t/Agent2 w r)])
cc/await [(t/Agent2 t/Nothing t/Any) * -> nil]
cc/await-for [t/AnyInteger (t/Agent2 t/Nothing t/Any) * -> t/Bool]
cc/await1 (t/All [w r] [(t/Agent2 w r) -> (t/Agent2 w r)])
cc/release-pending-sends [-> t/AnyInteger]
])

cc/add-watch (t/All [x [a :< (IRef t/Nothing x)]]
                    (t/IFn 
                      ; this arity remembers the type of reference we pass to the function
                      [a t/Any [t/Any a x x -> t/Any] -> t/Any]
                      ; if the above cannot be inferred, 
                      [(IRef t/Nothing x) t/Any [t/Any (IRef t/Nothing x) x x -> t/Any] -> t/Any]))
cc/remove-watch [(IRef t/Nothing t/Any) t/Any -> t/Any]

#?@(:cljs [] :default [
cc/agent-error [(t/Agent2 t/Nothing t/Any) -> (t/U nil Throwable)]
cc/restart-agent (t/All [w] [(t/Agent2 w t/Any) w & :optional {:clear-actions t/Any} -> t/Any])
cc/set-error-handler! (t/All [w r] [(t/Agent2 w r) [(t/Agent2 w r) Throwable -> t/Any] -> t/Any])
cc/error-handler (t/All [w r] [(t/Agent2 w r) -> (t/U nil [(t/Agent2 w r) Throwable -> t/Any])])
cc/set-error-mode! [(t/Agent2 t/Nothing t/Any) (t/U ':fail ':continue) -> t/Any]
cc/error-mode [(t/Agent2 t/Nothing t/Any) -> t/Any]
cc/agent-errors [(t/Agent2 t/Nothing t/Any) -> (t/U nil (t/ASeq Throwable))]
cc/clear-agent-errors [(t/Agent2 t/Nothing t/Any) -> t/Any]
cc/shutdown-agents [-> t/Any]
])

cc/take (t/All [x] (t/IFn [t/Int :-> (t/Transducer x x)]
                          [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/ASeq x)]))
cc/drop (t/All [x] (t/IFn [t/Int :-> (t/Transducer x x)]
                          [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/ASeq x)]))
cc/take-last (t/All [x] [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/NilableNonEmptyASeq x)])
cc/drop-last (t/All [x] [t/AnyInteger (t/Nilable (t/Seqable x)) -> (t/ASeq x)])

cc/hash [t/Any -> t/AnyInteger]
cc/hash-combine [t/AnyInteger t/Any -> t/AnyInteger]

;;TODO cljs
#?@(:cljs [] :default [
cc/ifn? (t/Pred clojure.lang.IFn)
cc/fn? (t/Pred t/Fn)
cc/instance? [Class t/Any -> t/Bool]
])

cc/cons (t/All [x] [x (t/Option (t/Seqable x)) -> (t/ASeq x)])
cc/reverse (t/All [x] [(t/Option (t/Seqable x)) -> (t/ASeq x)])
cc/rseq (t/All [x] [(#?(:cljs cljs.core/IReversible :default clojure.core.typed/Reversible) x) -> (t/NilableNonEmptyASeq x)])

;coercions
#?@(:cljs [] :default [
cc/bigdec [(t/U t/Str t/Num) -> BigDecimal]
cc/bigint [(t/U t/Str t/Num) -> clojure.lang.BigInt]
cc/biginteger [(t/U t/Str t/Num) -> java.math.BigInteger]
])
cc/boolean [t/Any -> t/Bool]
cc/byte [(t/U Character t/Num) -> Byte]
#?@(:cljs [
cc/char [(t/U s/Str t/Num) -> s/Str]
] :default [
cc/char [(t/U Character t/Num) -> Character]
])
cc/double [t/Num -> #?(:cljs t/Num :default Double)]
cc/float [t/Num -> #?(:cljs t/Num :default Float)]
cc/int [#?(:cljs t/Num :default (t/U Character t/Num)) -> #?(:cljs t/AnyInteger :default Integer)]
cc/long [#?(:cljs t/Num :default (t/U Character t/Num)) -> #?(:cljs t/AnyInteger :default Long)]
#?@(:cljs [] :default [
cc/num [t/Num -> t/Num]
])
#?@(:cljs [
cc/short [t/Num -> t/Num]
] :default [
cc/short [(t/U Character t/Num) -> Short]
])

;array ctors
#?@(:cljs [] :default [
cc/boolean-array (t/IFn [(t/U t/Num (t/Seqable t/Bool)) -> (Array boolean)]
                        [t/Num (t/U t/Bool (t/Seqable t/Bool)) -> (Array boolean)])
cc/byte-array (t/IFn [(t/U t/Num (t/Seqable Byte)) -> (Array byte)]
                     [t/Num (t/U Byte (t/Seqable Byte)) -> (Array byte)])
cc/char-array (t/IFn [(t/U t/Num (t/Seqable Character)) -> (Array char)]
                     [t/Num (t/U t/Num (t/Seqable Character)) -> (Array char)])
cc/short-array (t/IFn [(t/U t/Num (t/Seqable Short)) -> (Array short)]
                      [t/Num (t/U Short (t/Seqable Short)) -> (Array short)])
cc/int-array (t/IFn [(t/U t/Num (t/Seqable t/Num)) -> (Array int)]
                    [t/Num (t/U t/Num (t/Seqable t/Num)) -> (Array int)])
cc/double-array (t/IFn [(t/U t/Num (t/Seqable t/Num)) -> (Array double)]
                       [t/Num (t/U t/Num (t/Seqable t/Num)) -> (Array double)])
])

;cast to java array
;; TODO rethink input and output types. eg.,
;;      cc/booleans [(ReadyOnlyArray boolean) -> (t/U nil (Array boolean))]
;; TODO objects??
;;      cc/objects [(ReadyOnlyArray Object) -> (t/U nil (ReadyOnlyArray Object))]
;;                                  
;; TODO propagate to Numbers/booleans etc
;cc/booleans [t/Any -> (t/U nil (Array boolean))]
;cc/bytes [t/Any -> (t/U nil (Array byte))]
;cc/chars [t/Any -> (t/U nil (Array char))]
;cc/shorts [t/Any -> (t/U nil (Array short))]
;cc/ints [t/Any -> (t/U nil (Array int))]
;cc/longs [t/Any -> (t/U nil (Array long))]
;cc/floats [t/Any -> (t/U nil (Array float))]
;cc/doubles [t/Any -> (t/U nil (Array double))]

cc/max-key (t/All [x] [[x -> t/Num] x x x * -> x])
cc/min-key (t/All [x] [[x -> t/Num] x x x * -> x])

cc/< [t/Num t/Num * -> t/Bool]
cc/<= [t/Num t/Num * -> t/Bool]
cc/> [t/Num t/Num * -> t/Bool]
cc/>= [t/Num t/Num * -> t/Bool]
cc/== [t/Num t/Num * -> t/Bool]

cc/max (t/IFn #?(:clj [Long Long * -> Long])
              #?(:clj [Double Double * -> Double])
              [t/Num t/Num * -> t/Num])
cc/min (t/IFn #?(:clj [Long Long * -> Long])
              #?(:clj [Double Double * -> Double])
              [t/Num t/Num * -> t/Num])

#?@(:cljs [] :default [
cc/ref (t/All [x] [x & :optional {:validator (t/U nil [x -> t/Any]) :meta (t/U nil (t/Map t/Any t/Any))
                                  :min-history (t/U nil t/AnyInteger)
                                  :max-history (t/U nil t/AnyInteger)}
                   -> (clojure.lang.Ref x x)])
])

cc/rand (t/IFn [-> t/Num]
               [t/Num -> t/Num])
cc/rand-int [t/Int -> t/Int]
cc/ex-info (t/IFn [(t/U nil t/Str) (t/Map t/Any t/Any) -> t/ExInfo]
                  [(t/U nil t/Str) (t/Map t/Any t/Any) (t/U nil Throwable) -> t/ExInfo])
cc/ex-data (t/IFn [t/ExInfo -> (t/Map t/Any t/Any)]
                  [t/Any -> (t/Nilable (t/Map t/Any t/Any))])


;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (t/Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (t/Pred (IChunk t/Any)).
#?@(:cljs [] :default [
cc/chunked-seq? [t/Any -> t/Any]
cc/chunk-first 
     (t/All [x]
          ;should be IChunkedSeq -> IChunk
          [(t/Seqable x) -> (clojure.lang.IChunk x)])
cc/chunk-rest
     (t/All [x]
          ;should be IChunkRest -> t/Seq
          [(clojure.lang.Seqable x) -> (t/ASeq x)])
cc/chunk-buffer
     (t/All [x]
          [(t/U Integer Long) -> (clojure.lang.ChunkBuffer x)])
cc/chunk
     (t/All [x]
          [(clojure.lang.ChunkBuffer x) -> (clojure.lang.IChunk x)])
cc/chunk-cons
     (t/All [x]
          [(clojure.lang.IChunk x) (t/Seqable x) -> (t/ASeq x)])
cc/chunk-append
     (t/All [x]
          [(clojure.lang.ChunkBuffer x) x -> t/Any])
;;END CHUNK HACKS
])


cc/subvec (t/All [x] (t/IFn [(t/Vec x) t/AnyInteger -> (t/Vec x)]
                            [(t/Vec x) t/AnyInteger t/AnyInteger -> (t/Vec x)]))

#?@(:cljs [] :default [
cc/alias [t/Sym t/Sym -> nil]
cc/all-ns [-> (t/NilableNonEmptyASeq t/Namespace)]
])

#?@(:cljs [] :default [
cc/*file* t/Str
])
cc/*command-line-args* (t/NilableNonEmptyASeq t/Str)
#?@(:cljs [
cc/*unchecked-if* t/Bool
cc/*unchecked-arrays* t/Bool
cc/*warn-on-infer* t/Bool
] :default [
cc/*warn-on-reflection* t/Bool
cc/*compile-path* t/Str
cc/*compile-files* t/Bool
cc/*unchecked-math* t/Bool
cc/*compiler-options* (t/Map t/Any t/Any)
cc/*in* java.io.Reader
cc/*out* java.io.Writer ;; FIXME cljs
cc/*err* java.io.Writer
])
cc/*flush-on-newline* t/Bool
cc/*print-meta* t/Bool
cc/*print-dup* t/Bool
cc/*print-readably* t/Bool
#?@(:cljs [] :default [
cc/*read-eval* (t/U ':unknown t/Bool)
])

cc/trampoline (t/All [r b ...] [[b ... b -> (t/Rec [f] (t/U r [-> (t/U f r)]))]
                                b ... b -> r])


;; math.numeric-tower

#?@(:cljs [] :default [
clojure.math.numeric-tower/floor (t/IFn [t/AnyInteger -> t/AnyInteger]
                                        [t/Num -> t/Num])
clojure.math.numeric-tower/abs (t/IFn [t/AnyInteger -> t/AnyInteger]
                                      [t/Num -> t/Num])
])

;; core.match

#?@(:cljs [] :default [
clojure.core.match/backtrack Exception
])

cc/eval [t/Any -> t/Any]
cc/rand-nth (t/All [x] [(t/U (Indexed x) (t/SequentialSeqable x)) -> x])

#?@(:cljs [
;TODO
;cljs.pprint/cl-format [(t/U cljs.core/IWriter nil t/Bool) t/Str t/Any * -> (t/U nil t/Str)]
;cljs.pprint/fresh-line [-> t/Any]
;cljs.pprint/get-pretty-writer [cljs.core/IWriter -> cljs.core/IWriter]
;clojure.pprint/pprint (t/IFn [t/Any -> nil]
;                             [t/Any java.io.Writer -> nil])
] :default [
clojure.pprint/cl-format [(t/U java.io.Writer nil t/Bool) t/Str t/Any * -> (t/U nil t/Str)]
clojure.pprint/fresh-line [-> t/Any]
clojure.pprint/get-pretty-writer [java.io.Writer -> java.io.Writer]
clojure.pprint/pprint (t/IFn [t/Any -> nil]
                             [t/Any java.io.Writer -> nil])
])

#?@(:cljs [] :default [
clojure.repl/pst (t/IFn [-> nil]
                        [(t/U t/Int Throwable) -> nil]
                        [Throwable t/Int -> nil])
clojure.repl/print-doc [t/Sym -> t/Any]
clojure.repl/find-doc [(t/U t/Str java.util.regex.Pattern) -> t/Any]
clojure.repl/source-fn [t/Any -> (t/U nil t/Str)]
clojure.java.javadoc/javadoc [Object -> t/Any]
complete.core/completions
(t/IFn [t/Any -> t/Any]
     [t/Any t/Any -> t/Any])
])


)
