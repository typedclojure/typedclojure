;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.ann.clojure
  "Type annotations for the base Clojure distribution."
  #?(:cljs (:require-macros [typed.ann-macros.clojure :as macros]))
  (:require [clojure.core :as cc]
            [typed.clojure :as t]
            #?(:clj [typed.ann-macros.clojure :as macros])
            #?(:clj typed.ann.clojure.jvm) ;; jvm annotations
            #?(:clj clojure.core.typed))
  #?(:clj
     (:import (clojure.lang PersistentHashSet PersistentList
                            APersistentMap #_IPersistentCollection
                            #_ITransientSet
                            IRef)
              (java.util Comparator Collection))))

;; ==========================================
;; Protocol annotations

(t/ann-protocol cc/Inst
                -inst-ms [cc/Inst :-> t/Int])

#?(:clj (do
(t/defalias IOFactoryOpts (t/HMap :optional {:append t/Any, :encoding (t/Nilable t/Str)}))
(t/ann-protocol clojure.java.io/IOFactory
                make-reader
                [clojure.java.io/IOFactory IOFactoryOpts :-> java.io.BufferedReader]

                make-writer 
                [clojure.java.io/IOFactory IOFactoryOpts :-> java.io.BufferedWriter]

                make-input-stream 
                [clojure.java.io/IOFactory IOFactoryOpts :-> java.io.BufferedInputStream]

                make-output-stream
                [clojure.java.io/IOFactory IOFactoryOpts :-> java.io.BufferedOutputStream])

(t/ann-protocol clojure.java.io/Coercions
                as-file
                [clojure.java.io/Coercions :-> (t/Nilable java.io.File)]

                as-url
                [clojure.java.io/Coercions :-> (t/Nilable java.net.URL)])
))

#?(:cljs (do
(t/ann-protocol cljs.core/Fn)
(t/ann-protocol cljs.core/IFn)
(t/ann-protocol cljs.core/ICloneable
                -clone [cljs.core/ICloneable :-> t/Any])
(t/ann-protocol [[x :variance :covariant]]
                cljs.core/IIterable
                ;; TODO
                -iterator [(cljs.core/IIterable x) :-> t/Any #_Object])
(t/ann-protocol cljs.core/ICounted
                -count [cljs.core/ICounted :-> t/Num])
(t/ann-protocol cljs.core/IEmptyableCollection
                ;; :/ TFn param on protocol?
                -empty [cljs.core/IEmptyableCollection :-> t/Any])
(t/ann-protocol cljs.core/ICollection
                -conj [cljs.core/ICollection t/Any :-> cljs.core/ICollection])
(t/ann-protocol [[x :variance :covariant]] cljs.core/IIndexed
                -nth (t/All [y]
                            [(cljs.core/IIndexed x) t/JSnumber (t/? y) :-> (t/U x y)]))
(t/ann-protocol cljs.core/ASeq)
(t/ann-protocol [[x :variance :covariant
                  ;;FIXME causes mystery error
                  ;:< (t/NilableNonEmptySeq t/Any)
                  ]] cljs.core/ISeqable) 
(t/ann-protocol [[x :variance :covariant]] cljs.core/ISeq
                -first [(cljs.core/ISeq x) :-> (t/Nilable x)]
                -rest [(cljs.core/ISeq x) :-> (cljs.core/ISeq x)])
;cljs.core/INext [[x :variance :covariant]]
(t/ann-protocol [[v :variance :covariant]] cljs.core/ILookup)
(t/ann-protocol [[k :variance :covariant]
                 [v :variance :covariant]]
                cljs.core/IAssociative
                -contains-key [(cljs.core/IAssociative k v) t/Any :-> t/Bool]
                -assoc (t/All [k1 v1] [(cljs.core/IAssociative k v) k1 v1 :->
                                       (cljs.core/IAssociative (t/U k k1) (t/U v v1))]))
(t/ann-protocol cljs.core/IMap
                -dissoc [cljs.core/IMap t/Any :-> cljs.core/IMap])
(t/ann-protocol [[k :variance :covariant]
                 [v :variance :covariant]]
                cljs.core/IMapEntry
                -key [(cljs.core/IMapEntry k v) :-> k]
                -val [(cljs.core/IMapEntry k v) :-> v])
(t/ann-protocol cljs.core/ISet
                -disjoin (t/All [s] [s t/Any :-> s]))
(t/ann-protocol [[x :variance :covariant]]
                cljs.core/IStack
                -peek [(cljs.core/IStack x) :-> (t/U nil x)]
                -pop [(cljs.core/IStack x) :-> (cljs.core/IStack x)])
(t/ann-protocol [[x :variance :covariant]]
                cljs.core/IVector
                -assoc-n (t/All [x1] [(cljs.core/IVector x) t/Num x1
                                      :-> (cljs.core/IVector (t/U x x1))]))
(t/ann-protocol [[x :variance :covariant]]
                cljs.core/IDeref
                -deref [(cljs.core/IDeref x) :-> x])
(t/ann-protocol [[x :variance :covariant]]
                cljs.core/IDerefWithTimeout
                -deref-with-timeout [(cljs.core/IDerefWithTimeout x) :-> x])
(t/ann-protocol cljs.core/IMeta
                -meta [cljs.core/IMeta :-> (t/Nilable (t/Map t/Any t/Any))])
(t/ann-protocol cljs.core/IWithMeta
                -with-meta [cljs.core/IWithMeta (t/Nilable (t/Map t/Any t/Any)) :-> cljs.core/IWithMeta])
;TODO
;cljs.core/IReduce [[]]
(t/ann-protocol cljs.core/IKVReduce ;;TODO
                -kv-reduce [cljs.core/IKVReduce [t/Any t/Any t/Any :-> t/Any] :-> t/Any])
(t/ann-protocol cljs.core/IList)
(t/ann-protocol cljs.core/IEquiv
                -equiv [cljs.core/IEquiv t/Any :-> t/Bool])
(t/ann-protocol cljs.core/IHash
                -hash [cljs.core/IHash :-> t/Num])
(t/ann-protocol cljs.core/ISequential)
(t/ann-protocol cljs.core/IRecord)
(t/ann-protocol [[x :variance :covariant]]
                cljs.core/IReversible
                -rseq [(cljs.core/IReversible x) :-> (t/NilableNonEmptySeq x)])
(t/ann-protocol [[k :variance :covariant]
                 [v :variance :covariant]] cljs.core/IFind
                -find [(cljs.core/IFind k v) t/Any :-> (t/Nilable (cljs.core/IMapEntry k v))]
                )
(t/ann-protocol [[x :variance :invariant]]
                cljs.core/ISorted
                -sorted-seq [(cljs.core/ISorted x) t/Bool :-> (t/Nilable (t/ASeq x))]
                ;; second arg => comparable?
                -sorted-seq-from [(cljs.core/ISorted x) t/Any t/Bool :-> (t/Nilable (t/ASeq x))]
                -entry-key [(cljs.core/ISorted x) t/Any :-> t/Any]
                -comparator [(cljs.core/ISorted x) :-> [x x :-> t/Num]])
(t/ann-protocol cljs.core/IPending)
;cljs.core/IWriter [[]]
;cljs.core/IPrintWithWriter [[]]
;    ;TODO
;;cljs.core/IWatchable [[]]
;    ;cljs.core/IEditableCollection [[]]
;    ;cljs.core/ITransientCollection [[]]
;    ;cljs.core/ITransientAssociative [[]]
;    ;cljs.core/ITransientMap [[]]
;    ;cljs.core/ITransientVector [[]]
;    ;cljs.core/ITransientSet [[]]
(t/ann-protocol [[x :variance :invariant]]
                cljs.core/IComparable)
;    ;cljs.core/IChunk [[]]
;    ;cljs.core/IChunkedSeq [[]]
;    ;cljs.core/IChunkedNext [[]]
(t/ann-protocol cljs.core/INamed
                -named [cljs.core/INamed :-> t/Str]
                -namespace [cljs.core/INamed :-> (t/Nilable t/Str)])

(t/ann-protocol cljs.core/UUID)
))

;; ==========================================
;; Datatypes

#?(:cljs (do
(t/ann-datatype [[w :variance :contravariant]
                 [r :variance :covariant]]
                cljs.core/Atom)
(t/ann-datatype cljs.core/Symbol)
(t/ann-datatype cljs.core/Keyword)
(t/ann-datatype [[x :variance :covariant]] cljs.core/List)
(t/ann-datatype [[x :variance :covariant]] cljs.core/Reduced)
(t/ann-datatype cljs.core/ExceptionInfo)
(t/ann-datatype [[x :variance :covariant]] cljs.core/Delay)
))

;; ==========================================
;; Type aliases

(t/defalias
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

(t/defalias
  ^{:doc "A type that returns true for clojure.core/integer?"
    :forms '[Int]}
  t/Int
  t/AnyInteger)

(t/defalias
  ^{:doc "A type that returns true for clojure.core/number?"
    :forms '[Num]}
  t/Num
  #?(:clj Number
     :cljs t/JSnumber))

(t/defalias
  ^{:doc "A keyword"
    :forms '[Keyword]}
  t/Keyword
  #?(:clj clojure.lang.Keyword
     :cljs cljs.core/Keyword))

(t/defalias
  ^{:doc "A keyword"
    :forms '[Kw]}
  t/Kw
  t/Keyword)

(t/defalias
  ^{:doc "A symbol"
    :forms '[Symbol]}
  t/Symbol
  #?(:clj clojure.lang.Symbol
     :cljs cljs.core/Symbol))

(t/defalias
  ^{:doc "A symbol"
    :forms '[Sym]}
  t/Sym
  t/Symbol)

(t/defalias
  t/Ident
  (t/U t/Sym t/Kw))

(t/defalias
  ^{:doc "A string"
    :forms '[Str]}
  t/Str
  #?(:clj java.lang.String
     :cljs t/JSstring))

(t/defalias
  t/Named
  #?(:clj clojure.lang.Named
     :cljs cljs.core/INamed))

(t/defalias
  t/Indexed
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Indexed x)
            :cljs (cljs.core/IIndexed x))))

(t/defalias
  ^{:doc "A boolean"
    :forms '[Bool]}
  t/Bool
  #?(:clj java.lang.Boolean
     :cljs t/JSboolean))

(t/defalias
  ^{:doc "A namespace"
    :forms '[Namespace]}
  t/Namespace
  #?(:clj clojure.lang.Namespace
     ;; nilable?
     :cljs cljs.core/Namespace))

(t/defalias
  t/UUID
  #?(:clj java.util.UUID
     :cljs cljs.core/UUID))

(t/defalias
  ^{:doc "An atom that can write type w and read type r."
    :forms '[(Atom2 w r)]}
  t/Atom2
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]] 
         (#?(:clj clojure.lang.Atom :cljs cljs.core/Atom) w r)))

(t/defalias
  ^{:doc "An atom that can read and write type x."
    :forms '[(Atom1 x)]}
  t/Atom1
  (t/TFn [[x :variance :invariant]]
         (t/Atom2 x x)))

#?(:clj (t/defalias
          ^{:doc "A volatile that can write type x and read type y."
            :forms '[(Volatile2 x y)]}
          t/Volatile2
          (t/TFn [[x :variance :contravariant]
                  [y :variance :covariant]]
                 (clojure.lang.Volatile x y))))

#?(:clj (t/defalias
          ^{:doc "A volatile that can read and write type x."
            :forms '[(Volatile x)]}
          t/Volatile
          (t/TFn [[x :variance :invariant]]
                 (t/Volatile2 x x))))

(t/defalias
  ^{:doc "An var that can write type w and read type r."
    :forms '[(Var2 w r)]}
  t/Var2 
  (t/TFn [[w :variance :contravariant]
          [r :variance :covariant]] 
         #?(:clj (clojure.lang.Var w r)
            :cljs (cljs.core/Var w r))))

(t/defalias
  ^{:doc "An var that can read and write type x."
    :forms '[(Var1 x)]}
  t/Var1 
  (t/TFn [[x :variance :invariant]] 
         (t/Var2 x x)))

#?(:clj
   (t/defalias
     ^{:doc "A ref that can write type w and read type r."
       :forms '[(Ref2 w r)]}
     t/Ref2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Ref w r))))

#?(:clj
   (t/defalias
     ^{:doc "A ref that can read and write type x."
       :forms '[(Ref1 x)]}
     t/Ref1
     (t/TFn [[x :variance :invariant]]
            (t/Ref2 x x))))

#?(:clj
   (t/defalias
     ^{:doc "An agent that can write type w and read type r."
       :forms '[(Agent2 w r)]}
     t/Agent2
     (t/TFn [[w :variance :contravariant]
             [r :variance :covariant]] 
            (clojure.lang.Agent w r))))

#?(:clj
   (t/defalias
     ^{:doc "An agent that can read and write type x."
       :forms '[(Agent1 x)]}
     t/Agent1
     (t/TFn [[x :variance :invariant]] 
            (t/Agent2 x x))))

(t/defalias
  ^{:doc "A union of x and nil."
    :forms '[(Option x)]}
  t/Option
  (t/TFn [[x :variance :covariant]] (t/U nil x)))

(t/defalias
  ^{:doc "A union of x and nil."
    :forms '[(Nilable x)]}
  t/Nilable
  t/Option)

(t/defalias
  ^{:doc "The result of re-pattern."
    :forms '[t/Regex]}
  t/Regex
  #?(:clj java.util.regex.Pattern
     :cljs js/Regexp))

(t/defalias
  ^{:doc "The identity function at the type level."
    :forms '[(Id x)]}
  t/Id
  (t/TFn [[x :variance :covariant]] x))

(t/defalias
  ^{:doc "The return type of `seq` on a seqable."
    :forms '[(SeqOn x)]}
  t/SeqOn
  (t/TFn [[x :variance :invariant :< (t/Seqable t/Any)]]
         (t/Match x
                  [[Seq :< (t/NilableNonEmptySeq t/Any)]]
                  #?(:clj (clojure.lang.Seqable Seq)
                     :cljs (cljs.core/ISeqable Seq))
                  :-> Seq
                  [[Seq :< (t/NilableNonEmptySeq t/Any)]]
                  (t/Nilable #?(:clj (clojure.lang.Seqable Seq)
                                :cljs (cljs.core/ISeqable Seq)))
                  :-> (t/Nilable Seq))))

(t/defalias
  ^{:doc "A type that returns true for clojure.core/seqable?, with members x."
    :forms '[(Seqable x)]}
  t/Seqable
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           #?(:clj (clojure.lang.Seqable (t/NilableNonEmptySeq x))
              :cljs (cljs.core/ISeqable (t/NilableNonEmptySeq x))))))

(t/defalias
  t/Counted
  #?(:clj clojure.lang.Counted
     :cljs cljs.core/ICounted))

(t/defalias
  t/URI
  #?(:clj java.net.URI
     :cljs goog.Uri))

(t/defalias
  ^{:doc "A type that returns true for clojure.core/coll?, with members x."
    :forms '[(Coll x)]}
  t/Coll
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentCollection x)
            :cljs (t/I (t/Seqable x)
                       cljs.core/ICollection
                       ;;cljs.core/ICounted
                       cljs.core/IEmptyableCollection
                       ;;cljs.core/IEquiv
                       ))))

(t/defalias
  ^{:doc "The type of all things with count 0. Use as part of an intersection.
         eg. See EmptySeqable."
    :forms '[EmptyCount]}
  t/EmptyCount
  (t/ExactCount 0))

(t/defalias
  ^{:doc "The type of all things with count greater than 0. Use as part of an intersection.
         eg. See NonEmptySeq"
    :forms '[NonEmptyCount]}
  t/NonEmptyCount
  (t/CountRange 1))

(t/defalias
  ^{:doc "A persistent collection with member type x and count greater than 0."
    :forms '[(NonEmptyColl x)]}
  t/NonEmptyColl
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Coll x)
              t/NonEmptyCount)))

(t/defalias
  ^{:doc "An associative persistent collection supporting associative operations on keys type k and values type v."
    :forms '[(t/Associative k v)]}
  t/Associative
  (t/TFn [[k :variance :covariant]
          [v :variance :covariant]]
         #?(:clj (clojure.lang.Associative k v)
            :cljs (t/I (cljs.core/IAssociative k v)
                       ;; emulate clojure.lang.Associative's ancestors
                       (t/Coll t/Any)
                       (cljs.core/ILookup v)))))

(t/defalias
  ^{:doc "A Clojure reversible collection."
    :forms '[(Reversible x)]}
  t/Reversible
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Reversible x)
            :cljs (cljs.core/IReversible x))))

(t/defalias
  ^{:doc "A persistent vector with member type x."
    :forms '[(Vec x)]}
  t/Vec
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentVector x)
            :cljs (t/I (cljs.core/IVector x)
                       (t/Associative t/Int x)
                       (t/Coll x)
                       (cljs.core/ILookup x)
                       cljs.core/ISequential
                       (cljs.core/IStack x)
                       (t/Reversible x)
                       (t/Indexed x)))))

(t/defalias
  ^{:doc "A persistent vector with member type x and count greater than 0."
    :forms '[(NonEmptyVec x)]}
  t/NonEmptyVec
  (t/TFn [[x :variance :covariant]]
       (t/I (t/Vec x)
            t/NonEmptyCount)))

(t/defalias
  ^{:doc "A persistent vector returned from clojure.core/vector (and others)"
    :forms '[(AVec x)]}
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

(t/defalias
  ^{:doc "A persistent vector returned from clojure.core/vector (and others) and count greater than 0."
    :forms '[(NonEmptyAVec x)]}
  t/NonEmptyAVec
  (t/TFn [[x :variance :covariant]]
       (t/I (t/AVec x)
            t/NonEmptyCount)))

(t/defalias
  t/MapEntry
  (t/TFn [[k :variance :covariant]
          [v :variance :covariant]]
         #?(:clj (clojure.lang.IMapEntry k v)
            :cljs (cc/IMapEntry k v))))

(t/defalias
  t/AMapEntry
  (t/TFn [[k :variance :covariant]
          [v :variance :covariant]]
         #?(:clj (clojure.lang.AMapEntry k v)
            :cljs (cc/IMapEntry k v))))

(t/defalias
  ^{:doc "A persistent map with keys k and vals v."
    :forms '[(Map k v)]}
  t/Map
  (t/TFn [[k :variance :covariant]
          [v :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentMap k v)
            :cljs (t/I cljs.core/ICloneable
                       (cljs.core/IIterable (cc/IMapEntry k v))
                       cljs.core/IWithMeta
                       cljs.core/IMeta
                       cc/ICollection
                       cc/IEmptyableCollection
                       cljs.core/IEquiv
                       cljs.core/IHash
                       (t/Seqable (cc/IMapEntry k v))
                       cc/ICounted
                       (cc/ILookup v)
                       (cc/IAssociative k v)
                       (cc/IFind k v)
                       cljs.core/IMap
                       cljs.core/IKVReduce
                       ;; TODO IFn
                       ;; TODO cljs.core/IEditableCollection
                       ))))

(t/defalias 
  ^{:doc "Values that can be conj'ed to a t/Map, adding keys of type k
         and vals of type v."}
  t/MapConjable
  (t/TFn [[k :variance :covariant]
          [v :variance :covariant]]
         (t/U (t/MapEntry k v)
              '[k v]
              (t/Seqable (t/MapEntry k v)))))

(t/defalias
  ^{:doc "A persistent set with member type x"
    :forms '[(Set x)]}
  t/Set
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentSet x)
            :cljs (t/I cljs.core/ICloneable
                       (cljs.core/IIterable x)
                       cljs.core/IWithMeta
                       cljs.core/IMeta
                       cljs.core/ICollection
                       cljs.core/IEmptyableCollection
                       cljs.core/IEquiv
                       cljs.core/IHash
                       (t/Seqable x)
                       cljs.core/ICounted
                       (cljs.core/ILookup x)
                       cljs.core/ISet
                       ;; TODO IFn
                       ;; TODO cljs.core/IEditableCollection
                       ))))

(t/defalias
  ^{:doc "A sorted collection."
    :forms '[(t/Sorted x)]}
  t/Sorted
  (t/TFn [[x :variance :invariant]]
         #?(:clj (clojure.lang.Sorted x)
            :cljs (cljs.core/ISorted x))))

(t/defalias
  ^{:doc "A sorted persistent set with member type x"
    :forms '[(SortedSet x)]}
  t/SortedSet
  (t/TFn [[x :variance :invariant]]
         (t/I (t/Set x)
              (t/Sorted x))))

(t/defalias
  ^{:doc "A sorted persistent map with key type k and value type v"
    :forms '[(SortedMap x)]}
  t/SortedMap
  (t/TFn [[k :variance :invariant]
          [v :variance :covariant]]
         (t/I (t/Map k v)
              (t/Sorted k))))

(t/defalias
  ^{:doc "A type that can be used to create a sequence of member type x
         with count greater than 0."
    :forms '[(NonEmptySeqable x)]}
  t/NonEmptySeqable 
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seqable x)
              t/NonEmptyCount)))

(t/defalias
  ^{:doc "A type that can be used to create a sequence of member type x
         with count 0."
    :forms '[(EmptySeqable x)]}
  t/EmptySeqable
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seqable x)
              t/EmptyCount)))

(t/defalias
  ^{:doc "A persistent sequence of member type x."
    :forms '[(Seq x)]}
  t/Seq
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.ISeq x)
            :cljs (t/I (cljs.core/ISeq x)
                       cljs.core/IWithMeta
                       cljs.core/IMeta
                       (t/Seqable x)
                       (cljs.core/IIterable x)
                       ;(cljs.core/INext x)
                       cljs.core/ICollection
                       cljs.core/IEmptyableCollection
                       cljs.core/ISequential
                       cljs.core/IEquiv))))

(t/defalias
  ^{:doc "A persistent sequence of member type x with count greater than 0."
    :forms '[(NonEmptySeq x)]}
  t/NonEmptySeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Seq x)
              t/NonEmptyCount)))

(t/defalias
  ^{:doc "A persistent sequence of member type x with count greater than 0, or nil."
    :forms '[(NilableNonEmptySeq x)]}
  t/NilableNonEmptySeq
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           (t/NonEmptySeq x))))

(t/defalias
  ^{:doc "A hierarchy for use with derive, isa? etc."
    :forms '[Hierarchy]}
  t/Hierarchy
  '{:parents (t/Map t/Any t/Any)
    :ancestors (t/Map t/Any t/Any)
    :descendants (t/Map t/Any t/Any)})

(t/defalias
  ^{:doc "A Clojure derefable (see clojure.core/deref)."
    :forms '[(Deref x)]}
  t/Deref
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IDeref x)
            ;; note: IDerefWithTimeout not used in cljs
            :cljs (cljs.core/IDeref x))))

#?(:clj
(t/defalias
  ^{:doc "A Clojure future (see clojure.core/{future-call,future})."
    :forms '[(Future x)]}
  t/Future 
  (t/TFn [[x :variance :covariant]]
         (t/I (t/Deref x)
              (clojure.lang.IBlockingDeref x)
              clojure.lang.IPending
              java.util.concurrent.Future))))

#?(:clj
(t/defalias
  ^{:doc "A Clojure promise (see clojure.core/{promise,deliver})."
    :forms '[(Promise x)]}
  t/Promise 
  (t/TFn [[x :variance :invariant]]
         (t/I (t/Deref x)
              (clojure.lang.IBlockingDeref x)
              clojure.lang.IPending
              ;; FIXME I think this might be an implementation detail.
              [x :-> (t/Nilable (t/Promise x))]))))

(t/defalias
  ^{:doc "A Clojure delay (see clojure.core/{delay,force})."
    :forms '[(Delay x)]}
  t/Delay
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Delay x)
            :cljs (cljs.core/Delay x))))

(t/defalias
  ^{:doc "A Clojure blocking derefable (see clojure.core/deref)."
    :forms '[(BlockingDeref x)]}
  t/BlockingDeref
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IBlockingDeref x)
            :cljs (cljs.core/IDerefWithTimeout x))))

(t/defalias
  ^{:doc "A Clojure persistent list."
    :forms '[(List x)]}
  t/List
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.IPersistentList x)
            :cljs (t/I cljs.core/IList
                       cljs.core/ICloneable
                       cljs.core/IWithMeta
                       cljs.core/IMeta
                       cljs.core/ASeq
                       (cljs.core/ISeq x)
                       cljs.core/ICollection
                       cljs.core/IEmptyableCollection
                       cljs.core/ISequential))))
(t/defalias
  ^{:doc "A Clojure custom exception type."
    :forms '[ExInfo]}
  t/ExInfo
  #?(:clj (t/I clojure.lang.IExceptionInfo
               RuntimeException)
     :cljs cljs.core/ExceptionInfo))

(t/defalias
  ^{:doc "A Clojure proxy."
    :forms '[Proxy]}
  t/Proxy
  clojure.lang.IProxy)

(t/defalias
  ^{:doc "A Clojure stack."
    :forms '[(Stack x)]}
  t/Stack
  (t/TFn [[x :variance :covariant]]
       (clojure.lang.IPersistentStack x)))

(t/defalias
  ^{:doc "A sequential collection."
    :forms '[Sequential]}
  t/Sequential
  #?(:clj clojure.lang.Sequential
     :cljs cljs.core/ISequential))

(t/defalias
  ^{:doc "A sequential, seqable collection. Seq's aren't always Sequential."
    :forms '[(SequentialSeqable x)]}
  t/SequentialSeqable
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (t/Seqable x))))

(t/defalias
  ^{:doc "A sequential, seqable Clojure collection."
    :forms '[(SequentialColl x)]}
  t/SequentialColl
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (t/Coll x))))

(t/defalias
  ^{:doc "A Clojure sequential sequence. Seq's aren't always Sequential."
    :forms '[(SequentialSeq x)]}
  t/SequentialSeq
  (t/TFn [[x :variance :covariant]]
         (t/I t/Sequential
              (t/Seq x))))

(t/defalias
  ^{:doc "A sequential seq returned from clojure.core/seq"
    :forms '[(ASeq x)]}
  t/ASeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/SequentialSeq x)
              #?@(:clj [(Iterable x)
                        (java.util.Collection x)
                        (java.util.List x)
                        clojure.lang.IObj]))))

(t/defalias
  ^{:doc "A sequential non-empty seq retured from clojure.core/seq"
    :forms '[(NonEmptyASeq x)]}
  t/NonEmptyASeq
  (t/TFn [[x :variance :covariant]]
         (t/I (t/ASeq x)
              t/NonEmptyCount)))

(t/defalias
  ^{:doc "The result of clojure.core/seq."
    :forms '[(NilableNonEmptyASeq x)]}
  t/NilableNonEmptyASeq
  (t/TFn [[x :variance :covariant]]
         (t/Nilable
           (t/NonEmptyASeq x))))

(t/defalias
  ^{:doc "A type that returns true for clojure.core/fn?"
    :forms '[Fn]}
  t/Fn
  #?(:clj clojure.lang.Fn
     :cljs (t/U cljs.core/Fn
                ;;TODO 
                #_js/Function)))

(t/defalias
  ^{:doc "A Clojure multimethod."
    :forms '[Multi]}
  t/Multi
  clojure.lang.MultiFn)

(t/defalias
  ^{:doc "A reduced value of type x."
    :forms '[(t/Reduced x)]}
  t/Reduced
  (t/TFn [[x :variance :covariant]]
         #?(:clj (clojure.lang.Reduced x)
            :cljs (cljs.core/Reduced x))))

(t/defalias
  ^{:doc "A reducer function with accumulator a and reduces over collections of b"
    :forms '[(Reducer a b)]}
  t/Reducer
  (t/TFn [[a :variance :contravariant]
          [b :variance :invariant]]
         (t/IFn 
           ;init+complete
           [(t/? b) :-> b]
           ;step
           [b a :-> (t/U b (t/Reduced b))])))

(t/defalias
  ^{:doc "A transducer function that transforms in to out."
    :forms '[(Transducer in out)]}
  t/Transducer
  (t/TFn [[in :variance :contravariant]
          [out :variance :covariant]]
         ;; note: putting t/Transducer in an IFn makes r existential (but only in contravariant position IIUC? need to revisit my notes)
         ;; Stephen Dolan noted this when I showed him the type of `into`.
         ;; eg., Simulating Existential Types https://www.cs.cmu.edu/~fp/courses/15312-f04/assignments/asst5.pdf
         (t/All [r]
                [(t/Reducer out r) :-> (t/Reducer in r)])))

(t/defalias
  t/Comparable
  (t/TFn [[x :variance :invariant]]
         #?(:clj (Comparable x)
            :cljs (cljs.core/IComparable x))))

;; ==========================================
;; Var annotations

(macros/anns
#?@(:cljs [] :default [
clojure.core.typed/check-ns [(t/? t/Sym) :-> t/Any]
;; Internal annotations
;clojure.core.typed.current-impl/*current-impl* t/Any
clojure.core.typed.current-impl/clojure t/Any
clojure.core.typed.current-impl/clojurescript t/Any
clojure.core.typed/ann* [t/Any t/Any t/Any :-> t/Any]
clojure.core.typed/untyped-var* [t/Any t/Any :-> t/Any]
clojure.core.typed/declare-names* [t/Any :-> t/Any]
clojure.core.typed/typed-deps* [t/Any :-> t/Any]
clojure.core.typed/warn-on-unannotated-vars* [:-> t/Any]
clojure.core.typed/ann-datatype* [t/Any t/Any t/Any t/Any :-> t/Any]
clojure.core.typed/ann-protocol* [t/Any t/Any t/Any :-> t/Any]
clojure.core.typed/ann-record* [t/Any t/Any t/Any t/Any :-> t/Any]
clojure.core.typed/ann-pdatatype* [t/Any t/Any t/Any t/Any :-> t/Any]
clojure.core.typed/declare-datatypes* [t/Any :-> t/Any]
clojure.core.typed/declare-protocols* [t/Any :-> t/Any]
clojure.core.typed/non-nil-return* [t/Any t/Any :-> t/Any]
clojure.core.typed/nilable-param* [t/Any t/Any :-> t/Any]
clojure.core.typed/override-constructor* [t/Any t/Any :-> t/Any]
clojure.core.typed/override-method* [t/Any t/Any :-> t/Any]
clojure.core.typed/typed-deps* [t/Any :-> t/Any]
clojure.core.typed/load-if-needed [:-> t/Any]
clojure.core.typed/*collect-on-eval* t/Any
; should always be special cased
;clojure.core.typed/var>* [t/Any :-> (t/Var2 t/Nothing t/Any)]
])

;; core annotations

cc/*ns* t/Namespace

#?@(:cljs [] :default [
cc/pop-thread-bindings [:-> t/Any]
cc/load [t/Str :* :-> t/Any]
cc/read-string [t/Str :-> t/Any]
cc/read [(t/alt (t/cat (t/? java.io.Reader))
                (t/cat java.io.Reader t/Bool)
                (t/cat java.io.Reader t/Bool t/Any (t/? t/Bool)))
         :-> t/Any]
cc/read+string [(t/alt (t/cat (t/? java.io.Reader))
                       (t/cat java.io.Reader t/Bool)
                       (t/cat java.io.Reader t/Bool t/Any (t/? t/Bool)))
                :-> '[t/Any t/Str]]
cc/read-line [:-> (t/U nil t/Str)]
cc/add-classpath [(t/U t/Str java.net.URL) :-> nil]
])

cc/*1 t/Any
cc/*2 t/Any
cc/*3 t/Any
cc/*e #?(:cljs t/Any :default (t/U nil Throwable))
#?@(:cljs [] :default [
cc/*agent* (t/U nil (t/Agent2 t/Nothing t/Any))
cc/*allow-unresolved-vars* t/Any
cc/*data-readers* (t/Map t/Sym (t/Var2 t/Nothing t/Any))
cc/*default-data-reader-fn* (t/U nil [t/Any t/Any :-> t/Any])
cc/*fn-loader* t/Any
cc/*math-context* t/Any
cc/*source-path* t/Str
cc/*use-context-classloader* t/Any
])
cc/*assert* t/Any

cc/alength [(ReadOnlyArray t/Any) :-> t/AnyInteger]
cc/aclone (t/All [x] [(ReadOnlyArray x) :-> (Array x)])
cc/aget (t/All [x]
               (t/IFn [(ReadOnlyArray x) 
                       t/AnyInteger :-> x]
                      [(ReadOnlyArray (ReadOnlyArray x)) 
                       t/AnyInteger t/AnyInteger :-> x]
                      [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x))) 
                       t/AnyInteger t/AnyInteger t/AnyInteger :-> x]
                      [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))) 
                       t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger :-> x]
                      ; don't support unsound cases
                      [(ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray (ReadOnlyArray x)))))
                       t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger :-> x]))

;(t/All [x] [(Array x) (t/+ t/AnyInteger) x :-> x])
cc/aset
(t/All [x]
  (t/IFn
    [(Array x) t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]
    [(Array x) t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger t/AnyInteger x :-> x]))

#?@(:cljs [] :default [
cc/macroexpand-1 [t/Any :-> t/Any]
cc/macroexpand [t/Any :-> t/Any]

cc/create-struct [t/Any :* :-> (t/Map t/Any t/Any)]
])

cc/find-ns [t/Sym :-> (t/Nilable t/Namespace)]
cc/create-ns [t/Sym :-> t/Namespace]
#?@(:cljs [] :default [
cc/remove-ns [t/Sym :-> t/Namespace]
cc/ns-map [(t/U t/Sym t/Namespace) :-> t/Sym]
cc/ns-aliases [(t/U t/Sym t/Namespace) :-> (t/Map t/Sym t/Namespace)]
cc/the-ns [(t/U t/Sym t/Namespace) :-> t/Namespace]
cc/in-ns [t/Sym :-> nil]
cc/import [t/Any :* :-> nil]
])
cc/namespace [(t/U t/Sym t/Keyword) :-> (t/Nilable t/Str)]
cc/ns-name [(t/U t/Sym t/Namespace) :-> t/Sym]
cc/name [(t/U t/Str t/Named) :-> t/Str]
cc/identity (t/All [x] [x :-> x
                        :filters {:then (! (t/U nil false) 0)
                                  :else (is (t/U nil false) 0)}
                        :object {:id 0}])
cc/gensym [(t/? (t/U t/Sym t/Str)) :-> t/Sym]
#?@(:cljs [] :default [
cc/intern [(t/U t/Sym t/Namespace) t/Sym (t/? t/Any) :-> (t/Var2 t/Nothing t/Any)]
])


cc/doall (t/All [[c :< (t/U nil (t/Seqable t/Any))]]
                [(t/? t/AnyInteger) c :-> c])
cc/dorun [(t/? t/AnyInteger) (t/Seqable t/Any) :-> nil]
cc/iterate (t/All [x]
                  [[x :-> x] x :-> (t/NonEmptyASeq x)])
cc/memoize (t/All [x y :..]
                  [[y :.. y :-> x] :-> [y :.. y :-> x]])

cc/key (t/All [x] [(t/MapEntry x t/Any) :-> x])
cc/val (t/All [x] [(t/MapEntry t/Any x) :-> x])

cc/iteration (t/All [k v ret] [[k :-> ret] & :optional {:somef [ret :-> t/Any]
                                                        :vf [ret :-> v]
                                                        :kf [ret :-> k]
                                                        :initk k}
                               :-> (Seqable ret)])

;cc/juxt
;(t/All [b1 :..]
;(t/All [x r b2 :..]
;     (Fn [[b1 :.. b1 :-> b2] :.. b2 :-> [b1 :.. b1 :-> '[b2 :.. b2]]]
;         [[b1 :.. b1 :-> r] :* :-> [b1 :.. b1 :-> (t/Vec r)]]
;         [[x :* :-> b2] :.. b2 :-> [x :* :-> '[b2 :.. b2]]]
;         [[x :* :-> r] :* :-> [x :* :-> (t/Vec r)]])))


;TODO flip filters
cc/complement (t/All [x] [[x :-> t/Any] :-> [x :-> t/Bool]])
; should preserve filters
cc/boolean [t/Any :-> t/Bool]

cc/filter (t/All [x y] (t/IFn
                         [[x :-> t/Any :filters {:then (is y 0)}] :-> (t/Transducer x y)]
                         [[x :-> t/Any :filters {:then (! y 0)}] :-> (t/Transducer x (t/I x (t/Not y)))]
                         [[x :-> t/Any] :-> (t/Transducer x x)]
                         [[x :-> t/Any :filters {:then (is y 0)}] (t/Seqable x) :-> (t/ASeq y)]
                         [[x :-> t/Any :filters {:then (! y 0)}] (t/Seqable x) :-> (t/ASeq (t/I x (t/Not y)))]
                         [[x :-> t/Any] (t/Seqable x) :-> (t/ASeq x)]))
cc/filterv (t/All [x y] (t/IFn
                          [[x :-> t/Any :filters {:then (is y 0)}] (t/Seqable x) :-> (t/AVec y)]
                          [[x :-> t/Any] (t/Seqable x) :-> (t/AVec x)]))
cc/remove (t/All [x y] (t/IFn
                         [[x :-> t/Any :filters {:else (is y 0)}] :-> (t/Transducer x y)]
                         [[x :-> t/Any :filters {:else (! y 0)}] :-> (t/Transducer x (t/I x (t/Not y)))]
                         [[x :-> t/Any] :-> (t/Transducer x x)]
                         [[x :-> t/Any :filters {:else (is y 0)}] (t/Seqable x) :-> (t/ASeq y)]
                         [[x :-> t/Any :filters {:else (! y 0)}] (t/Seqable x) :-> (t/ASeq (t/I x (t/Not y)))]
                         [[x :-> t/Any] (t/Seqable x) :-> (t/ASeq x)]))


cc/take-while (t/All [x y] (t/IFn [[x :-> t/Any] :-> (t/Transducer x x)]
                                  [[x :-> t/Any] (t/Seqable x) :-> (t/ASeq x)]))
cc/drop-while (t/All [x] (t/IFn [[x :-> t/Any] :-> (t/Transducer x x)]
                                [[x :-> t/Any] (t/Seqable x) :-> (t/ASeq x)]))

cc/replace (t/All [v] (t/IFn [(t/Map v v) :-> (t/Transducer v v)]
                             [(t/Map v v) (t/Vec v) :-> (t/Vec v)]
                             [(t/Map v v) (t/Seqable v) :-> (t/U (t/Vec v) (t/ASeq v))]))

cc/split-with (t/All [x y z] (t/IFn
                               [[x :-> t/Any :filters {:then (is y 0), :else (is z 0)}] (t/Seqable x) :-> '[(t/ASeq y) (t/ASeq z)]]
                               [[x :-> t/Any] (t/Seqable x) :-> '[(t/ASeq x) (t/ASeq x)]]))

cc/split-at (t/All [x] [t/Int (t/Seqable x) :-> '[(t/ASeq x) (t/ASeq x)]])

cc/partition-by (t/All [x] (t/IFn [[x :-> t/Any] :-> (t/Transducer x (t/NonEmptyAVec x))]
                                  [[x :-> t/Any] (t/Seqable x) :-> (t/ASeq (t/NonEmptyASeq x))]))

cc/partition-all (t/All [x] (t/IFn [t/Int :-> (t/Transducer x (t/NonEmptyAVec x))]
                                   [t/Int (t/? t/Int) (t/Seqable x) :-> (t/ASeq (t/NonEmptyASeq x))]))

cc/partition (t/All [a] [t/Int (t/? t/Int) (t/? t/Int) (t/Seqable a) :-> (t/ASeq (t/NonEmptyASeq a))])

cc/partitionv (t/All [a] [t/Int (t/? t/Int) (t/? t/Int) (t/Seqable a) :-> (t/ASeq (t/NonEmptyAVec a))])

cc/repeatedly (t/All [x] (t/IFn [[:-> x] :-> (t/NonEmptyASeq x)]
                                [t/Int [:-> x] :-> (t/ASeq x)]))


cc/some (t/All [x y] [[x :-> y] (t/Seqable x) :-> (t/Option y)])

; Unions need to support dots for this to work:
;
; (t/All [t0 b :..]
;    (t/IFn [[t/Any :-> t/Any :filters {:then (is t0 0) :else (! t0 0)}] 
;            [t/Any :-> t/Any :filters {:then (is b 0) :else (! b 0)}] :.. b
;             :-> (t/IFn [t/Any :-> t/Any :filters {:then (is (t/U t0 b :.. b) 0) :else (! (t/U t0 b :.. b) 0)}]
;                        [t/Any :* :-> t/Any])]))
cc/some-fn 
(t/All [t0 t1 t2 t3 t4 t5]
       (t/IFn [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/U t0 t1) 0) :else (! (t/U t0 t1) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/U t0 t1 t2) 0) :else (! (t/U t0 t1 t2) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any :-> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/U t0 t1 t2 t3) 0) :else (! (t/U t0 t1 t2 t3) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any :-> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any :-> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/U t0 t1 t2 t3 t4) 0) :else (! (t/U t0 t1 t2 t3 t4) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any :-> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any :-> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               [t/Any :-> t/Bool :filters {:then (is t5 0) :else (! t5 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/U t0 t1 t2 t3 t4 t5) 0) :else (! (t/U t0 t1 t2 t3 t4 t5) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Any] :+ :-> [t/Any :* :-> t/Any]]))
cc/every-pred
(t/All [t0 t1 t2 t3 t4 t5]
       (t/IFn [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/I t0 t1) 0) :else (! (t/I t0 t1) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/I t0 t1 t2) 0) :else (! (t/I t0 t1 t2) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any :-> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/I t0 t1 t2 t3) 0) :else (! (t/I t0 t1 t2 t3) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Bool :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Bool :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Bool :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any :-> t/Bool :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any :-> t/Bool :filters {:then (is t4 0) :else (! t4 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/I t0 t1 t2 t3 t4) 0) :else (! (t/I t0 t1 t2 t3 t4) 0)}]
                          [t/Any :* :-> t/Any])]
              [[t/Any :-> t/Any :filters {:then (is t0 0) :else (! t0 0)}] 
               [t/Any :-> t/Any :filters {:then (is t1 0) :else (! t1 0)}]
               [t/Any :-> t/Any :filters {:then (is t2 0) :else (! t2 0)}]
               [t/Any :-> t/Any :filters {:then (is t3 0) :else (! t3 0)}]
               [t/Any :-> t/Any :filters {:then (is t4 0) :else (! t4 0)}]
               [t/Any :-> t/Any :filters {:then (is t5 0) :else (! t5 0)}]
               :-> (t/IFn [t/Any :-> t/Bool :filters {:then (is (t/I t0 t1 t2 t3 t4 t5) 0) :else (! (t/I t0 t1 t2 t3 t4 t5) 0)}]
                          [t/Any :* :-> t/Any])]
              [(t/+ [t/Any :-> t/Any]) :-> [t/Any :* :-> t/Any]]))

cc/concat (t/All [x] [(t/Seqable x) :* :-> (t/ASeq x)])

cc/set (t/All [x] [(t/Seqable x) :-> #?(:cljs (t/Set x)
                                        :default (PersistentHashSet x))])
cc/hash-set (t/All [x] [x :* :-> #?(:cljs (t/Set x)
                                    :default (PersistentHashSet x))])
;TODO
;cc/hash-map (t/All [x y z :..]
;                   (t/IFn [(t/cat z z) :.. z :-> (t/Assoc '{} z :.. z)]
;                          [(t/cat x y) :* :-> (t/Map x y)]))
cc/hash-map (t/All [x y] [(t/cat x y) :* :-> (t/Map x y)])
cc/array-map (t/All [x y] [(t/cat x y) :* :-> (t/Map x y)])
cc/sorted-map (t/All [x y] [(t/cat x y) :* :-> (t/SortedMap x y)])
cc/sorted-map-by (t/All [x y] [[x x :-> t/Int] (t/cat x y) :* :-> (t/SortedMap x y)])
cc/sorted-set (t/All [x] [x :* :-> (t/SortedSet x)])
;;FIXME use t/Comparator for first arg
cc/sorted-set-by (t/All [x] [[x x :-> t/Int] x :* :-> (t/SortedSet x)])
cc/list (t/All [x] [x :* :-> (#?(:clj PersistentList :cljs t/List) x)])
;cc/list* (t/All [x] [x :* (t/Seqable x) :-> (t/NilableNonEmptyASeq x)])
cc/list* (t/All [x] (t/IFn [(t/Seqable x) :-> (t/NilableNonEmptyASeq x)]
                           [x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x x x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]
                           [x x x x x x x x x x (t/Seqable x) :-> (t/NonEmptyASeq x)]))

cc/list? (t/Pred (t/List t/Any))
#?@(:cljs [] :default [
cc/load-reader [java.io.Reader :-> t/Any]
])

cc/methods [t/Multi :-> (t/Map t/Any t/Any)]

cc/munge (t/IFn [t/Sym :-> t/Sym]
                [t/Str :-> t/Str]
                [t/Any :-> (t/U t/Sym t/Str)])

cc/pos? [t/Num :-> t/Bool]
cc/neg? [t/Num :-> t/Bool]

cc/nthrest (t/All [x] [(t/Seqable x) t/AnyInteger :-> (t/ASeq x)])

cc/vector (t/All [r b :..]
                 (t/IFn [b :.. b :-> '[b :.. b]]
                        [r :* :-> (t/AVec r)]))
cc/vec (t/All [x] [(t/Seqable x) :-> (t/AVec x)])

;#?@(:cljs [] :default [
;;TODO annotate clojure.core.Vec
;cc/vector-of
;]

cc/not [t/Any :-> t/Bool]
cc/constantly (t/All [x] [x :-> [t/Any :* :-> x]])

#?@(:cljs [] :default [
cc/bound? [(t/Var2 t/Nothing t/Any) :* :-> t/Bool]
cc/thread-bound? [(t/Var2 t/Nothing t/Any) :* :-> t/Bool]
cc/bases [(t/Nilable Class) :-> (t/NilableNonEmptyASeq Class)]
])

cc/make-hierarchy [:-> t/Hierarchy]
cc/isa? [(t/? t/Hierarchy) t/Any t/Any :-> t/Bool]

cc/disj (t/All [x] (t/IFn [(t/SortedSet x) t/Any :* :-> (t/SortedSet x)]
                          [(t/Set x) t/Any :* :-> (t/Set x)]))

cc/assoc
(t/All [[m :< (t/Option (t/Associative t/Any t/Any))]
        k v c :..]
       (t/IFn [m k v (t/cat c c) :.. c :-> (t/Assoc m k v c :.. c)]
              [m k v (t/cat k v) :* :-> (t/Assoc m k v)]))

cc/update (t/All [[m :< (t/Option (t/Associative t/Any t/Any))]
                  k v c :..]
                 [m k [(t/Get m k) c :.. c :-> v] c :.. c :-> (t/Assoc m k v)])
;;TODO
;cc/update-in (t/All [m k :.. v c :..] [m (t/HSequential [k :.. k]) [(t/GetIn m (t/HSequential [k :.. k])) c :.. c :-> v] c :.. c :-> (t/AssocIn m (t/HSequential [k :.. k]) v)])

cc/dissoc (t/All [k v] (t/IFn [(t/Map k v) t/Any :* :-> (t/Map k v)]
                              [(t/Nilable (t/Map k v)) t/Any :* :-> (t/Nilable (t/Map k v))]))

cc/zipmap (t/All [k v] [(t/Seqable k) (t/Seqable v) :-> #?(:cljs (t/Map k v)
                                                          :default (APersistentMap k v))])

cc/keys (t/All [k] (t/IFn [(t/Map k t/Any) :-> (t/ASeq k) :object {:id 0 :path [Keys]}]
                          [(t/Seqable (t/MapEntry k t/Any)) :-> (t/Nilable (t/ASeq k))]))
cc/vals (t/All [v] (t/IFn [(t/Map t/Any v) :-> (t/ASeq v) :object {:id 0 :path [Vals]}]
                          [(t/Seqable (t/MapEntry t/Any v)) :-> (t/Nilable (t/ASeq v))]))

#_
(t/All [a :.. b :..] [[(Next... a) :-> a] :.. a
                      [b :.. b :-> (Last  (t/cat a :.. a))] :->
                      [b :.. b :-> (First (t/cat a :.. a))]])

cc/comp (t/All [a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 b :..]
               (t/IFn [                                                                                                                             :-> [a0 :-> a0]]
                      [                                                                                                            [b :.. b :-> a0] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0]                                                                                                 [b :.. b :-> a1] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1]                                                                                     [b :.. b :-> a2] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2]                                                                         [b :.. b :-> a3] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2] [a4 :-> a3]                                                             [b :.. b :-> a4] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2] [a4 :-> a3] [a5 :-> a4]                                                 [b :.. b :-> a5] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2] [a4 :-> a3] [a5 :-> a4] [a6 :-> a5]                                     [b :.. b :-> a6] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2] [a4 :-> a3] [a5 :-> a4] [a6 :-> a5] [a7 :-> a6]                         [b :.. b :-> a7] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2] [a4 :-> a3] [a5 :-> a4] [a6 :-> a5] [a7 :-> a6] [a8 :-> a7]             [b :.. b :-> a8] :-> [b :.. b :-> a0]]
                      [[a1 :-> a0] [a2 :-> a1] [a3 :-> a2] [a4 :-> a3] [a5 :-> a4] [a6 :-> a5] [a7 :-> a6] [a8 :-> a7] [a9 :-> a8] [b :.. b :-> a9] :-> [b :.. b :-> a0]]))


;apply: wishful thinking
;     (t/All [y b1 :.. b2 :..]
;          (t/IFn [[b1 :.. b1 b2 :.. b2 :-> y] b1 :.. b1 (t/HSequential [b2 :.. b2]) :-> y]
;                 [[b1 :.. b1 r :*      :-> y] b1 :.. b1 (t/Seqable r)               :-> y]))

cc/apply
(t/All [y a b c d r z :..]
       (t/IFn [[z :.. z :-> y] (t/U nil (t/HSequential [z :.. z])) :-> y]
              [[a z :.. z :-> y] a (t/U nil (t/HSequential [z :.. z])) :-> y]
              [[a b z :.. z :-> y] a b (t/U nil (t/HSequential [z :.. z])) :-> y]
              [[a b c z :.. z :-> y] a b c (t/U nil (t/HSequential [z :.. z])) :-> y]
              [[a b c d z :.. z :-> y] a b c d (t/U nil (t/HSequential [z :.. z])) :-> y]
              [[r :* :-> y] (t/Seqable r) :-> y]
              [[a r :* :-> y] a (t/Seqable r) :-> y]
              [[a b r :* :-> y] a b (t/Seqable r) :-> y]
              [[a b c r :* :-> y] a b c (t/Seqable r) :-> y]
              [[a b c d r :* :-> y] a b c d (t/Seqable r) :-> y]))

;partial: wishful thinking (replaces the first 4 arities)
; (t/All [r b1 :.. b2 :..]
;    [[b1 :.. b1 b2 :.. b2 :-> r] b1 :.. b1 :-> [b2 :.. b2 :-> r]])

cc/partial
(t/All [y a b c d z :..]
       (t/IFn [[z :.. z :-> y] :-> [z :.. z :-> y]]
              [[a z :.. z :-> y] a :-> [z :.. z :-> y]]
              [[a b z :.. z :-> y] a b :-> [z :.. z :-> y]]
              [[a b c z :.. z :-> y] a b c :-> [z :.. z :-> y]]
              [[a b c d z :.. z :-> y] a b c d :-> [z :.. z :-> y]]
              [[a :* :-> y] a :* :-> [a :* :-> y]]))

cc/str [t/Any :* :-> t/Str]
cc/prn-str [t/Any :* :-> t/Str]
cc/pr-str [t/Any :* :-> t/Str]
cc/newline [:-> nil]

cc/print [t/Any :* :-> nil]
cc/println [t/Any :* :-> nil]
cc/print-str [t/Any :* :-> t/Str]
cc/println-str [t/Any :* :-> t/Str]
#?@(:cljs [] :default [
cc/printf [t/Str t/Any :* :-> nil]
cc/format [t/Str t/Any :* :-> t/Str]
])
cc/pr [t/Any :* :-> nil]
cc/prn [t/Any :* :-> nil]
cc/flush [:-> nil]
cc/*print-length* (t/U nil false t/AnyInteger)
cc/*print-level* (t/U nil false t/AnyInteger)
#?@(:cljs [] :default [
cc/*verbose-defrecords* t/Bool
cc/print-ctor [Object [Object java.io.Writer :-> t/Any] java.io.Writer :-> nil]
])

cc/prefer-method [t/Multi t/Any t/Any :-> t/Any]
#?@(:cljs [] :default [
cc/print-simple [t/Any java.io.Writer :-> nil]
cc/char-escape-string (t/Map Character t/Str)
cc/char-name-string (t/Map Character t/Str)
cc/primitives-classnames (t/Map Class t/Str)
cc/namespace-munge [(t/U t/Sym t/Namespace) :-> t/Str]
;cc/find-protocol-impl ['{:on-interface Class
;                                   :impls ?}]

cc/re-matcher [t/Regex t/Str :-> java.util.regex.Matcher]
cc/re-groups [t/Regex :-> (t/U nil t/Str (t/Vec (t/Option t/Str)))]
])

cc/re-find (t/IFn #?(:clj [java.util.regex.Matcher :-> (t/U nil t/Str (t/Vec (t/Option t/Str)))])
                  [t/Regex t/Str :-> (t/U nil t/Str (t/Vec (t/Option t/Str)))])
cc/re-seq [t/Regex t/Str :-> (t/ASeq (t/U nil t/Str (t/Vec (t/Option t/Str))))]

cc/subs [t/Str t/Int (t/? t/Int) :-> t/Str]

;TODO
;cc/spit [java.io.Writer t/Any]

#?@(:cljs [] :default [
cc/future-call (t/All [x] [[:-> x] :-> (t/Future x)])

cc/volatile! (t/All [x] [x :-> (t/Volatile x)])
])

cc/atom (t/All [x] [x & :optional {:validator (t/Nilable [x :-> t/Any]) :meta t/Any} :-> (t/Atom2 x x)])

cc/deref (t/All [x #?(:clj y)]
                (t/IFn 
                  [(t/Deref x) :-> x]
                  #?(:clj [(t/U (t/Deref t/Any) java.util.concurrent.Future) :-> t/Any])
                  #?(:clj [(t/BlockingDeref x) t/AnyInteger y :-> (t/U x y)])
                  #?(:clj [(t/U java.util.concurrent.Future (t/BlockingDeref t/Any)) t/AnyInteger t/Any :-> t/Any])))

cc/delay? (t/Pred (t/Delay t/Any))

#?@(:cljs [] :default [
cc/future-cancelled? [java.util.concurrent.Future :-> t/Bool]
cc/future-cancel [java.util.concurrent.Future :-> t/Any]
cc/future? (t/Pred java.util.concurrent.Future)
cc/future-done? [java.util.concurrent.Future :-> t/Bool]
])

cc/force (t/All [x] (t/IFn [(t/Delay x) :-> x]
                           #_[(t/I x (t/Not (t/Delay t/Any))) :-> x]
                           [t/Any :-> t/Any]))

cc/realized? [#?(:clj clojure.lang.IPending
                 :cljs cljs.core/IPending) :-> t/Bool]

cc/select-keys (t/All [k v] [(t/Map k v) (t/Seqable t/Any) :-> (t/Map k v)])

cc/sort (t/All [x] [(t/? (t/I Comparator [x x :-> t/AnyInteger])) (t/Seqable x) :-> (t/ASeq x)])
cc/sort-by (t/All [a] [(t/? [a :-> Number]) (t/Seqable a) :-> (t/ASeq a)])
cc/replicate (t/All [a] [t/AnyInteger a :-> (t/ASeq a)])

cc/reset! (t/All [w r] [(t/Atom2 w r) w :-> w])
cc/swap! (t/All [w r b :..] [(t/Atom2 w r) [r b :.. b :-> w] b :.. b :-> w])
#?@(:cljs [] :default [
cc/reset-vals! (t/All [w r] [(t/Atom2 w r) w :-> '[r w]])
cc/swap-vals! (t/All [w r b :..] [(t/Atom2 w r) [r b :.. b :-> w] b :.. b :-> '[r w]])
cc/vreset! (t/All [w] [(t/Volatile2 w t/Infer) w :-> w])
])
cc/compare-and-set! (t/All [w] [(t/Atom2 w t/Any) t/Any w :-> t/Bool])

cc/set-validator! (t/All [x] [#?(:clj (clojure.lang.IRef x x)
                                 :cljs (cljs.core/Atom x x))
                              (t/Nilable [x :-> t/Any]) :-> t/Any])
cc/get-validator (t/All [x] [#?(:clj (clojure.lang.IRef x x)
                                 :cljs (cljs.core/Atom x x))
                             :-> (t/Nilable [x :-> t/Any])])

#?@(:cljs [] :default [
cc/alter-var-root (t/All [w r b :..] [(t/Var2 w r) [r b :.. b :-> w] b :.. b :-> w])

cc/method-sig [java.lang.reflect.Method :-> '[t/Any (t/NilableNonEmptySeq t/Any) t/Any]]
cc/proxy-name [Class (t/Seqable Class) :-> t/Str]
cc/get-proxy-class [Class :* :-> Class]
cc/construct-proxy [Class t/Any :* :-> t/Any]
cc/init-proxy [t/Proxy (t/Map t/Str t/Any) :-> t/Proxy]
cc/update-proxy [t/Proxy (t/Map t/Str t/Any) :-> t/Proxy]
cc/proxy-mappings [t/Proxy :-> (t/Map t/Str t/Any)]
cc/proxy-call-with-super (t/All [x] [[:-> x] t/Proxy t/Str :-> x])
cc/bean [Object :-> (t/Map t/Any t/Any)]
])

cc/fnil (t/All [x y z a b :..] (t/IFn [[x b :.. b :-> a] x :-> [(t/Nilable x) b :.. b :-> a]]
                                      [[x y b :.. b :-> a] x y :-> [(t/Nilable x) (t/Nilable y) b :.. b :-> a]]
                                      [[x y z b :.. b :-> a] x y z :-> [(t/Nilable x) (t/Nilable y) (t/Nilable z) b :.. b :-> a]]))

cc/symbol (t/IFn [(t/U t/Kw t/Sym t/Str (t/Var2 t/Nothing t/Any)) :-> t/Sym]
                 [(t/U nil t/Str) t/Str :-> t/Sym])

cc/keyword
(t/IFn [(t/U t/Keyword t/Sym t/Str) :-> t/Keyword 
        :object {:id 0 :path [Keyword]}
        :filters {:then tt
                  :else ff}]
       [nil :-> nil 
        :object {:id 0 :path [Keyword]}
        :filters {:then ff
                  :else tt}]
       [t/Any :-> (t/U nil t/Keyword) 
        :object {:id 0 :path [Keyword]}
        :filters {:then (is (t/U t/Keyword t/Sym t/Str) 0)
                  :else (! (t/U t/Keyword t/Sym t/Str) 0)}]
       [(t/Option t/Str) t/Str :-> t/Keyword
        :filters {:then tt
                  :else ff}])

#?@(:cljs [] :default [
cc/find-keyword
[(t/alt (t/cat (t/U t/Keyword t/Sym t/Str))
        (t/cat t/Str t/Str))
 :-> (t/Option t/Keyword)]
])

cc/derive [t/Hierarchy :?, (t/U t/Named #?(:clj Class)), t/Named :-> t/Hierarchy]

;; already defined in clj base-env
#?@(:cljs [
cc/compare (t/All [x]
                  [(t/alt (t/cat (t/Option t/Num) (t/Option t/Num))
                          (t/cat (t/Option (t/Comparable x)) (t/Option (t/Comparable x))))
                   :-> t/Num])
])

#?@(:cljs [] :default [
cc/require [t/Any :* :-> nil]
cc/requiring-resolve [t/Sym :-> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
cc/use [t/Any :* :-> nil]
cc/*loaded-libs* (t/Ref1 (t/Set t/Sym))
])

cc/seq? (t/Pred (t/Seq t/Any))
cc/set? (t/Pred (t/Set t/Any))
cc/vector? (t/Pred (t/Vec t/Any))
cc/nil? (t/Pred nil)
#?@(:cljs [
cc/undefined? (t/Pred t/JSundefined)
])
cc/false? (t/Pred false)
cc/true? (t/Pred true)
cc/symbol? (t/Pred t/Sym)
cc/keyword? (t/Pred t/Keyword)
cc/map? (t/Pred (t/Map t/Any t/Any))
cc/boolean? (t/Pred t/Bool)
cc/any? [t/Any :-> true]

; would be nice
; (t/Pred (t/Not nil))
cc/some? [t/Any :-> t/Bool :filters {:then (! nil 0)
                                    :else (is nil 0)}]

#?@(:cljs [] :default [
cc/cast (t/All [x] [Class x :-> x])
])

cc/associative? (t/Pred (t/Associative t/Any t/Any))
cc/coll? (t/Pred (t/Coll t/Any))
      ;TODO should these be parameterized?
cc/sequential? (t/Pred t/Sequential)
cc/sorted? (t/Pred (t/Sorted t/Any))
cc/meta [t/Any :-> (t/Nilable (t/Map t/Any t/Any))]
;; FIXME IObj annotations are a hack. doesn't literally return the same reference.
cc/with-meta (t/All [[x :< #?(:clj clojure.lang.IObj
                              :cljs cljs.core/IWithMeta)]]
                    [x (t/Nilable (t/Map t/Any t/Any)) :-> x])
cc/vary-meta (t/All [[x :< #?(:clj clojure.lang.IObj
                              :cljs cljs.core/IWithMeta)] b :..]
                    [x [(t/Nilable (t/Map t/Any t/Any)) b :.. b :-> (t/Nilable (t/Map t/Any t/Any))] b :.. b :-> x])

cc/reset-meta! [clojure.lang.IReference (t/Nilable (t/Map t/Any t/Any)) :-> (t/Nilable (t/Map t/Any t/Any))]
cc/alter-meta! 
(t/All [b :..]
       [clojure.lang.IReference
        [(t/Nilable (t/Map t/Any t/Any)) b :.. b :->
         (t/Nilable (t/Map t/Any t/Any))] b :.. b :-> (t/Nilable (t/Map t/Any t/Any))])

#?@(:cljs [] :default [
cc/commute (t/All [w r b :..] [(t/Ref2 w r) [r b :.. b :-> w] b :.. b :-> w])
cc/alter (t/All [w r b :..] [(t/Ref2 w r) [r b :.. b :-> w] b :.. b :-> w])
])

cc/cycle (t/All [x] [(t/Seqable x) :-> (t/NonEmptyASeq x)])

#?@(:cljs [] :default [
cc/compile [t/Sym :-> t/Sym]
])

cc/comparator (t/All [x y] [[x y :-> t/Any] :-> (t/I Comparator [x y :-> t/AnyInteger])])

#?@(:cljs [] :default [
cc/seq-to-map-for-destructuring [(t/Seqable t/Any) :-> t/Any]
;cc/seq-to-map-for-destructuring (t/All [x] (t/IFn [(t/HSeq [x]) :-> x]
;                                                  ;; could be anything if seq is of count 1
;                                                  [(t/Seq t/Any) :-> t/Any]))
cc/destructure [t/Any :-> t/Any]
])

cc/distinct (t/All [x] (t/IFn [:-> (t/Transducer x x)]
                              [(t/Seqable x) :-> (t/ASeq x)]))

cc/string? (t/Pred t/Str)
cc/char? #?(:clj (t/Pred Character)
            :cljs [t/Any :-> t/Bool :filters {:then (is t/Str 0)}])

clojure.string/split
[t/Str t/Regex (t/? t/AnyInteger) :-> (t/AVec t/Str)]

clojure.string/join
[(t/? t/Any) (t/Seqable t/Any) :-> t/Str]

clojure.string/upper-case [CharSequence :-> t/Str]

clojure.string/blank? [(t/U nil t/Str) :-> t/Bool]
clojure.string/capitalize [t/Str :-> t/Str]
clojure.string/lower-case [t/Str :-> t/Str]
clojure.string/replace [(t/alt (t/cat t/Str t/Str t/Str)
                               (t/cat t/Str Character Character)
                               (t/cat t/Str t/Regex (t/U t/Str [t/Str :-> t/Str]))) :-> t/Str]
clojure.string/replace-first [(t/alt (t/cat t/Str t/Str t/Str)
                                     (t/cat t/Str Character Character)
                                     (t/cat t/Str t/Regex (t/U t/Str [t/Str :-> t/Str]))) :-> t/Str]
clojure.string/reverse [t/Str :-> t/Str]
clojure.string/trim [t/Str :-> t/Str]
clojure.string/trimr [t/Str :-> t/Str]
clojure.string/triml [t/Str :-> t/Str]

clojure.data/diff [t/Any t/Any :-> '[t/Any t/Any t/Any]]

#?@(:cljs [] :default [
cljs.instant/read-instant-instant [CharSequence :-> java.sql.Timestamp] ;; clj macros file
clojure.instant/read-instant-date [t/Str :-> java.util.Date]
clojure.instant/read-instant-calendar [t/Str :-> java.util.GregorianCalendar]
clojure.instant/read-instant-timestamp [t/Str :-> java.sql.Timestamp]
])

#?@(:cljs [] :default [
clojure.repl/apropos [(t/U t/Str t/Regex) :-> (t/Seq t/Sym)]
clojure.repl/demunge [t/Str :-> t/Str]
clojure.repl/source-fn [t/Sym :-> (t/U t/Str nil)]
])

#?@(:cljs [] :default [
clojure.template/apply-template [(t/Vec t/Any) t/Any (t/Seqable t/Any) :-> t/Any]
])

clojure.set/subset? [(t/Set t/Any) (t/Set t/Any) :-> t/Bool]
clojure.set/superset? [(t/Set t/Any) (t/Set t/Any) :-> t/Bool]
clojure.set/join [(t/Set (t/Map t/Any t/Any)) (t/Set (t/Map t/Any t/Any)) (t/? (t/Map t/Any t/Any)) :-> (t/Set (t/Map t/Any t/Any))]

 ; would be nice
;(t/All [[m :> (t/Map t/Any t/Any)] k]
;     [(t/Set m) (t/Seqable k) :-> (t/Map (t/Map k (Get m k)) (t/Set m))]
;     )
clojure.set/index (t/All [x y] [(t/Set (t/Map x y)) (t/Seqable t/Any) :-> (t/Map (t/Map t/Any t/Any) (t/Set (t/Map x y)))])
clojure.set/map-invert (t/All [a b] [(t/Map a b) :-> (t/Map b a)])

 ;would be nice, not quite correct though
; (t/All [x y [m :< (t/Map x y)] k]
;    [(t/Set m) (t/Vec k) :-> (t/Set (t/Map k (Get m k)))])
clojure.set/project (t/All [x y] [(t/Set (t/Map x y)) (t/Vec t/Any) :-> (t/Set (t/Map x y))])
clojure.set/rename (t/All [x y] [(t/Set (t/Map x y)) (t/Map t/Any x) :-> (t/Set (t/Map x y))])
clojure.set/rename-keys (t/All [x y] [(t/Map x y) (t/Map t/Any x) :-> (t/Map x y)])
 ;like filter
clojure.set/select (t/All [x y] (t/IFn [[x :-> t/Any :filters {:then (is y 0)}] (t/Set x) :-> (t/Set y)]
                                       [[x :-> t/Any :filters {:then (! y 0)}] (t/Set x) :-> (t/Set (t/I x (t/Not y)))]
                                       [[x :-> t/Any] (t/Set x) :-> (t/Set x)]))
clojure.set/union (t/All [x] [(t/Set x) :* :-> (t/Set x)])
clojure.set/intersection (t/All [x] [(t/+ (t/Set x)) :-> (t/Set x)])
clojure.set/difference (t/All [x] [(t/Set x) (t/Set t/Any) :* :-> (t/Set x)])
 
; FIXME should be [t/Str [t/Any :-> t/Any] :-> t/Str]
clojure.string/escape [t/Str (t/U (t/Map t/Any t/Any) [t/Any :-> t/Any]) :-> t/Str]
clojure.string/split-lines [t/Str :-> (t/Vec t/Str)]

#?@(:cljs [] :default [
clojure.test/function? [t/Any :-> t/Bool]
clojure.test/assert-any [t/Any t/Any :-> t/Any]
clojure.test/do-report [t/Any :-> t/Any]
clojure.test/run-tests [t/Sym :* :-> (t/Map t/Any t/Any)]
clojure.test/run-all-tests [(t/? t/Regex) :-> (t/Map t/Any t/Any)]
clojure.test/successful? [(t/U nil (t/Map t/Any t/Any)) :-> t/Bool]
clojure.test/compose-fixtures [[[:-> t/Any] :-> t/Any] [[:-> t/Any] :-> t/Any] :-> [[:-> t/Any] :-> t/Any]]
clojure.test/testing-vars-str [(t/Map t/Any t/Any) :-> t/Str]
clojure.test/testing-contexts-str [:-> t/Str]
clojure.test/test-ns [(t/U t/Namespace t/Sym) :-> (t/Map t/Any t/Any)]
clojure.test/test-var [(t/Var2 t/Nothing t/Any) :-> t/Any]

clojure.test.tap/print-tap-plan [t/Any :-> t/Any]
clojure.test.tap/print-tap-diagnostic [t/Str :-> t/Any]
clojure.test.tap/print-tap-pass [t/Any :-> t/Any]
clojure.test.tap/print-tap-fail [t/Any :-> t/Any]

clojure.java.javadoc/add-local-javadoc [t/Any :-> (t/List t/Any)]
clojure.java.javadoc/add-remote-javadoc [t/Str t/Any :-> (t/Map t/Any t/Any)]
clojure.java.javadoc/javadoc [t/Any :-> t/Any]
])

clojure.edn/read-string [(t/U t/Str nil) :-> t/Any]

#?@(:cljs [] :default [
clojure.java.shell/sh [t/Any :*
                       ;would be nice (combine :* and kw args)
                       ; t/Str :*
                       ;& :optional {:in t/Any  ;; any valid input to clojure.java.io/copy
                       ;             :inc-enc t/Str :out-env (t/U ':bytes t/Str)
                       ;             :env (t/U (Array t/Str) (t/Map t/Any t/Any))
                       ;             :dir (t/U t/Str java.io.File)}
                       :-> '{:exit t/Str
                            :out (t/U (Array byte) t/Str)
                            :err t/Str}]
clojure.java.browse/browse-url [t/Any :-> t/Any]
clojure.java.io/delete-file [clojure.java.io/Coercions (t/? t/Any) :-> t/Any]
clojure.java.io/make-parents [(t/+ clojure.java.io/Coercions) :-> t/Any]
clojure.java.io/file [(t/+ clojure.java.io/Coercions) :-> java.io.File]
clojure.java.io/as-relative-path [clojure.java.io/Coercions :-> t/Str]
;; TODO second arg is flattened IOFactoryOpts
clojure.java.io/reader [clojure.java.io/IOFactory :-> java.io.BufferedReader]
;; TODO second arg is flattened IOFactoryOpts
clojure.java.io/writer [clojure.java.io/IOFactory :-> java.io.BufferedWriter]
clojure.java.io/resource [t/Str (t/? ClassLoader) :-> (t/Nilable java.net.URL)]
clojure.stacktrace/e [:-> t/Any]
clojure.stacktrace/print-cause-trace [Throwable :-> t/Any]
clojure.stacktrace/print-stack-trace [Throwable :-> t/Any]
clojure.stacktrace/print-throwable [Throwable :-> t/Any]
clojure.stacktrace/root-cause [Throwable :-> Throwable]
;; FIXME keyword arguments
clojure.reflect/reflect [(t/+ t/Any) :-> (t/Map t/Any t/Any)]
clojure.inspector/atom? [t/Any :-> t/Bool]
clojure.inspector/collection-tag [t/Any :-> t/Keyword]
clojure.inspector/tree-model [t/Any :-> t/Any]
clojure.inspector/old-table-model [(t/Seqable t/Any) :-> t/Any]
clojure.inspector/inspect [t/Any :-> javax.swing.JFrame]
clojure.inspector/inspect-tree [t/Any :-> javax.swing.JFrame]
clojure.inspector/inspect-table [(t/Seqable t/Any) :-> javax.swing.JFrame]
])

#?@(:cljs [] :default [
clojure.main/demunge [t/Str :-> t/Str]
clojure.main/repl-prompt [:-> t/Any]
clojure.main/repl-read [t/Any t/Any :-> t/Any]
clojure.main/repl-caught [Throwable :-> t/Any]
clojure.main/repl-exception [Throwable :-> t/Any]
clojure.main/root-cause [Throwable :-> Exception]
clojure.main/repl [& :optional {:init [:-> t/Any]
                                :need-prompt [:-> t/Any]
                                :prompt [:-> t/Any]
                                :flush [:-> t/Any]
                                :read [t/Any t/Any :-> t/Any]
                                :eval [t/Any :-> t/Any]
                                :print [t/Any :-> t/Any]
                                :caught [Throwable :-> t/Any]}
                   :-> t/Any]
clojure.main/main [t/Any :* :-> t/Any]
clojure.main/load-script [t/Str :-> t/Any]
])

clojure.walk/keywordize-keys [t/Any :-> t/Any]
clojure.walk/macroexpand-all [t/Any :-> t/Any]
clojure.walk/postwalk [[t/Any :-> t/Any] t/Any :-> t/Any]
clojure.walk/postwalk-demo [t/Any :-> t/Any]
clojure.walk/postwalk-replace [(t/Map t/Any t/Any) t/Any :-> t/Any]
clojure.walk/prewalk [[t/Any :-> t/Any] t/Any :-> t/Any]
clojure.walk/prewalk-demo [t/Any :-> t/Any]
clojure.walk/prewalk-replace [(t/Map t/Any t/Any) t/Any :-> t/Any]
clojure.walk/stringify-keys [t/Any :-> t/Any]
clojure.walk/walk [[t/Any :-> t/Any] [t/Any :-> t/Any] t/Any :-> t/Any]

clojure.zip/zipper [[t/Any :-> t/Any] [(t/Seqable t/Any) :-> (t/U nil (t/Seq t/Any))] 
                    [t/Any (t/U nil (t/Seq t/Any)) :-> t/Any]
                    t/Any 
                    :-> (t/Vec t/Any)]
clojure.zip/seq-zip [t/Any :-> (t/Vec t/Any)]
clojure.zip/vector-zip [t/Any :-> (t/Vec t/Any)]
clojure.zip/xml-zip [t/Any :-> (t/Vec t/Any)]
clojure.zip/node [(t/Vec t/Any) :-> t/Any]
clojure.zip/branch? [(t/Vec t/Any) :-> t/Bool]
clojure.zip/children [(t/Vec t/Any) :-> (t/U nil (t/Seq t/Any))]
clojure.zip/root [(t/Vec t/Any) :-> t/Any]
clojure.zip/rightmost [(t/Vec t/Any) :-> (t/Vec t/Any)]
clojure.zip/right [(t/Vec t/Any) :-> t/Any]
clojure.zip/up [(t/Vec t/Any) :-> (t/U nil (t/Vec t/Any))]
clojure.zip/rights [(t/Vec t/Any) :-> t/Any]
clojure.zip/replace [(t/Vec t/Any) t/Any :-> (t/Vec t/Any)]
clojure.zip/down [(t/Vec t/Any) :-> (t/U (t/Vec t/Any) nil)]
clojure.zip/left [(t/Vec t/Any) :-> (t/U (t/Vec t/Any) nil)]
clojure.zip/lefts [(t/Vec t/Any) :-> (t/U (t/Vec t/Any) nil)]
clojure.zip/leftmost [(t/Vec t/Any) :-> (t/U (t/Vec t/Any) nil)]
clojure.zip/append-child [(t/Vec t/Any) t/Any :-> (t/Vec t/Any)]
clojure.zip/branch? [(t/Vec t/Any) :-> t/Bool]
clojure.zip/end? [(t/Vec t/Any) :-> t/Bool]
clojure.zip/insert-child [(t/Vec t/Any) t/Any :-> (t/Vec t/Any)]
clojure.zip/insert-left [(t/Vec t/Any) t/Any :-> (t/Vec t/Any)]
clojure.zip/insert-right [(t/Vec t/Any) t/Any :-> (t/Vec t/Any)]
clojure.zip/next [(t/Vec t/Any) :-> (t/Vec t/Any)]
clojure.zip/prev [(t/Vec t/Any) :-> (t/U (t/Vec t/Any) nil)]
;; more to say here
clojure.zip/path [(t/Vec t/Any) :-> t/Any]
clojure.zip/remove [(t/Vec t/Any) :-> (t/Vec t/Any)]

cc/interpose (t/All [x] (t/IFn [x :-> (t/Transducer x x)]
                               [x (t/Seqable x) :-> (t/ASeq x)]))
cc/interleave (t/All [x] [(t/Seqable x) :* :-> (t/ASeq x)])

cc/repeat (t/All [x] (t/IFn [x :-> (t/NonEmptyASeq x)]
                            [t/AnyInteger x :-> (t/ASeq x)]))

;cc/every? (t/All [x y] 
;                         (t/IFn [[x :-> t/Any :filters {:then (is y 0)}] (t/Coll x) :-> t/Bool
;                              :filters {:then (is (t/Coll (t/I x y)) 1)}]
;                             ; argument could be nil
;                             [[x :-> t/Any :filters {:then (is y 0)}] (t/U nil (t/Coll x)) :-> t/Bool
;                              :filters {:then (is (t/U nil (t/Coll (t/I x y))) 1)}]
;                             [[x :-> t/Any] (t/Seqable x) :-> t/Bool]))
cc/every? (t/All [x y] (t/IFn [[x :-> t/Any :filters {:then (is y 0)}] (t/Coll x) :-> t/Bool
                               :filters {:then (is (t/Coll y) 1)}]
                              ; argument could be nil
                              [[x :-> t/Any :filters {:then (is y 0)}] (t/U nil (t/Coll x)) :-> t/Bool
                               :filters {:then (is (t/U nil (t/Coll y)) 1)}]
                              [[x :-> t/Any] (t/Seqable x) :-> t/Bool]))

cc/range (t/IFn [:-> (t/NonEmptyASeq t/AnyInteger)]
                [t/Num :-> (t/ASeq t/AnyInteger)]
                [t/AnyInteger t/Num (t/? t/AnyInteger) :-> (t/ASeq t/AnyInteger)]
                [t/Num t/Num (t/? t/Num) :-> (t/ASeq t/Num)])

#?@(:cljs [] :default [
cc/class (t/IFn [nil :-> nil :object {:id 0 :path [Class]}]
                [Object :-> Class :object {:id 0 :path [Class]}]
                [t/Any :-> (t/Option Class) :object {:id 0 :path [Class]}])
])

; need better metadata support if this even has a chance of working
; like class
;; idea: provide global registry of :type mappings to qualified keywords.
;; users can register their types to opt in. Add a new path element so we
;; can refine the type if the result is found to be a kw (and if the arg has immutable metadata),
;; eg., cc/type [t/Any :-> t/Any :obj {:id 0 :path [Type]}]
cc/type [t/Any :-> t/Any]

;;TODO clojure.core/not-empty
cc/seq (t/All [[x :< (t/Seqable t/Any)]]
              [x :-> (t/SeqOn x) :object {:id 0 :path [Seq]}])
#_(t/All [x] (t/IFn [(t/NonEmptyColl x) :-> (t/NonEmptyASeq x)
                          :filters {:then tt
                                    :else ff}]
                         [(t/Option (t/Coll x)) :-> (t/NilableNonEmptyASeq x)
                          :filters {:then (& (is t/NonEmptyCount 0)
                                             (! nil 0))
                                    :else (| (is nil 0)
                                             (is t/EmptyCount 0))}]
                         [(t/Seqable x) :-> (t/NilableNonEmptyASeq x)]))

; t/Seqable [[x :variance :covariant]
;          :count [l :variance :covariant :< AnyCountRange]
;          :to-seq [sfn :kind (t/TFn [[x :variance :covariant]]
;                               (t/I IWithMeta (IMeta nil) (t/Seq x) (ICollection x) 
;                                  IEmptyableCollection ISequential))]]

; cc/seq (t/All [x
;                        [sfn :kind [* :-> *]]
;                    (t/IFn
;                      [(t/Seqable x :count (t/CountRange 1) :to-seq sfn) :-> (sfn x)]
;                      [(t/Seqable x :count AnyCountRange :to-seq sfn) :-> (t/U nil (sfn x))]))

cc/empty? (t/IFn [(t/Option (t/HSequential [t/Any :*])) :-> t/Bool
                  :filters {:then (| (is t/EmptyCount 0)
                                     (is nil 0))
                            :else (is t/NonEmptyCount 0)}]
                 [(t/Option (t/Coll t/Any)) :-> t/Bool
                  :filters {:then (| (is t/EmptyCount 0)
                                     (is nil 0))
                            :else (is t/NonEmptyCount 0)}]
                 [(t/Seqable t/Any) :-> t/Bool])

cc/map (t/All [c a b :..] (t/IFn [[a :-> c] :-> (t/Transducer a c)]
                                 [[a b :.. b :-> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) :.. b :-> (t/NonEmptyASeq c)]
                                 [[a b :.. b :-> c] (t/Seqable a) (t/Seqable b) :.. b :-> (t/ASeq c)]))
cc/mapv (t/All [c a b :..] (t/IFn [[a b :.. b :-> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) :.. b :-> (t/NonEmptyAVec c)]
                                  [[a b :.. b :-> c] (t/Seqable a) (t/Seqable b) :.. b :-> (t/AVec c)]))
cc/mapcat (t/All [c a b :..] (t/IFn
                               [[a :-> (t/Seqable c)] :-> (t/Transducer a c)]
                               [[a b :.. b :-> (t/Seqable c)] (t/Seqable a) (t/Seqable b) :.. b :-> (t/ASeq c)]))
cc/cat (t/All [x] (t/Transducer (Seqable x) x))
#?@(:cljs [] :default [
cc/pmap (t/All [c a b :..] (t/IFn [[a b :.. b :-> c] (t/NonEmptySeqable a) (t/NonEmptySeqable b) :.. b :-> (t/NonEmptyASeq c)]
                                  [[a b :.. b :-> c] (t/Seqable a) (t/Seqable b) :.. b :-> (t/ASeq c)]))
cc/pcalls (t/All [r] [[:-> r] :* :-> (t/ASeq r)])
])

#_#_
cc/halt-when
(t/All [a d]
  [[a :-> t/Any] :-> (t/Transducer a a)]
  [[a :-> t/Any] (t/U nil [t/Any a :-> a]) :-> (t/Transducer a a)])

cc/frequencies (t/All [a] [(t/Seqable a) :-> (t/Map a t/AnyInteger)])

#?(:cljs cc/*clojurescript-version* :default cc/*clojure-version*)
'{:major t/Any :minor t/Any :incremental t/Any :qualifier t/Any}

#?@(:cljs [] :default [
cc/clojure-version [:-> t/Str]
])

#?@(:cljs [] :default [
cc/promise (t/All [x] [:-> (t/Promise x)])
cc/deliver (t/All [x] [(t/Promise x) x :-> (t/Nilable (t/Promise x))])
])

cc/flatten [(t/Seqable t/Any) :-> (t/Seq t/Any)]

cc/map-indexed (t/All [x y] (t/IFn [[t/Int x :-> y] :-> (t/Transducer x y)]
                                   [[t/Int x :-> y] (t/Seqable x) :-> (t/ASeq y)]))
cc/keep-indexed (t/All [a c] (t/IFn [[t/Int a :-> (t/U nil c)] :-> (t/Transducer a c)]
                                    [[t/Int a :-> (t/U nil c)] (t/Seqable a) :-> (t/Seq c)]))
cc/bounded-count [(t/U t/Counted (t/Seqable t/Any)) :-> t/Int]
cc/keep (t/All [a b] (t/IFn [[a :-> (t/Option b)] :-> (t/Transducer a b)]
                            [[a :-> (t/Option b)] (t/Seqable a) :-> (t/ASeq b)]))
cc/dedupe (t/All [x] (t/IFn [:-> (t/Transducer x x)]
                            [(t/Seqable x) :-> (t/ASeq x)]))
cc/random-sample (t/All [x] (t/IFn [t/Num :-> (t/Transducer x x)]
                                   [t/Num (t/Seqable x) :-> (t/ASeq x)]))
cc/halt-when (t/All [x] (t/IFn [[x :-> t/Any] 
                                ;; TODO opens a can of worms. requires knowledge of transduction context.
                                ;; overrides final value of `into`, while doing nothing in `sequence`.
                                ;(t/Option [t/Any v :-> t/Any]) :?
                                :-> (t/Transducer x x)]
                               ))

#?@(:clj [
; (t/All [x ...] [(t/Transducer x y) (t/Seqable x) :-> (Iterable y)])
cc/eduction (t/All [x y z a b] (t/IFn [(t/Seqable x) :-> (Iterable x)]
                                      [(t/Transducer x y) (t/Seqable x) :-> (Iterable y)]
                                      [(t/Transducer x y) (t/Transducer y z) (t/Seqable x) :-> (Iterable z)]
                                      [(t/Transducer x y) (t/Transducer y z) (t/Transducer z a) (t/Seqable x) :-> (Iterable a)]
                                      [(t/Transducer x y) (t/Transducer y z) (t/Transducer z a) (t/Transducer a b) (t/Seqable x) :-> (Iterable b)]))
]) 

cc/seqable? (t/Pred (t/Seqable t/Any))
cc/indexed? (t/Pred (t/Indexed t/Any))
cc/inst-ms [:-> t/Int]
cc/inst? (t/Pred cc/Inst)
cc/uuid? (t/Pred t/UUID)
cc/random-uuid [:-> t/UUID]
cc/parse-uuid [t/Str :-> (t/Option t/UUID)]
cc/uri? (t/Pred t/URI)
cc/tagged-literal? (t/Pred clojure.lang.TaggedLiteral)
cc/reader-conditional? (t/Pred clojure.lang.ReaderConditional)

cc/tagged-literal [t/Sym t/Any :-> clojure.lang.TaggedLiteral]
cc/reader-conditional [t/Any t/Bool :-> clojure.lang.ReaderConditional]

cc/add-tap [[t/Any :-> t/Any] :-> nil]
cc/remove-tap [t/Any :-> nil]
cc/tap> [t/Any :-> t/Bool]

cc/merge-with (t/All [k v] (t/IFn [[v v :-> v] nil :* :-> nil]
                                  [[v v :-> v] (t/Map k v) :* :-> (t/Map k v)]
                                  [[v v :-> v] (t/Option (t/Map k v)) :* :-> (t/Option (t/Map k v))]))

cc/reduce (t/All [a c] (t/IFn 
                         ;Without accumulator
                         ; default
                         ; (reduce + my-coll)
                         [[a a :-> (t/U (t/Reduced a) a)] (t/NonEmptySeqable a) :-> a]
                         [(t/IFn [a a :-> (t/U (t/Reduced a) a)] [:-> a]) (t/Seqable a) :-> a]
                         ; default
                         ; (reduce + 3 my-coll)
                         ; (reduce (fn [a b] a) (reduced 1) nil) 
                         ; ;=> (reduced 1)
                         [[a c :-> (t/U (t/Reduced a) a)] a (t/Seqable c) :-> a]))
cc/transduce (t/All [a b c] (t/IFn [(t/Transducer a a) (t/Reducer a a) (t/Seqable a) :-> a]
                                   [(t/Transducer b c) (t/IFn [c :-> c] [c a :-> (t/U c (t/Reduced c))]) a (t/Seqable b) :-> a]))
cc/reduce-kv (t/All [a k v] [[a k v :-> (t/U (t/Reduced a) a)] a (t/Option (t/Associative k v)) :-> a])
cc/reductions (t/All [a b] (t/IFn [[a a :-> (t/U (t/Reduced a) a)] (t/NonEmptySeqable a) :-> (t/NonEmptyASeq a)]
                                  [(t/IFn [:-> a] [a a :-> (t/U (t/Reduced a) a)]) (t/Seqable a) :-> (t/NonEmptyASeq a)]
                                  [[a b :-> (t/U (t/Reduced a) a)] a (t/Seqable b) :-> (t/NonEmptyASeq a)]))
cc/reduced (t/All [x] [x :-> (t/Reduced x)])
cc/unreduced (t/All [x] (t/IFn [(t/Reduced x) :-> x]
                               [(t/U x (t/Reduced x)) :-> x]))
cc/ensure-reduced (t/All [x] [(t/U x (t/Reduced x)) :-> (t/Reduced x)])
cc/completing (t/All [a b] [(t/IFn [:-> b] [b a :-> (t/U b (t/Reduced b))])
                            [b :-> b]
                            :-> (t/Reducer a b)])

#_(comment
  cc/reduce
       (t/All [a c d]
            (t/IFn 
              ;Without accumulator
              ; empty coll, f takes no args
              ; (reduce + []) => 0, (reduce + nil) => 0
              [[:-> c] (t/U nil (t/I (ExactCount 0) (t/Seqable c))) :-> c]
              ; coll count = 1, f is not called
              ; (reduce + [1]) => 1
              [t/Any (t/I (ExactCount 1) (t/Seqable c)) :-> c]
              ; coll count >= 2
              ; (reduce + [1 2]) => 3
              [[c c :-> c] (t/I (t/CountRange 2) (t/Seqable c)) :-> c]
              ; default
              ; (reduce + my-coll)
              [(t/IFn [c c :-> c] [:-> c]) (t/Seqable c) :-> c]
              ;With accumulator
              ; empty coll, f not called, returns accumulator
              ; (reduce + 3 []) => 3
              [t/Any a (t/U nil (t/I (ExactCount 0) (t/Seqable t/Any))) :-> a]
              ; default
              ; (reduce + 3 my-coll)
              [[a c :-> a] a (t/Seqable c) :-> a]))
  )

;should be special cased
cc/not= [(t/+ t/Any) :-> t/Bool]

cc/first (t/All [x] (t/IFn [(t/HSequential [x t/Any :*]) :-> x
                            :object {:id 0 :path [(Nth 0)]}]
                           [(t/EmptySeqable x) :-> nil]
                           [(t/NonEmptySeqable x) :-> x]
                           [(t/Seqable x) :-> (t/Option x)]))
cc/second (t/All [x] (t/IFn [(t/HSequential [t/Any x t/Any :*]) :-> x
                             :object {:id 0 :path [(Nth 1)]}]
                            [(t/Option (t/I (t/Seqable x) (t/CountRange 0 1))) :-> nil]
                            [(t/I (t/Seqable x) (t/CountRange 2)) :-> x]
                            [(t/Seqable x) :-> (t/Option x)]))
cc/ffirst (t/All [x] [(t/Seqable (t/Seqable x)) :-> (t/Nilable x)])
cc/nfirst (t/All [x] [(t/Seqable (t/Seqable x)) :-> (t/NilableNonEmptyASeq x)])
cc/group-by (t/All [x y] [[x :-> y] (t/Seqable x) :-> (t/Map y (t/NonEmptyAVec x))])
cc/fnext (t/All [x] [(t/Seqable x) :-> (t/Option x)])
cc/nnext (t/All [x] [(t/Seqable x) :-> (t/NilableNonEmptyASeq x)])
cc/nthnext (t/All [x] (t/IFn [nil t/AnyInteger :-> nil]
                             [(t/Seqable x) t/AnyInteger :-> (t/NilableNonEmptyASeq x)]))
cc/rest (t/All [x] [(t/Seqable x) :-> (t/ASeq x)])
cc/last (t/All [x] (t/IFn [(t/NonEmptySeqable x) :-> x]
                          [(t/Seqable x) :-> (t/U nil x)]))
cc/butlast (t/All [x] [(t/Seqable x) :-> (t/NilableNonEmptyASeq x)])
cc/next (t/All [x] (t/IFn [(t/Option (t/Coll x)) :-> (t/NilableNonEmptyASeq x)
                           :filters {:then (& (is (t/CountRange 2) 0)
                                              (! nil 0))
                                     :else (| (is (t/CountRange 0 1) 0)
                                              (is nil 0))}]
                          [(t/Seqable x) :-> (t/NilableNonEmptyASeq x)]))

cc/into
(t/All [x y :named [a]]
       (t/IFn [(t/Map x y) (t/Seqable (t/MapConjable x y)) :-> (t/Map x y)]
              [(t/Vec x) (t/Seqable x) :-> (t/Vec x)]
              [(t/Set x) (t/Seqable x) :-> (t/Set x)]
              ;unsound. t/Coll would need an extra param to specify Conjable elements.
              [(t/Coll t/Any) (t/Seqable t/Any) :-> (t/Coll t/Any)]
              ; transducer arities
              [(t/Map x y) (t/Transducer a (t/MapConjable x y)) (t/Seqable a) :-> (t/Map x y)]
              [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]
              [(t/Set x) (t/Transducer y x) (t/Seqable y) :-> (t/Set x)]
              ;unsound. t/Coll would need an extra param to specify Conjable elements.
              [(t/Coll t/Any) (t/Transducer y t/Any) (t/Seqable y) :-> (t/Coll t/Any)]))

cc/conj
;     (t/All [e
;           [Arg :< (t/TFn [[x :variance :covariant]] t/Any)]
;           [Res :< (t/TFn [[x :variance :covariant]]
;                     (t/Coll t/Any))]]
;          (t/IFn [(clojure.lang.IPersistentCollection e Arg Res) (Arg e) (Arg e) :* :-> (Res e)]
;              [nil e e :* :-> (clojure.lang.PersistentList e)]))
(t/All [x y] (t/IFn [(t/Vec x) (t/+ x) :-> (t/Vec x)]
                    #?(:clj [(APersistentMap x y) (t/+ (t/MapConjable x y)) :-> (APersistentMap x y)])
                    [(t/Map x y) (t/+ (t/MapConjable x y)) :-> (t/Map x y)]
                    [(t/Set x) (t/+ x) :-> (t/Set x)]
                    [(t/ASeq x) (t/+ x) :-> (t/ASeq x)]
                    #?(:clj [nil (t/+ x) :-> (clojure.lang.PersistentList x)])
                    ;unsound. t/Coll would need an extra param to specify Conjable elements.
                    [(t/Nilable (t/Coll t/Any)) (t/+ t/Any) :-> (t/Coll t/Any)]))

; IPersistentCollection [[x :variance :covariant]
;                        :conj-fn [conj-fn :kind (t/TFn [[x :variance :covariant]] (IPersistentCollection x))]
;                        :empty-fn [empty-fn :kind (t/TFn [] (IPersistentCollection t/Nothing :count (ExactCount 0)))]]

; cc/conj
;   (t/All [x conj-fn]
;     [(IPersistentCollection x :conj-fn conj-fn) x :-> (conj-fn x)]
;     [nil x :-> (PersistentList x)]
;     [(t/U nil (IPersistentCollection x :conj-fn conj-fn)) x :-> (t/U nil (conj-fn x))])

; cc/empty
;   (t/All [x empty-fn]
;      [(IPersistentCollection t/Any :empty-fn empty-fn) :-> (empty-fn)]
;      [nil :-> nil]
;      [(t/U nil (IPersistentCollection t/Any :empty-fn empty-fn)) :-> (t/U nil (empty-fn))])

cc/sequence (t/All [a b] (t/IFn [(t/Seqable a) :-> (t/ASeq a)]
                                ;;TODO rest arity
                                [(t/Transducer a b) (t/Seqable a) :-> (t/ASeq b)]))
cc/find (t/All [x y]
               (t/IFn [nil t/Any :-> nil]
                      [(t/Nilable (t/Associative x y)) t/Any :-> (t/Nilable (t/AMapEntry x y))]
                      ;#?(:clj [(t/Nilable (t/U (t/Associative t/Any t/Any) java.util.Map)) t/Any :-> (t/Nilable (java.util.Map$Entry t/Any t/Any))])
                      ;;TODO TransientAssociative2
                      ))

cc/get-in [t/Any (t/Nilable (t/Seqable t/Any)) (t/? t/Any) :-> t/Any]

cc/assoc-in [(t/Nilable (t/Associative t/Any t/Any)) (t/Seqable t/Any) t/Any :-> t/Any]

cc/update-keys (t/All [k k' v] [(t/Nilable (t/Associative k v)) [k :-> k'] :-> (t/Map k' v)])
cc/update-vals (t/All [k v v'] [(t/Nilable (t/Associative k v)) [v :-> v'] :-> (t/Map k v')])

cc/merge (t/All [k v] (t/IFn [nil :* :-> nil]
                             [(t/Map k v) (t/Nilable (t/Map k v)) :* :-> (t/Map k v)]
                             [(t/Option (t/Map k v)) :* :-> (t/Option (t/Map k v))]))

;more to be said here?
cc/contains? [(t/Seqable t/Any) t/Any :-> t/Bool]

cc/= [(t/+ t/Any) :-> t/Bool]
cc/identical? [t/Any t/Any :-> t/Bool]
#?@(:cljs [
cc/keyword-identical? [t/Any t/Any :-> t/Bool]
])
cc/distinct? [(t/+ t/Any) :-> t/Bool]

#?@(:cljs [] :default [
cc/rational? (t/Pred (t/U t/Int clojure.lang.Ratio BigDecimal))
cc/decimal? (t/Pred BigDecimal)
cc/denominator [clojure.lang.Ratio :-> t/Num]
])

cc/mod (t/IFn [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
              [t/Num t/Num :-> t/Num])

#?@(:cljs [] :default [
cc/var-get (t/All [r] [(t/Var2 t/Nothing r) :-> r])
cc/var-set (t/All [w] [(t/Var2 w t/Any) w :-> w])

cc/supers [Class :-> (t/U nil (t/I t/NonEmptyCount (t/Set Class)))]
])

cc/take-nth (t/All [x] [t/AnyInteger (t/Seqable x) :-> (t/ASeq x)])

cc/shuffle (t/All [x] (t/IFn [(t/I (Collection x) (t/Seqable x)) :-> (t/Vec x)]
                             [(Collection x) :-> (t/Vec x)]))

cc/special-symbol? [t/Any :-> t/Bool]

cc/integer? (t/Pred t/AnyInteger)
cc/int? (t/Pred #?(:clj (t/U Long
                             Integer
                             Short
                             Byte)
                   :cljs (t/U t/CLJSInteger
                              goog.math.Integer
                              goog.math.Long)))
cc/pos-int? [t/Any :-> t/Bool
             :filters {:then (is #?(:clj (t/U Long
                                              Integer
                                              Short
                                              Byte)
                                    :cljs (t/U t/CLJSInteger
                                               goog.math.Integer
                                               goog.math.Long)) 0)}]
cc/neg-int? [t/Any :-> t/Bool
             :filters {:then (is #?(:clj (t/U Long
                                              Integer
                                              Short
                                              Byte)
                                    :cljs (t/U t/CLJSInteger
                                               goog.math.Integer
                                               goog.math.Long)) 0)}]
cc/nat-int? [t/Any :-> t/Bool
             :filters {:then (is #?(:clj (t/U Long
                                              Integer
                                              Short
                                              Byte)
                                    :cljs (t/U t/CLJSInteger
                                               goog.math.Integer
                                               goog.math.Long)) 0)}]
cc/number? (t/Pred t/Num)
cc/double? (t/Pred #?(:clj Double
                      :cljs t/Num))
cc/float? (t/Pred #?(:clj (t/U Double Float)
                     :cljs t/Num))
cc/ident? (t/Pred t/Ident)
cc/simple-ident? [t/Any :-> t/Bool :filters {:then (is t/Ident 0)}]
cc/qualified-ident? [t/Any :-> t/Bool :filters {:then (is t/Ident 0)}]
cc/simple-symbol? [t/Any :-> t/Bool :filters {:then (is t/Sym 0)}]
cc/qualified-symbol? [t/Any :-> t/Bool :filters {:then (is t/Sym 0)}]
cc/simple-keyword? [t/Any :-> t/Bool :filters {:then (is t/Kw 0)}]
cc/qualified-keyword? [t/Any :-> t/Bool :filters {:then (is t/Kw 0)}]
cc/var? (t/Pred (t/Var2 t/Nothing t/Any))

#?@(:cljs [] :default [
cc/class? (t/Pred Class)
cc/bytes? (t/Pred (Array byte))
cc/resolve [(t/? t/Any) t/Sym :-> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
cc/ns-resolve (t/IFn [(t/U t/Sym t/Namespace) t/Sym :-> (t/U (t/Var2 t/Nothing t/Any) Class nil)]
                     ; should &env arg be more accurate?
                     [(t/U t/Sym t/Namespace) t/Any t/Sym :-> (t/U (t/Var2 t/Nothing t/Any) Class nil)])
cc/extenders [t/Any :-> (t/Seqable (t/Nilable Class))]
])

cc/+ (t/IFn #?(:clj [Long :* :-> Long])
            #?(:clj [(t/U Long Double) :* :-> Double])
            [t/AnyInteger :* :-> t/AnyInteger]
            [t/Num :* :-> t/Num])
cc/- (t/IFn #?(:clj [(t/+ Long) :-> Long])
            #?(:clj [(t/+ (t/U Long Double)) :-> Double])
            [(t/+ t/AnyInteger) :-> t/AnyInteger]
            [(t/+ t/Num) :-> t/Num])
cc/* (t/IFn #?(:clj [Long :* :-> Long])
            #?(:clj [(t/U Long Double) :* :-> Double])
            [t/AnyInteger :* :-> t/AnyInteger]
            [t/Num :* :-> t/Num])
cc// (t/IFn #?(:clj [Double (t/+ Double) :-> Double])
            [(t/+ t/Num) :-> t/Num])

#?@(:cljs [] :default [
cc/+' (t/IFn [t/AnyInteger :* :-> t/AnyInteger]
             [t/Num :* :-> t/Num])
cc/-' (t/IFn [(t/+ t/AnyInteger) :-> t/AnyInteger]
             [(t/+ t/Num) :-> t/Num])
cc/*' (t/IFn [t/AnyInteger :* :-> t/AnyInteger]
             [t/Num :* :-> t/Num])
])
cc/quot (t/IFn #?(:clj [Long Long :-> Long])
               #?(:clj [(t/U Long Double) (t/U Long Double) :-> Double])
               [t/AnyInteger t/AnyInteger :-> t/AnyInteger] 
               [t/Num t/Num :-> t/Num])

cc/unchecked-inc (t/IFn [t/AnyInteger :-> t/AnyInteger]
                        [t/Num :-> t/Num])
cc/unchecked-inc-int [t/Num :-> t/AnyInteger]
cc/unchecked-dec (t/IFn [t/AnyInteger :-> t/AnyInteger]
                        [t/Num :-> t/Num])
cc/unchecked-dec-int [t/Num :-> t/AnyInteger]
cc/unchecked-subtract (t/IFn [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
                             [t/Num t/Num :-> t/Num])
cc/unchecked-subtract-int [t/Num t/Num :-> t/AnyInteger]
cc/unchecked-negate (t/IFn [t/AnyInteger :-> t/AnyInteger]
                           [t/Num :-> t/Num])
cc/unchecked-negate-int [t/Num :-> t/AnyInteger]
cc/unchecked-add (t/IFn [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
                        [t/Num t/Num :-> t/Num])
cc/unchecked-add-int [t/Num t/Num :-> t/AnyInteger]
cc/unchecked-multiply (t/IFn [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
                             [t/Num t/Num :-> t/Num])
cc/unchecked-multiply-int [t/Num t/Num :-> t/AnyInteger]
cc/unchecked-divide-int [t/Num t/Num :-> t/AnyInteger]
cc/unchecked-remainder-int [t/Num t/Num :-> t/AnyInteger]
cc/rem (t/IFn [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
              [t/Num t/Num :-> t/Num])
cc/inc (t/IFn #?(:clj [Long :-> Long])
              #?(:clj [Double :-> Double])
              [t/AnyInteger :-> t/AnyInteger]
              [t/Num :-> t/Num])
cc/dec (t/IFn #?(:clj [Long :-> Long])
              #?(:clj [Double :-> Double])
              [t/AnyInteger :-> t/AnyInteger]
              [t/Num :-> t/Num])

#?@(:cljs [] :default [
cc/inc' (t/IFn [t/AnyInteger :-> t/AnyInteger]
               [t/Num :-> t/Num])
cc/dec' (t/IFn [t/AnyInteger :-> t/AnyInteger]
               [t/Num :-> t/Num])
cc/rationalize [t/Num :-> t/Num]
])

cc/abs (t/IFn #?(:clj [Long :-> Long])
              #?(:clj [Double :-> Double])
              [t/Num :-> t/Num])

cc/NaN? [#?(:cljs t/Num :default Double) :-> t/Bool]
cc/infinite? [#?(:cljs t/Num :default Double) :-> t/Bool]

cc/bit-not [t/AnyInteger :-> t/AnyInteger]
cc/bit-and [t/AnyInteger (t/+ t/AnyInteger) :-> t/AnyInteger]
cc/bit-or [t/AnyInteger (t/+ t/AnyInteger) :-> t/AnyInteger]
cc/bit-xor [t/AnyInteger (t/+ t/AnyInteger) :-> t/AnyInteger]
cc/bit-and-not [t/AnyInteger (t/+ t/AnyInteger) :-> t/AnyInteger]
cc/bit-clear [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
cc/bit-set [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
cc/bit-flip [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
cc/bit-test [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
cc/bit-shift-left [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
cc/bit-shift-right [t/AnyInteger t/AnyInteger :-> t/AnyInteger]
cc/unsigned-bit-shift-right [t/AnyInteger t/AnyInteger :-> t/AnyInteger]

clojure.math/E #?(:cljs t/Num :default Double)
clojure.math/PI #?(:cljs t/Num :default Double)
;TODO rest of clojure.math

cc/even? [t/AnyInteger :-> t/Bool]
cc/odd? [t/AnyInteger :-> t/Bool]

cc/peek (t/All [x] (t/IFn [(t/I t/NonEmptyCount (t/Stack x)) :-> x]
                          [(t/Stack x) :-> (t/Nilable x)]))
;;TODO assert non-empty arg
cc/pop (t/All [x] (t/IFn [(t/List x) :-> (t/List x)]
                         [(t/Vec x) :-> (t/Vec x)]
                         [(t/Stack x) :-> (t/Stack x)]))

#?@(:cljs [] :default [
cc/get-thread-bindings [:-> (t/Map (t/Var2 t/Nothing t/Any) t/Any)]
cc/bound-fn*
    (t/All [r b :..]
         [[b :.. b :-> r] :-> [b :.. b :-> r]])
cc/find-var
    [t/Sym :-> (t/U nil (t/Var2 t/Nothing t/Any))]
cc/agent
    (t/All [x] [x & :optional {:validator (t/U nil [x :-> t/Any]) :meta t/Any
                             :error-handler (t/U nil [(t/Agent1 x) Throwable :-> t/Any])
                             :error-mode (t/U ':continue ':fail)} 
              :-> (t/Agent1 x)])
cc/set-agent-send-executor! [java.util.concurrent.ExecutorService :-> t/Any]
cc/set-agent-send-off-executor! [java.util.concurrent.ExecutorService :-> t/Any]
cc/send-via (t/All [w r b :..] [(t/Agent2 w r) [r b :.. b :-> w] b :.. b :-> (t/Agent2 w r)])
cc/send (t/All [w r b :..] [(t/Agent2 w r) [r b :.. b :-> w] b :.. b :-> (t/Agent2 w r)])
cc/send-off (t/All [w r b :..] [(t/Agent2 w r) [r b :.. b :-> w] b :.. b :-> (t/Agent2 w r)])
cc/await [(t/Agent2 t/Nothing t/Any) :* :-> nil]
cc/await-for [t/AnyInteger (t/Agent2 t/Nothing t/Any) :* :-> t/Bool]
cc/await1 (t/All [w r] [(t/Agent2 w r) :-> (t/Agent2 w r)])
cc/release-pending-sends [:-> t/AnyInteger]
])

cc/add-watch (t/All [x [a :< (#?(:clj IRef :cljs t/Atom2) t/Nothing x)]]
                    (t/IFn 
                      ; this arity remembers the type of reference we pass to the function
                      [a t/Any [t/Any a x x :-> t/Any] :-> t/Any]
                      ; if the above cannot be inferred, 
                      [(#?(:clj IRef :cljs t/Atom2) t/Nothing x) t/Any [t/Any (#?(:clj IRef :cljs t/Atom2) t/Nothing x) x x :-> t/Any] :-> t/Any]))
cc/remove-watch [(#?(:clj IRef :cljs t/Atom2) t/Nothing t/Any) t/Any :-> t/Any]

#?@(:cljs [] :default [
cc/agent-error [(t/Agent2 t/Nothing t/Any) :-> (t/U nil Throwable)]
cc/restart-agent (t/All [w] [(t/Agent2 w t/Any) w & :optional {:clear-actions t/Any} :-> t/Any])
cc/set-error-handler! (t/All [w r] [(t/Agent2 w r) [(t/Agent2 w r) Throwable :-> t/Any] :-> t/Any])
cc/error-handler (t/All [w r] [(t/Agent2 w r) :-> (t/Nilable [(t/Agent2 w r) Throwable :-> t/Any])])
cc/set-error-mode! [(t/Agent2 t/Nothing t/Any) (t/U ':fail ':continue) :-> t/Any]
cc/error-mode [(t/Agent2 t/Nothing t/Any) :-> t/Any]
cc/agent-errors [(t/Agent2 t/Nothing t/Any) :-> (t/Nilable (t/ASeq Throwable))]
cc/clear-agent-errors [(t/Agent2 t/Nothing t/Any) :-> t/Any]
cc/shutdown-agents [:-> t/Any]
])

cc/take (t/All [x] (t/IFn [t/Int :-> (t/Transducer x x)]
                          [t/Int (t/Seqable x) :-> (t/ASeq x)]))
cc/drop (t/All [x] (t/IFn [t/Int :-> (t/Transducer x x)]
                          [t/Int (t/Seqable x) :-> (t/ASeq x)]))
cc/take-last (t/All [x] [t/Int (t/Seqable x) :-> (t/NilableNonEmptyASeq x)])
cc/drop-last (t/All [x] [(t/? t/Int) (t/Seqable x) :-> (t/ASeq x)])

cc/hash [t/Any :-> t/AnyInteger]
cc/hash-combine [t/AnyInteger t/Any :-> t/AnyInteger]

cc/ifn? (t/Pred #?(:clj clojure.lang.IFn
                   :cljs cljs.core/IFn))
cc/fn? (t/Pred t/Fn)

cc/instance? [#?(:clj Class :cljs js/Object) t/Any :-> t/Bool]
cc/cons (t/All [x] [x (t/Seqable x) :-> (t/ASeq x)])
cc/reverse (t/All [x] [(t/Seqable x) :-> (t/ASeq x)])
cc/rseq (t/All [x] [(t/Reversible x) :-> (t/NilableNonEmptyASeq x)])
cc/subseq  (t/All [x e] [(t/I (t/Seqable e) (t/Sorted x)) [t/Int t/Int :-> t/Bool] t/Int (t/cat [t/Int t/Int :-> t/Bool] t/Int) :? :-> (t/Nilable (t/ASeq e))])
cc/rsubseq (t/All [x e] [(t/I (t/Seqable e) (t/Sorted x)) [t/Int t/Int :-> t/Bool] t/Int (t/cat [t/Int t/Int :-> t/Bool] t/Int) :? :-> (t/Nilable (t/ASeq e))])

;coercions
#?@(:cljs [] :default [
cc/bigdec [(t/U t/Str t/Num) :-> BigDecimal]
cc/bigint [(t/U t/Str t/Num) :-> clojure.lang.BigInt]
cc/biginteger [(t/U t/Str t/Num) :-> java.math.BigInteger]
])
cc/boolean [t/Any :-> t/Bool]
cc/parse-boolean [t/Str :-> (t/Option t/Bool)]
cc/byte [(t/U Character t/Num) :-> Byte]
#?@(:cljs [
cc/char [(t/U t/Str t/Num) :-> t/Str]
] :default [
cc/char [(t/U Character t/Num) :-> Character]
])
cc/double [t/Num :-> #?(:cljs t/Num :default Double)]
cc/parse-double [s/Str :-> (t/Option #?(:cljs t/Num :default Double))]

cc/float [t/Num :-> #?(:cljs t/Num :default Float)]
cc/int [#?(:cljs t/Num :default (t/U Character t/Num)) :-> #?(:cljs t/AnyInteger :default Integer)]

cc/long [#?(:cljs t/Num :default (t/U Character t/Num)) :-> #?(:cljs t/AnyInteger :default Long)]
cc/parse-long [t/Str :-> (t/Option #?(:cljs t/AnyInteger :default Long))]
#?@(:cljs [] :default [
cc/num [t/Num :-> t/Num]
])
#?@(:cljs [
cc/short [t/Num :-> t/Num]
] :default [
cc/short [(t/U Character t/Num) :-> Short]
])

;array ctors
#?@(:cljs [] :default [
cc/boolean-array (t/IFn [(t/U t/Num (t/Seqable t/Bool)) :-> (Array boolean)]
                        [t/Num (t/U t/Bool (t/Seqable t/Bool)) :-> (Array boolean)])
cc/byte-array (t/IFn [(t/U t/Num (t/Seqable Byte)) :-> (Array byte)]
                     [t/Num (t/U Byte (t/Seqable Byte)) :-> (Array byte)])
cc/char-array (t/IFn [(t/U t/Num (t/Seqable Character)) :-> (Array char)]
                     [t/Num (t/U t/Num (t/Seqable Character)) :-> (Array char)])
cc/short-array (t/IFn [(t/U t/Num (t/Seqable Short)) :-> (Array short)]
                      [t/Num (t/U Short (t/Seqable Short)) :-> (Array short)])
cc/int-array (t/IFn [(t/U t/Num (t/Seqable t/Num)) :-> (Array int)]
                    [t/Num (t/U t/Num (t/Seqable t/Num)) :-> (Array int)])
cc/double-array (t/IFn [(t/U t/Num (t/Seqable t/Num)) :-> (Array double)]
                       [t/Num (t/U t/Num (t/Seqable t/Num)) :-> (Array double)])
])

;cast to java array
;; TODO rethink input and output types. eg.,
;;      cc/booleans [(ReadyOnlyArray boolean) :-> (t/U nil (Array boolean))]
;; TODO objects??
;;      cc/objects [(ReadyOnlyArray Object) :-> (t/U nil (ReadyOnlyArray Object))]
;;                                  
;; TODO propagate to Numbers/booleans etc
;cc/booleans [t/Any :-> (t/U nil (Array boolean))]
;cc/bytes [t/Any :-> (t/U nil (Array byte))]
;cc/chars [t/Any :-> (t/U nil (Array char))]
;cc/shorts [t/Any :-> (t/U nil (Array short))]
;cc/ints [t/Any :-> (t/U nil (Array int))]
;cc/longs [t/Any :-> (t/U nil (Array long))]
;cc/floats [t/Any :-> (t/U nil (Array float))]
;cc/doubles [t/Any :-> (t/U nil (Array double))]

cc/max-key (t/All [x] [[x :-> t/Num] (t/+ x) :-> x])
cc/min-key (t/All [x] [[x :-> t/Num] (t/+ x) :-> x])

cc/< [(t/+ t/Num) :-> t/Bool]
cc/<= [(t/+ t/Num) :-> t/Bool]
cc/> [(t/+ t/Num) :-> t/Bool]
cc/>= [(t/+ t/Num) :-> t/Bool]
cc/== [(t/+ t/Num) :-> t/Bool]

cc/max (t/IFn #?(:clj [(t/+ Long) :-> Long])
              #?(:clj [(t/+ Double) :-> Double])
              [(t/+ t/AnyInteger) :-> t/AnyInteger]
              [(t/+ t/Num) :-> t/Num])
cc/min (t/IFn #?(:clj [(t/+ Long) :-> Long])
              #?(:clj [(t/+ Double) :-> Double])
              [(t/+ t/AnyInteger) :-> t/AnyInteger]
              [(t/+ t/Num) :-> t/Num])

#?@(:cljs [] :default [
cc/ref (t/All [x] [x & :optional {:validator (t/U nil [x :-> t/Any]) :meta (t/U nil (t/Map t/Any t/Any))
                                  :min-history (t/U nil t/AnyInteger)
                                  :max-history (t/U nil t/AnyInteger)}
                   :-> (clojure.lang.Ref x x)])
])

cc/rand [(t/? t/Num) :-> t/Num]
cc/rand-int [t/Int :-> t/Int]
cc/ex-info (t/IFn [(t/Nilable t/Str) (t/Map t/Any t/Any) :-> t/ExInfo]
                  [(t/Nilable t/Str) (t/Map t/Any t/Any) (t/? #?(:clj (t/Nilable Throwable) :cljs t/Any)) :-> t/ExInfo])
cc/ex-data (t/IFn [t/ExInfo :-> (t/Map t/Any t/Any)]
                  [t/Any :-> (t/Nilable (t/Map t/Any t/Any))])
cc/ex-message [t/Any :-> (t/Nilable t/Str)]
cc/ex-cause [t/Any :-> #?(:clj (t/Nilable Throwable) :cljs t/Any)]


;; START CHUNK HACKS
;; These are hacks to get around the expansion of doseq>
;; Basically, inference isn't good enough to narrow a (t/Seqable x) to 
;; an (IChunk x), because chunked-seq? needs to be (t/Pred (IChunk t/Any)).
#?@(:cljs [] :default [
cc/chunked-seq? [t/Any :-> t/Any]
cc/chunk-first 
     (t/All [x]
          ;should be IChunkedSeq :-> IChunk
          [(t/Seqable x) :-> (clojure.lang.IChunk x)])
cc/chunk-rest
     (t/All [x]
          ;should be IChunkRest :-> t/Seq
          [(t/Seqable x) :-> (t/ASeq x)])
cc/chunk-buffer
     (t/All [x]
          [(t/U Integer Long) :-> (clojure.lang.ChunkBuffer x)])
cc/chunk
     (t/All [x]
          [(clojure.lang.ChunkBuffer x) :-> (clojure.lang.IChunk x)])
cc/chunk-cons
     (t/All [x]
          [(clojure.lang.IChunk x) (t/Seqable x) :-> (t/ASeq x)])
cc/chunk-append
     (t/All [x]
          [(clojure.lang.ChunkBuffer x) x :-> t/Any])
;;END CHUNK HACKS
])


cc/subvec (t/All [x] [(t/Vec x) t/AnyInteger (t/? t/AnyInteger) :-> (t/Vec x)])

#?@(:cljs [] :default [
cc/alias [t/Sym t/Sym :-> nil]
cc/all-ns [:-> (t/NilableNonEmptyASeq t/Namespace)]
])

#?@(:cljs [] :default [
cc/*file* t/Str
])
cc/*command-line-args* (t/NilableNonEmptyASeq t/Str)
#?@(:cljs [
cc/*unchecked-if* t/Bool
cc/*unchecked-arrays* t/Bool
cc/*warn-on-infer* t/Bool
cc/enable-console-print! [:-> t/Any]
] :default [
cc/*warn-on-reflection* t/Bool
cc/*compile-path* t/Str
cc/*compile-files* t/Bool
cc/*unchecked-math* t/Bool
cc/*compiler-options* (t/Map t/Any t/Any)
cc/*in* java.io.Reader
cc/*out* java.io.Writer ;; FIXME cljs
cc/*err* java.io.Writer
cc/*repl* t/Any
])
cc/*flush-on-newline* t/Bool
cc/*print-meta* t/Bool
cc/*print-dup* t/Bool
cc/*print-readably* t/Bool
#?@(:cljs [] :default [
cc/*read-eval* (t/U ':unknown t/Bool)
])

cc/trampoline (t/All [r b :..] [[b :.. b :-> (t/Rec [f] (t/U r [:-> (t/U f r)]))]
                                b :.. b :-> r])


;; math.numeric-tower

#?@(:cljs [] :default [
clojure.math.numeric-tower/floor (t/IFn [t/AnyInteger :-> t/AnyInteger]
                                        [t/Num :-> t/Num])
clojure.math.numeric-tower/abs (t/IFn [t/AnyInteger :-> t/AnyInteger]
                                      [t/Num :-> t/Num])
])

;; core.match

#?@(:cljs [] :default [
clojure.core.match/backtrack Exception
])

cc/eval [t/Any :-> t/Any]
cc/rand-nth (t/All [x] [(t/U (t/Indexed x) (t/SequentialSeqable x)) :-> x])

#?@(:cljs [
;TODO
;cljs.pprint/cl-format [(t/U cljs.core/IWriter nil t/Bool) t/Str t/Any :* :-> (t/U nil t/Str)]
;cljs.pprint/fresh-line [:-> t/Any]
;cljs.pprint/get-pretty-writer [cljs.core/IWriter :-> cljs.core/IWriter]
;clojure.pprint/pprint (t/IFn [t/Any :-> nil]
;                             [t/Any java.io.Writer :-> nil])
] :default [
clojure.pprint/cl-format [(t/U java.io.Writer nil t/Bool) t/Str t/Any :* :-> (t/Nilable t/Str)]
clojure.pprint/fresh-line [:-> t/Any]
clojure.pprint/get-pretty-writer [java.io.Writer :-> java.io.Writer]
clojure.pprint/pprint [t/Any (t/? java.io.Writer) :-> nil]
])

#?@(:cljs [] :default [
clojure.repl/pst (t/IFn [:-> nil]
                        [(t/U t/Int Throwable) :-> nil]
                        [Throwable t/Int :-> nil])
clojure.repl/print-doc [t/Sym :-> t/Any]
clojure.repl/find-doc [(t/U t/Str t/Regex) :-> t/Any]
clojure.repl/source-fn [t/Any :-> (t/U nil t/Str)]
clojure.java.javadoc/javadoc [Object :-> t/Any]
complete.core/completions
[t/Any (t/? t/Any) :-> t/Any]
])
)
