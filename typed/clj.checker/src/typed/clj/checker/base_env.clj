;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.base-env
  (:require [typed.clojure :as t]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.runtime.env-utils :as env-utils]
            [typed.clj.checker.ctor-override-env :as ctor-override-env]
            [typed.clj.checker.field-override-env :as field-override-env]
            [typed.clj.checker.method-override-env :as method-override-env]
            [typed.clj.checker.method-param-nilables :as method-param-nilables]
            [typed.clj.checker.method-return-nilables :as method-return-nilables]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.base-env-common :refer [delay-and-cache-env] :as common]
            [typed.cljc.checker.base-env-helper :as h]
            [typed.cljc.checker.datatype-ancestor-env :as datatype-ancestor-env]
            [typed.cljc.checker.datatype-env :as datatype-env]
            [typed.cljc.checker.declared-kind-env :as declared-kind-env]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.path-rep :as pe]
            [typed.cljc.checker.protocol-env :as protocol-env]
            [typed.cljc.checker.subst]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.var-env :as var-env])
  (:import (clojure.lang LazySeq PersistentHashSet PersistentTreeSet PersistentList
                         ILookup Indexed #_ITransientSet
                         IRef Reduced)))

(defn- aset-*-type [t]
  (env-utils/delay-type
    (impl/with-clojure-impl
      (let [arr-t (prs/delay-parse-type `(~'Array ~t))
            rtn-type (prs/delay-parse-type t)
            num-t (prs/delay-parse-type `t/Num)]
        (apply (resolve `r/make-FnIntersection)
               (map (resolve `r/make-Function)
                    (loop [num 1
                           result []
                           dom [arr-t num-t]]
                      (if (> num 10)
                        result
                        (recur (inc num)
                               (conj result (conj dom rtn-type))
                               (conj dom num-t))))
                    (repeat rtn-type)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type annotations

(defn ^:private count-type []
  (env-utils/delay-type
    (impl/with-clojure-impl
      ((resolve `r/make-FnIntersection)
        ((resolve `r/make-Function)
          [(prs/delay-parse-type `(t/U (t/Seqable t/Any) clojure.lang.Counted))]
          (prs/delay-parse-type `(t/U java.lang.Integer java.lang.Long))
          :object ((resolve `obj/-path) [((resolve `pe/CountPE-maker))] 0))))))

(defn ^:private nth-type []
  (env-utils/delay-type
    (impl/with-clojure-impl
      (prs/delay-parse-type
        ;;TODO port this type from clojure.lang.RT/nthFrom properly. Try not to use Indexed as a fake ancestor.
        ;; maybe even remove Seqable fake ancestors and move to t/Seqable.
        `(t/All [~'x ~'y]
                (t/IFn 
                  [(t/U (Indexed ~'x) (t/SequentialSeqable ~'x)) t/AnyInteger :-> ~'x]
                  [(t/U (Indexed ~'x) (t/SequentialSeqable ~'x) nil) t/AnyInteger ~'y :-> (t/U ~'x ~'y)]
                  [(t/U (Indexed ~'x) (t/SequentialSeqable ~'x) nil) t/AnyInteger :-> (t/U ~'x nil)]))))))

;; public -- used in type-ctors via requiring-resolve
(defn get-type []
  (env-utils/delay-type
    (impl/with-clojure-impl
      (prs/delay-parse-type
        (let [x 'x
              y 'y]
          `(t/All [~x ~y]
                  (t/IFn 
                    ;no default
                    [(t/Option (ILookup t/Any ~x)) t/Any :-> (t/Option ~x)]
                    [nil t/Any :-> nil]
                    [(t/Option java.util.Map) t/Any :-> (t/Option t/Any)]
                    [(t/Option (t/Set ~x)) t/Any :-> (t/Option ~x)]
                    [(t/Option String) t/Any :-> (t/Option Character)]
                    [(t/Option (~'ReadOnlyArray ~x)) t/Any :-> (t/Option ~x)]
                    ;;[(t/Option (ITransientSet ~x)) t/Any :-> (t/Option ~x)] ;;TODO transients nyi
                    [t/Any t/Any :-> t/Any]
                    ;default
                    [(t/Option (ILookup t/Any ~x)) t/Any ~y :-> (t/U ~x ~y)]
                    [nil t/Any ~y :-> ~y]
                    [(t/Option java.util.Map) t/Any t/Any :-> t/Any]
                    [(t/Option (t/Set ~x)) t/Any ~y :-> (t/U ~x ~y)]
                    [(t/Option String) t/Any ~y :-> (t/U Character ~y)]
                    [(t/Option (~'ReadOnlyArray ~x)) t/Any ~y :-> (t/U ~x ~y)]
                    ;;[(t/Option (ITransientSet ~x)) t/Any ~y :-> (t/U ~x ~y)] ;;TODO transients nyi
                    [t/Any t/Any t/Any :-> t/Any])))))))

(defn ^:private reduced?-type []
  (env-utils/delay-type
    (impl/with-clojure-impl
      (prs/delay-parse-type
        `(t/Pred (Reduced t/Any))))))

(defn ^:private zero?-type []
  (env-utils/delay-type
    (impl/with-clojure-impl
      (prs/delay-parse-type
        `[t/Num :-> t/Bool
          :filters {:then (~'is (t/Value 0) 0)
                    :else (~'!  (t/Value 0) 0)}]))))

(defn ^:private compare-type []
  (env-utils/delay-type
    (impl/with-clojure-impl
      (prs/delay-parse-type
        `[t/Any t/Any :-> t/Num]))))

(delay-and-cache-env ^:private init-var-env
  ;(reset-alias-env!)
  {'clojure.core/count (count-type)
   'clojure.core/aset-boolean (aset-*-type 'boolean)
   'clojure.core/aset-byte (aset-*-type 'byte)
   'clojure.core/aset-char (aset-*-type 'char)
   'clojure.core/aset-short (aset-*-type 'short)
   'clojure.core/aset-int (aset-*-type 'int)
   'clojure.core/aset-long (aset-*-type 'long)
   'clojure.core/aset-float (aset-*-type 'float)
   'clojure.core/aset-double (aset-*-type 'double)
   'clojure.core/nth (nth-type)
   'clojure.core/get (get-type)
   'clojure.core/reduced? (reduced?-type)
   'clojure.core/zero? (zero?-type)
   'clojure.core/compare (compare-type)
   })


;(comment
;  (aget my-array 0 1 2)
;  (aget (aget my-array 0) 1 2)
;  (aget (aget (aget my-array 0) 1) 2)
;
;  (App [(Associative a b) c d -> (Associative (t/U a c) (t/U b d))]
;       (App [(Associative a b) c d -> (Associative (t/U a c) (t/U b d))]
;            (App [(Associative a b) c d -> (Associative (t/U a c) (t/U b d))]
;                 (Associative t/Keyword t/Num)
;                 :a 1)
;            :b 2)
;       :c 3)
;
;  (assoc my-map :a 1 :b 2 :c 3)
;  (assoc (assoc my-map :a 1) :b 2 :c 3)
;  (assoc (assoc (assoc my-map :a 1) :b 2) :c 3)
;
;  clojure.core/aset
;       (Label [rec]
;              (t/All [w [v :< w] :dotted [b]]
;                   [(Array w _) t/AnyInteger v -> v]
;                   [(Array _ r) t/AnyInteger b ... b
;                    :recur (rec r b ... b)]))
;
;  clojure.core/aget 
;       (Label [rec]
;              (t/All [x :dotted [b]] 
;                   (t/IFn [(Array _ x) t/AnyInteger -> x]
;                       [(Array _ x) t/AnyInteger b ... b
;                        :recur 
;                        (rec x b ... b)])))
;
;  clojure.core/assoc 
;       (t/All [[h <: (t/Map t/Any t/Any)]
;             a b e ...2]
;         [h k ...2 a b -> (t/Assoc h k ...2 a b)])
;
;       (Label [rec]
;              (t/All [[h :< (HMap {})] x y [k :< (t/I AnyValue t/Keyword)] [e :< k] :dotted [b]]
;                   [h k v -> (t/I h (HMap k v))]
;                   [(Associative y x) y x -> (Associative y x)]
;                   [h k v b ... b
;                    :recur (rec (t/I h (HMap {k v})) b ... b)]
;                   [(Associative y x) y x b ... b
;                    :recur (rec (Associative y x) b ... b)]
;                   ))
;
;  clojure.core/dissoc
;       (Label [rec]
;              (t/All [[m :< (Associative _ _)] :dotted [b]]
;                   [nil t/Any * -> nil]
;                   [m -> m]
;                   [m k b ... b
;                    :recur
;                    (rec (t/I m (HMap {} :without [k])) b ... b)]))
;
;  (update-in {:a {:b 1}} [:a :b] inc)
;  (update-in 
;    (update-in {:a {:b 1}} [:a] inc) 
;    [:b] 
;    inc)
;
;  clojure.core/update-in
;       (FixedPoint
;         (t/All [[x :< (t/U nil (Associative t/Any t/Any))] k [l :< k] v r e
;               :dotted [a b]]
;              (t/IFn [(HMap {l v}) (t/HSequential [k]) [v a ... a -> r] a ... a -> (t/I x (HMap {l r}))]
;                  [(HMap {l r}) (t/HSequential [k b ... b]) [v a ... a -> e] a ... a
;                   :recur
;                   [r (t/HSequential [b ... b]) [v a ... a -> e] a ... a]])))
;
;  ;clojure.core/get-in 
;  ;     (Label [rec]
;  ;       (t/All [[x :< (t/U nil (Associative t/Any t/Any))] k :dotted [b]]
;  ;            (t/IFn [x (t/HSequential []) -> x]
;  ;                [x (t/HSequential []) _ -> x]
;  ;                [(t/U nil (Associative _ y) (t/HSequential [k b ... b]) a -> x
;  ;                ;TODO
;  ;                [(t/U nil (Associative t/Any y)) (t/HSequential [k]) -> (t/U nil x)]
;  ;                    ))))
;
;  clojure.core/partial 
;       (Label [rec]
;              (t/All [x [a :< x] r :dotted [b c]]
;                   (t/IFn [[x c ... c -> r] a -> [c ... c -> r]]
;                       [[x c ... c -> r] a b ... b
;                        :recur
;                        (rec [c ... c -> r] b ... b)])))
;
;  ;                                [[y -> x] [b ... b -> y] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [b ... b -> z] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [b ... b -> k] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [b ... b -> l] -> [b ... b -> x]]
;  ;                                [[y -> x] [z -> y] [k -> z] [l -> k] [m -> l] [b ... b -> m] -> [b ... b -> x]]
;
;  clojure.core/juxt
;                  (t/All [y b ... c ...]
;                       [[b ... b -> y] [b ... b -> c] ... c -> [b ... b -> (DottedVec y c ... c)]])
;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nocheck env

(delay-and-cache-env ^:private init-var-nochecks
  (set (keys (init-var-env))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method param annotations

(delay-and-cache-env ^:private init-method-nilable-param-env {})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method return annotations

(delay-and-cache-env ^:private init-method-nonnilable-return-env
  (h/method-nonnilable-return-mappings

java.lang.Object/getClass #{0}
clojure.lang.Compiler/munge :all
java.lang.Class/getName :all
java.lang.Class/forName :all

java.lang.Object/toString :all
java.lang.String/toUpperCase :all
java.lang.String/intern :all
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method override annotations

(delay-and-cache-env ^:private init-method-override-env
  ;(reset-alias-env!)
  (merge
    {'clojure.lang.RT/nth (nth-type)
     'clojure.lang.RT/isReduced (reduced?-type)
     'clojure.lang.RT/isZero (zero?-type)
     'clojure.lang.Util/compare (compare-type)
     }
    (h/method-override-mappings

clojure.lang.Indexed/nth
  (t/All [x y]
       (t/IFn [(Indexed x) t/AnyInteger -> x]
              [(Indexed x) t/AnyInteger y -> (t/U x y)]))


;what about combinations of references and primitives?
clojure.lang.RT/box
(t/All [x]
     (t/IFn [Character -> Character]
          [Integer -> Integer]
          [Short -> Short]
          [t/Bool -> t/Bool]
          [Byte -> Byte]
          [Long -> Long]
          [Float -> Float]
          [Double -> Double]
          [(t/U Byte Short Integer Long) -> t/AnyInteger]
          [(t/U Float Double) -> t/Num]
          [nil -> nil]
          [x -> x]))

clojure.lang.RT/booleanCast [t/Any -> t/Bool]

clojure.lang.Numbers/char_array (t/IFn [(t/U t/Num (t/Seqable Character)) -> (Array char)]
                                       [t/Num (t/U t/Num (t/Seqable Character)) -> (Array char)])


clojure.lang.LockingTransaction/runInTransaction
                 (t/All [x]
                   [[-> x] -> x])

;array ops
clojure.lang.RT/alength [(ReadOnlyArray t/Any) -> Integer]

clojure.lang.RT/aget (t/All [o]
                        [(ReadOnlyArray o) Integer -> o])

clojure.lang.RT/aset (t/All [i o]
                          [(Array2 i o) t/AnyInteger i -> o])

;numbers
clojure.lang.Numbers/add (t/IFn [Long Long -> Long]
                                [Double Double -> Double]
                                [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                [t/Num t/Num -> t/Num])
clojure.lang.Numbers/inc (t/IFn [Long -> Long]
                                [Double -> Double]
                                [t/AnyInteger -> t/AnyInteger]
                                [t/Num -> t/Num])
clojure.lang.Numbers/dec (t/IFn [Long -> Long]
                              [Double -> Double]
                              [t/AnyInteger -> t/AnyInteger]
                              [t/Num -> t/Num])
clojure.lang.Numbers/quotient (t/IFn [Long Long -> Long]
                                   [(t/U Long Double) (t/U Long Double) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [t/Num t/Num -> t/Num])
clojure.lang.Numbers/incP (t/IFn [Long -> (t/U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [t/AnyInteger -> t/AnyInteger]
                               [t/Num -> t/Num])
clojure.lang.Numbers/decP (t/IFn [Long -> (t/U clojure.lang.BigInt Long)]
                               [Double -> Double]
                               [t/AnyInteger -> t/AnyInteger]
                               [t/Num -> t/Num])
clojure.lang.Numbers/unchecked_inc (t/IFn [Long -> Long]
                                        [Double -> Double]
                                        [t/AnyInteger -> t/AnyInteger]
                                        [t/Num -> t/Num])
clojure.lang.Numbers/unchecked_dec (t/IFn [Long -> Long]
                                        [Double -> Double]
                                        [t/AnyInteger -> t/AnyInteger]
                                        [t/Num -> t/Num])
clojure.lang.Numbers/unchecked_int_inc [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_dec [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_negate [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_subtract [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_add [t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_minus (t/IFn 
                                       ; negate
                                       [Long -> Long]
                                       [Double -> Double]
                                       [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                       [t/Num t/Num -> t/Num]
                                       ; subtract
                                       [Long Long -> Long]
                                       [(t/U Long Double) (t/U Long Double) -> Double]
                                       [t/AnyInteger -> t/AnyInteger]
                                       [t/Num -> t/Num])
clojure.lang.Numbers/minus (t/IFn
                             ; negate
                             [Long -> Long]
                             [Double -> Double]
                             [t/AnyInteger -> t/AnyInteger]
                             [t/Num -> t/Num]
                             ;minus
                             [Long Long -> Long]
                             [(t/U Double Long) (t/U Double Long) -> Long]
                             [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                             [t/Num t/Num -> t/Num])
clojure.lang.Numbers/unchecked_multiply (t/IFn [Long Long -> Long]
                                             [(t/U Long Double) (t/U Long Double) -> Double]
                                             [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                             [t/Num t/Num -> t/Num])
clojure.lang.Numbers/unchecked_int_multiply [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_divide [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/unchecked_int_remainder [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/remainder [t/Num t/Num -> t/AnyInteger]
clojure.lang.Numbers/multiply (t/IFn [Long Long -> Long]
                                   [(t/U Double Long) (t/U Double Long) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [t/Num t/Num -> t/Num])
clojure.lang.Numbers/divide (t/IFn [Long Long -> Long]
                                   [(t/U Double Long) (t/U Double Long) -> Double]
                                   [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                                   [t/Num t/Num -> t/Num])
      ;bit-not
clojure.lang.Numbers/not [t/AnyInteger -> Long]
;bit-and
clojure.lang.Numbers/and [t/AnyInteger t/AnyInteger -> Long]
;bit-or
clojure.lang.Numbers/or [t/AnyInteger t/AnyInteger -> Long]
;bit-xor
clojure.lang.Numbers/xor [t/AnyInteger t/AnyInteger -> Long]
;bit-and-not
clojure.lang.Numbers/andNot [t/AnyInteger t/AnyInteger -> Long]
; unsigned-bit-shift-right 
clojure.lang.Numbers/unsignedShiftRight [t/AnyInteger t/AnyInteger -> Long]

clojure.lang.Numbers/max (t/IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                           [t/Num t/Num -> t/Num])
clojure.lang.Numbers/min (t/IFn 
                           [Long Long -> Long]
                           [Double Double -> Double]
                           [t/AnyInteger t/AnyInteger -> t/AnyInteger]
                           [t/Num t/Num -> t/Num])

clojure.lang.Numbers/lt [t/Num t/Num -> t/Bool]
clojure.lang.Numbers/lte [t/Num t/Num -> t/Bool]
clojure.lang.Numbers/gt [t/Num t/Num -> t/Bool]
clojure.lang.Numbers/gte [t/Num t/Num -> t/Bool]

clojure.lang.Numbers/isNeg [t/Num -> t/Bool]
clojure.lang.Numbers/isPos [t/Num -> t/Bool]

; this is overloaded in interesting ways, but this is good enough for destructuring purposes
clojure.lang.PersistentHashMap/create [(t/U nil (t/Seq t/Any) java.util.Map (ReadOnlyArray Object)) -> (t/Map t/Any t/Any)]

clojure.lang.RT/floatCast  [t/Num -> Float]
clojure.lang.RT/byteCast   [(t/U Character t/Num) -> Byte]
clojure.lang.RT/charCast   [(t/U Character t/Num) -> Character]
clojure.lang.RT/doubleCast [t/Num -> Double]
clojure.lang.RT/intCast    [(t/U Character t/Num) -> Integer]
clojure.lang.RT/longCast   [(t/U Character t/Num) -> Long]
clojure.lang.RT/shortCast  [(t/U Character t/Num) -> Short]

clojure.lang.RT/uncheckedFloatCast  [t/Num -> Float]
clojure.lang.RT/uncheckedByteCast   [(t/U Character t/Num) -> Byte]
clojure.lang.RT/uncheckedCharCast   [(t/U Character t/Num) -> Character]
clojure.lang.RT/uncheckedIntCast    [(t/U Character t/Num) -> Integer]
clojure.lang.RT/uncheckedLongCast   [(t/U Character t/Num) -> Long]
clojure.lang.RT/uncheckedShortCast  [(t/U Character t/Num) -> Short]

clojure.lang.Numbers/num   [t/Num -> t/Num]
    )
    {'clojure.lang.RT/count (count-type)
     'clojure.lang.RT/get (get-type)
     }))

(comment
  clojure.lang.IFn/invoke (t/All [r a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 a20 arest]
                               (t/IFn
                                 [[-> r] -> r]
                                 [[a0 -> r] a0 -> r]
                                 [[a0 a1 -> r] a0 a1 -> r]
                                 [[a0 a1 a2 -> r] a0 a1 a2 -> r]
                                 [[a0 a1 a2 a3 -> r] a0 a1 a2 a3 -> r]
                                 [[a0 a1 a2 a3 a4 -> r] a0 a1 a2 a3 a4 -> r]
                                 [[a0 a1 a2 a3 a4 a5 -> r] a0 a1 a2 a3 a4 a5 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 -> r] a0 a1 a2 a3 a4 a5 a6 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 -> r] a0 a1 a2 a3 a4 a5 a6 a7 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 -> r]
                                 [[a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 arest * -> r] a0 a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15 a16 a17 a18 a19 (t/Seqable arest) -> r]
                                 )))

(delay-and-cache-env ^:private init-field-override-env
  (h/field-override-mappings
clojure.lang.PersistentArrayMap/EMPTY (t/HMap :complete? true)
))

(delay-and-cache-env ^:private init-ctor-override-env
  ;(reset-alias-env!)
  (h/ctor-override-mappings

clojure.lang.LazySeq (t/All [x] [[-> (t/Seqable x)] -> (LazySeq x)])
clojure.lang.Delay (t/All [x] [[-> x] -> (clojure.lang.Delay x)])
java.lang.ref.SoftReference (t/All [x] [x -> (java.lang.ref.SoftReference x)])
    ))

;; not added in refresh
(delay-and-cache-env ^:private init-declared-kinds {})

;; not added in refresh
(delay-and-cache-env ^:private init-datatype-env {})

;; not added in refresh
(delay-and-cache-env ^:private init-datatype-ancestor-env {})

(defn reset-clojure-envs! []
  (impl/with-clojure-impl
    ;(reset-alias-env!)
    (var-env/reset-var-type-env! (init-var-env) (init-var-nochecks))
    (method-return-nilables/reset-nonnilable-method-return-env! (init-method-nonnilable-return-env))
    (method-param-nilables/reset-method-nilable-param-env! (init-method-nilable-param-env))
    (method-override-env/reset-method-override-env! (init-method-override-env))
    (field-override-env/reset-field-override-env! (init-field-override-env))
    (ctor-override-env/reset-constructor-override-env! (init-ctor-override-env))
    ;(protocol-env/reset-protocol-env! (init-protocol-env))
    (declared-kind-env/reset-declared-kinds! (init-declared-kinds))
    (datatype-env/reset-datatype-env! (init-datatype-env))
    (datatype-ancestor-env/reset-datatype-ancestors! (init-datatype-ancestor-env)))
  nil)

(defn refresh-core-clojure-envs! []
  (impl/with-clojure-impl
    ;(refresh-core-alias-env!)
    ;(protocol-env/merge-protocol-env! (init-protocol-env))
    (var-env/refresh-var-type-env! (init-var-env) (init-var-nochecks))
    (method-param-nilables/merge-method-nilable-param-env! (init-method-nilable-param-env))
    (method-return-nilables/merge-nonnilable-method-return-env! (init-method-nonnilable-return-env))
    (method-override-env/merge-method-override-env! (init-method-override-env))
    (field-override-env/merge-field-override-env! (init-field-override-env))
    (ctor-override-env/merge-constructor-override-env! (init-ctor-override-env)))
  nil)
