;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.constant-type
  (:require [typed.cljc.checker.type-rep :as r :refer [ret]]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clojure :as t]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.hset-utils :as hset]
            [typed.cljc.checker.filter-ops :as fo])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons ISeq
                         IFn IPersistentStack IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef ARef IDeref IReference Sorted
                         LazySeq Indexed)))

(defprotocol ConstantType 
  (constant-ret [this opts]))

(def ^:dynamic *quoted?* false)

(defn constant-type
  ([s opts] (constant-type s false opts))
  ([s quoted? opts]
   (binding [*quoted?* (or quoted? *quoted?*)]
     (if (and (not *quoted?*)
              (seq? s)
              (= 'quote (first s))
              (= 2 (count s)))
       (binding [*quoted?* true]
         (r/ret-t (constant-ret (second s) opts)))
       (r/ret-t (constant-ret s opts))))))

;[Any -> Type]

(defmacro constant-type->val
  [& cls]
  (let [method `(constant-ret [v# opts#] (ret (r/-val v#)))]
    `(extend-protocol ConstantType
       ~@(apply concat (zipmap cls (repeat method))))))

(constant-type->val
  Class Symbol Long Double Integer java.math.BigDecimal
  clojure.lang.BigInt String Character clojure.lang.Keyword
  Boolean clojure.lang.Namespace)

(extend-protocol ConstantType
  nil
  (constant-ret [v opts]
    (impl/impl-case opts
      :clojure (ret (r/-val nil))
      :cljs (ret (r/JSNull-maker))))

  java.util.regex.Pattern
  (constant-ret [v opts]
    (impl/impl-case opts
      :clojure (ret (c/RClass-of java.util.regex.Pattern opts))
      :cljs (assert nil "TODO: CLJS pattern in ConstantType")))

  IPersistentSet
  (constant-ret [v opts]
    (ret
      (if (every? hset/valid-fixed? v)
        (r/-hset (r/sorted-type-set (map r/-val v)))
        (c/-name `t/Set (c/Un (map #(constant-type % opts) v) opts)))))

  ;default for ISeqs
  ISeq
  (constant-ret [iseq opts]
    (ret (r/-hsequential
           (mapv #(constant-type % opts) iseq)
           {:kind (cond
                    (list? iseq) :list
                    (seq? iseq) :seq
                    :else :sequential)}
           opts)))

  IPersistentVector
  (constant-ret [cvec opts] (ret (r/-hvec (mapv #(constant-type % opts) cvec) {} opts)))

  IPersistentMap
  (constant-ret [cmap opts]
    (let [kts (map #(constant-type % opts) (keys cmap))
          vts (map #(constant-type % opts) (vals cmap))]
      (if (every? r/Value? kts)
        (ret (c/-complete-hmap (zipmap kts vts) opts))
        (ret (c/In
               [(c/-name `t/Map
                         (c/Un kts opts)
                         (c/Un vts opts))
                (r/make-ExactCountRange (count cmap))]
               opts)))))
  
  ;base case
  Object
  (constant-ret [bse opts]
    (impl/impl-case opts
      :clojure (ret (c/RClass-of-with-unknown-params (class bse) opts))
      :cljs (cond
              (number? bse) (ret (r/JSNumber-maker))
              :else (assert nil "TODO: base case of constant-type")))))
