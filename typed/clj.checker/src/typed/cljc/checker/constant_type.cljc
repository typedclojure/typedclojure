;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.constant-type
  (:require [typed.cljc.checker.type-rep :as r :refer [ret]]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clojure :as t]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.hset-utils :as hset]
            [typed.clj.runtime.hmap-utils :as hmap-utils]
            [typed.cljc.checker.proposition-ops :as fo])
  (:import (clojure.lang IPersistentList IPersistentVector Symbol Cons ISeq
                         IFn IPersistentStack IPersistentSet IPersistentMap IMapEntry
                         Keyword Atom PersistentList IMeta PersistentArrayMap Compiler Named
                         IRef ARef IDeref IReference Sorted
                         LazySeq Indexed)))

(defprotocol ConstantType 
  (constant-ret [this opts]))

(defn- Double->Type [^Double v opts]
  (ret
     (if (Double/isNaN v)
       ;; since different instances of ##NaN are not equal it's not suitable as a singleton type
       (c/RClass-of Double opts)
       (r/-val v))))

(defn constant-type
  ([s opts] (constant-type s false opts))
  ([s quoted? opts]
   (let [quoted? (or quoted? (::quoted? opts))]
     (if (and (not quoted?)
              (seq? s)
              (= 'quote (first s))
              (= 2 (count s)))
       (r/ret-t (constant-ret (second s)
                              (assoc opts ::quoted? true)))
       #?(:cljr
          ;; CLR can't extend protocols to value types except long/double
          ;; so we handle them explicitly here
          (r/ret-t
            (cond
              (instance? System.Int64 s) (ret (r/-val s))
              (instance? System.Int32 s) (ret (r/-val s))
              (instance? System.Double s) (Double->Type s opts)
              (instance? System.Boolean s) (ret (r/-val s))
              (instance? System.Char s) (ret (r/-val s))
              (instance? System.Decimal s) (ret (r/-val s))
              :else (constant-ret s opts)))
          :default
          (r/ret-t (constant-ret s opts)))))))

;[Any -> Type]

(defmacro constant-type->val
  [& cls]
  (let [method `(constant-ret [v# opts#] (ret (r/-val v#)))]
    `(extend-protocol ConstantType
       ~@(apply concat (zipmap cls (repeat method))))))

(constant-type->val
  #?(:cljr System.Type :default Class) Symbol #?@(:cljr [] :default [Long Integer java.math.BigDecimal Boolean Character])
  clojure.lang.BigInt String clojure.lang.Keyword
  clojure.lang.Namespace)

(extend-protocol ConstantType
  nil
  (constant-ret [v opts]
    (impl/impl-case opts
      :clojure (ret (r/-val nil))
      :cljs (ret (r/JSNull-maker))))

  Double
  (constant-ret [v opts] (Double->Type v opts))

  #?(:cljr System.Text.RegularExpressions.Regex
     :default java.util.regex.Pattern)
  (constant-ret [v opts]
    (impl/impl-case opts
      :clojure (ret (c/RClass-of #?(:cljr System.Text.RegularExpressions.Regex
                                     :default java.util.regex.Pattern) opts))
      :cljs (assert nil "TODO: CLJS pattern in ConstantType")))

  IPersistentSet
  (constant-ret [v opts]
    (ret
      (if (every? hset/valid-fixed? v)
        (r/-hset (r/sorted-type-set (mapv r/-val v)))
        (c/-name `t/Set (c/Un (mapv #(constant-type % opts) v) opts)))))

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
    (let [kts (mapv #(constant-type % opts) (keys cmap))
          vts (mapv #(constant-type % opts) (vals cmap))
          kts' (mapv #(c/coerce-to-valid-hmap-key %) kts)]
      (if (not-any? nil? kts')
        (ret (c/-complete-hmap (zipmap kts' vts) opts))
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
