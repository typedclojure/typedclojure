;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.map
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.type-ctors :as c])
  (:import (clojure.lang APersistentMap)))

;(ann expected-vals [(Coll Type) (Nilable TCResult) Opts -> (Coll (Nilable TCResult))])
(defn expected-vals
  "Returns a sequence of (Nilable TCResults) to use as expected types for type
  checking the values of a literal map expression"
  [key-types expected opts]
  {:pre [(every? r/Type? key-types)
         ((some-fn r/TCResult? nil?) expected)]
   :post [(every? (some-fn nil? r/TCResult?) %)
          (= (count %)
             (count key-types))]}
  (let [expected (when expected 
                   (c/fully-resolve-type (r/ret-t expected) opts))
        flat-expecteds (when expected
                         (mapv #(c/fully-resolve-type % opts)
                               (if (r/Union? expected)
                                 (:types expected)
                                 [expected])))
        no-expecteds (repeat (count key-types) nil)]
    (cond 
      ; If every key in this map expression is a Value, let's try and
      ; make use of our expected type for each individual entry. This is
      ; most useful for `extend`, where we have HMaps of functions, and
      ; we want to forward the expected types to each val, a function.
      ;
      ; The expected type also needs to be an expected shape:
      ; - a HMap or union of just HMaps
      ; - each key must have exactly one possible type if it is
      ;   present
      ; - if a key is absent in a HMap type, it must be explicitly
      ;   absent
      (and expected 
           (every? r/Value? key-types)
           (every? r/HeterogeneousMap? flat-expecteds))
        (let [hmaps flat-expecteds]
          (reduce (fn [val-expecteds ktype]
                    ; also includes this contract wrapped in a Reduced
                    ; The post-condition for `expected-vals` catches this anyway
                    ;{:post [(every? (some-fn nil? r/TCResult?) %)]}

                    (let [; find the ktype key in each hmap.
                          ; - If ktype is present in mandatory or optional then we either use the entry's val type 
                          ; - otherwise if ktype is explicitly forbidden via :absent-keys or completeness
                          ;   we skip the entry.
                          ; - otherwise we give up and don't check this as a hmap, return nil
                          ;   that gets propagated up
                          corresponding-vals 
                          (reduce (fn [corresponding-vals {:keys [types absent-keys optional] :as hmap}]
                                    (if-let [v (some #(get % ktype) [types optional])]
                                      (conj corresponding-vals v)
                                      (cond
                                        (or (contains? absent-keys ktype)
                                            (c/complete-hmap? hmap))
                                          corresponding-vals
                                        :else 
                                          (reduced nil))))
                                  #{} hmaps)
                          val-expect (when (= 1 (count corresponding-vals))
                                       (r/ret (first corresponding-vals)))]
                      (if val-expect
                        (conj val-expecteds val-expect)
                        (reduced no-expecteds))))
                  [] key-types))

      ; If we expect an (IPersistentMap k v), just use the v as the expected val types
      (and (r/RClass? expected)
           (= (:the-class expected) 'clojure.lang.IPersistentMap))
        (let [{[_ vt] :poly?} expected]
          (map r/ret (repeat (count key-types) vt)))
      ; otherwise we don't give expected types
      :else no-expecteds)))

(defn check-map [{keyexprs :keys valexprs :vals :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:keys %))
          (vector? (:vals %))]}
  (let [ckeyexprs (mapv check-expr keyexprs)
        key-types (map (comp r/ret-t u/expr-type) ckeyexprs)

        val-rets
        (expected-vals key-types expected opts)

        cvalexprs (mapv check-expr valexprs val-rets)
        val-types (map (comp r/ret-t u/expr-type) cvalexprs)

        ts (zipmap key-types val-types)
        actual-t (if (every? c/keyword-value? (keys ts))
                   (c/-complete-hmap ts opts)
                   (c/In
                     [(impl/impl-case
                        :clojure (c/RClass-of APersistentMap [(c/Un (keys ts) opts)
                                                              (c/Un (vals ts) opts)]
                                              opts)
                        :cljs (c/-name 'typed.clojure/Map
                                       (c/Un (keys ts) opts)
                                       (c/Un (vals ts) opts)))
                      (r/make-ExactCountRange (count keyexprs))]
                     opts))
        actual-ret (r/ret actual-t (fo/-true-filter))]
    (assoc expr
           :keys ckeyexprs
           :vals cvalexprs
           u/expr-type (below/maybe-check-below actual-ret expected opts))))
