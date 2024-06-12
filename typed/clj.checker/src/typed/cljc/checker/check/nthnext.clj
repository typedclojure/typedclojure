;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.nthnext
  (:require [typed.clojure :as t]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.errors :as err]))

(defn drop-HSequential
  "Drop n elements from HSequential t."
  [n t opts]
  {:pre [(nat-int? n)
         (r/HSequential? t)]
   :post [(r/Type? %)]}
  (let [shift (fn [k]
                {:pre [(keyword? k)]
                 :post [(vector? %)]}
                (let [e (k t)]
                  (if (:repeat t)
                    (vec (take (count e)
                               (nthrest (cycle e) n)))
                    (vec (nthrest e n)))))]
    (r/-hseq (shift :types)
             {:filters (shift :fs)
              :objects (shift :objects)
              :rest (:rest t)
              :drest (:drest t)
              :repeat (:repeat t)}
             opts)))

(defn nthnext-hsequential [t n opts]
  {:pre [(r/HSequential? t)
         (nat-int? n)]
   :post [(r/Type? %)]}
  (let [res (drop-HSequential n t opts)]
    (cond
      (or (:rest res)
          (:drest res)
          (:repeat res))
      (c/Un [r/-nil res] opts)

      (empty? (:types res)) r/-nil
      :else res)))

(defn nthrest-hsequential [t n opts]
  {:pre [(r/HSequential? t)
         (nat-int? n)]
   :post [(r/Type? %)]}
  (drop-HSequential n t opts))

(defn nthnext-kw-args-seq [t n opts]
  {:pre [(r/KwArgsSeq? t)
         (nat-int? n)]
   :post [(r/Type? %)]}
  (if (zero? n)
    (c/Un [r/-nil
           (c/In [t (r/make-CountRange 1)] opts)]
          opts)
    (c/-name `t/NilableNonEmptyASeq r/-any)))

(defn nthrest-type [t n opts]
  {:pre [(r/Type? t)
         (nat-int? n)]
   :post [((some-fn nil? r/Type?) %)]}
  (if (zero? n)
    t
    (let [t (c/fully-resolve-type t opts)]
      (cond
        (r/Union? t) (let [ts (map #(nthrest-type % n opts) (:types t))]
                       (when (every? identity ts)
                         (c/Un ts opts)))
        (r/Intersection? t) (when-let [ts (seq (keep #(nthrest-type % n opts) (:types t)))]
                              (c/In ts opts))
        (r/Nil? t) (r/-hseq [] {} opts)
        (r/HSequential? t) (nthrest-hsequential t n opts)
        :else (when-let [res (cgen/unify-or-nil
                               {:fresh [x]
                                :out x
                                :opts opts}
                               t
                               (c/Un [r/-nil (c/-name `t/Seqable x)] opts))]
                (c/-name `t/ASeq res))))))

(defn nthnext-type [t n opts]
  {:pre [(r/Type? t)
         (nat-int? n)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [t (c/fully-resolve-type t opts)]
    (cond
      (r/Union? t) (let [ts (map #(nthnext-type % n opts) (:types t))]
                     (when (every? identity ts)
                       (c/Un ts opts)))
      (r/Intersection? t) (when-let [ts (seq (keep #(nthnext-type % n opts) (:types t)))]
                            (c/In ts opts))
      (r/Nil? t) r/-nil
      (r/HSequential? t) (nthnext-hsequential t n opts)
      (r/KwArgsSeq? t) (nthnext-kw-args-seq t n opts)
      :else (when-let [res (cgen/unify-or-nil
                             {:fresh [x]
                              :out x
                              :opts opts}
                             t
                             (c/Un [r/-nil (c/-name `t/Seqable x)] opts))]
              (c/-name `t/NilableNonEmptyASeq res)))))

(defn seq-type [t opts]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Type?) %)]}
  (nthnext-type t 0 opts))

(defn check-specific-rest [nrests {:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(nat-int? nrests)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (let [{[ctarget] :args :as expr} (-> expr
                                       (update :args #(mapv (fn [e] (check-expr e nil opts)) %))) ;FIXME possibly repeated type check
        target-ret (u/expr-type ctarget)]
    (when-let [t (nthrest-type (r/ret-t target-ret) nrests opts)]
      (-> expr
          (update :fn check-expr nil opts)
          (assoc u/expr-type (below/maybe-check-below
                               (r/ret t (fo/-true-filter))
                               expected
                               opts))))))

(defn check-specific-next [nnexts {:keys [args] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [(nat-int? nnexts)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (let [{[ctarget] :args :as expr} (update expr :args #(mapv (fn [e] (check-expr e nil opts)) %)) ;FIXME possibly repeated type check
        target-ret (u/expr-type ctarget)]
    (when-some [t (nthnext-type (r/ret-t target-ret) nnexts opts)]
      (let [res (r/ret t
                       (if (ind/subtype? t (c/Un [r/-nil (c/-name `t/Coll r/-any)] opts) opts)
                         (cond
                           ; first arity of `seq
                           ;[(NonEmptyColl x) -> (NonEmptyASeq x) :filters {:then tt :else ff}]
                           (ind/subtype? t (r/make-CountRange (inc nnexts)) opts) (fo/-true-filter)

                           ; handle empty collection with no object
                           (and (= orep/-empty (:o target-ret))
                                (ind/subtype? t (c/Un [r/-nil (r/make-CountRange 0 nnexts)] opts) opts))
                           (fo/-false-filter)

                           ; second arity of `seq
                           ;[(Option (Coll x)) -> (Option (NonEmptyASeq x))
                           ; :filters {:then (& (is NonEmptyCount 0)
                           ;                    (! nil 0))
                           ;           :else (or (is nil 0)
                           ;                     (is EmptyCount 0))}]
                           :else (fo/-FS (fo/-and [(fo/-filter-at (r/make-CountRange (inc nnexts))
                                                                  (:o target-ret))
                                                   (fo/-not-filter-at r/-nil
                                                                      (:o target-ret))]
                                                  opts)
                                         (fo/-or [(fo/-filter-at r/-nil
                                                                 (:o target-ret))
                                                  (fo/-filter-at (r/make-CountRange 0 nnexts)
                                                                 (:o target-ret))]
                                                 opts)))
                         (fo/-simple-filter)))]
      (-> expr
          (update :fn check-expr nil opts)
          (assoc u/expr-type (below/maybe-check-below
                               res
                               expected
                               opts)))))))

(defn check-nthnext [{[_ {maybe-int :form} :as args] :args :as expr} expected opts]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (when (= 2 (count args))
    (when (nat-int? maybe-int)
      (check-specific-next maybe-int expr expected opts))))

(defn check-next [{:keys [args] :as expr} expected opts]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (when (= 1 (count args))
    (check-specific-next 1 expr expected opts)))

(defn check-seq [{:keys [args] :as expr} expected opts]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (when (= 1 (count args))
    (check-specific-next 0 expr expected opts)))

(defn check-rest [{:keys [args] :as expr} expected opts]
  {:pre [(every? (comp #{:unanalyzed} :op) args)]
   :post [(or (nil? %)
              (-> % u/expr-type r/TCResult?))]}
  (when (= 1 (count args))
    (check-specific-rest 1 expr expected opts)))
