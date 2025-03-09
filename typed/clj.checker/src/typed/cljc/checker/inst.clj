;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.inst
  (:require [clojure.core.typed.errors :as err]
            [clojure.set :as set]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.trans :as trans]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r])
  (:import (typed.cljc.checker.type_rep Bounds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

#_
(defn same-or-narrower-bounds?
  "True if arg-bounds is inside bounds, false otherwise"
  [bounds arg-bounds opts]
  {:pre [(r/Bounds? bounds)
         (r/Bounds? arg-bounds)]
   :post [(boolean? %)]}
  (and (sub/subtype? (:lower-bound bounds) (:lower-bound arg-bounds) opts)
       (sub/subtype? (:upper-bound arg-bounds) (:upper-bound bounds) opts)
       ; make sure bounds make sense
       (sub/subtype? (:lower-bound bounds) (:upper-bound bounds) opts)
       (sub/subtype? (:lower-bound arg-bounds) (:upper-bound arg-bounds) opts)))

#_
(defn satisfies-bounds?
  "True if type t is inside bounds"
  [t bnds opts]
  (and (sub/subtype? t (:upper-bound bnds) opts)
       (sub/subtype? (:lower-bound bnds) t opts)))

(defn nfixed-args [ptype]
  {:pre [(r/-Poly? ptype)]}
  (- (:nbound ptype) (count (:named ptype))))

(defn manual-inst
  "Poly (Vec Type) (Map Sym Type) -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys named opts]
  {:pre [(r/-Poly? ptype)
         (vector? argtys)
         (every? r/AnyType? argtys)
         (every? symbol? (keys named))
         (every? r/AnyType? (vals named))]
   :post [(r/Type? %)]}
  (let [kind (:kind ptype)
        nrequired (nfixed-args ptype)
        argtys (case kind
                 :PolyDots (if (or (< (count argtys) (dec nrequired)) ;; insufficient fixed args, will fail with int-error
                                   ;; already correct format
                                   (and (= nrequired (count argtys))
                                        (r/Regex? (peek argtys))))
                             argtys
                             ;; backwards compat with single dotted arg
                             ;; (inst f)       => (inst f (t/cat))
                             ;; (inst f A B C) => (inst f (t/cat A B C))
                             (conj (subvec argtys 0 (dec nrequired))
                                   (r/regex
                                     (subvec argtys (dec nrequired))
                                     :cat)))
                 argtys)
        _ (when-not (= nrequired (count argtys))
            (err/int-error
              (str "Wrong number of arguments to instantiate polymorphic type (expected " 
                   nrequired
                   ", actual " (count argtys)
                   "\n\nTarget:\n" (prs/unparse-type ptype opts)
                   "\n\nActual arguments:\n" (str/join " " (map #(prs/unparse-type % opts) argtys)))
              opts))
        expected-named (:named ptype)
        _ (when (empty? expected-named)
            (when (seq named)
              (err/int-error (str "Passed :named types to instantiate first argument of inst, but this type doesn't have :named arguments: "
                                  (pr-str ptype))
                             opts)))
        argtys-before-named-subst (-> argtys
                                      (cond-> (= :PolyDots kind) pop)
                                      ;; :named arguments default to t/Any
                                      (into (repeat (count expected-named) r/-any))
                                      ;; dotted arg goes last
                                      (cond-> (= :PolyDots kind) (conj (peek argtys))))
        ;; fill in provided :named arguments
        argtys (reduce (fn [argtys [k v]]
                         (if-let [i (get expected-named k)]
                           (assoc argtys i v)
                           (err/int-error (str "Unrecognized :named variable passed to inst, "
                                               "given: " (pr-str k)
                                               " expected one of: " (pr-str (keys expected-named)))
                                          opts)))
                       argtys-before-named-subst
                       named)]
    (case kind
      :Poly
      (let [names (c/Poly-fresh-symbols* ptype)
            bbnds (c/Poly-bbnds* names ptype opts)
            opts (free-ops/with-bounded-frees opts names bbnds)
            body (c/Poly-body* names ptype opts)
            _ (doseq [[i nme ty bnds] (map vector (range) names argtys bbnds)]
                (assert (instance? Bounds bnds) "TODO other kinds")
                (let [lower-bound (subst/substitute-many (:lower-bound bnds) (take i argtys) (take i names) opts)
                      upper-bound (subst/substitute-many (:upper-bound bnds) (take i argtys) (take i names) opts)]
                  (when-not (sub/subtype? lower-bound upper-bound opts)
                    (err/int-error
                      (str "Lower-bound " (prs/unparse-type lower-bound opts)
                           " is not below upper-bound " (prs/unparse-type upper-bound opts))
                      opts))
                  (when-not (and (sub/subtype? ty upper-bound opts)
                                 (sub/subtype? lower-bound ty opts))
                    (err/int-error
                      (str "Manually instantiated type " (prs/unparse-type ty opts)
                           " is not between bounds " (prs/unparse-type lower-bound opts)
                           " and " (prs/unparse-type upper-bound opts))
                      opts))))]
        (subst/substitute-many body argtys names opts))

      :PolyDots
      (let [names (c/PolyDots-fresh-symbols* ptype)
            bbnds (c/PolyDots-bbnds* names ptype opts)
            opts (free-ops/with-bounded-frees opts names bbnds)
            body (c/PolyDots-body* names ptype opts)
            _ (when-not (= r/dotted-no-bounds (peek bbnds))
                (err/nyi-error "TODO interesting dotted bound" opts))
            dotted-cat (peek argtys)
            _ (when-not (and (r/Regex? dotted-cat)
                             (= :cat (:kind dotted-cat))
                             (not-any? r/Regex? (:types dotted-cat)))
                (err/int-error
                  (str "Must instantiate dotted variable with flat (t/cat ...): "
                       (prs/unparse-type dotted-cat opts))
                  opts))
            opts (free-ops/with-bounded-frees opts names bbnds)
            _ (doseq [[i nme ty bnds] (map vector (range) names argtys bbnds)]
                (let [bnds (subst/substitute-many bnds (take i argtys) (take i names) opts)]
                  (when-not (sub/has-kind? ty bnds opts)
                    (err/int-error
                      (str "Manually instantiated type " (prs/unparse-type ty opts)
                           " is not of kind " (prs/unparse-type bnds opts))
                      opts))))]
        (-> body
            ; expand dotted pre-types in body
            (trans/trans-dots (peek names) ;the bound
                              (:types dotted-cat)  ;the types to expand pre-type with
                              opts)
            ; substitute normal variables
            (subst/substitute-many (pop argtys) (pop names) opts))))))

(defn inst-from-targs-syn [ptype targs-syn prs-ns expected opts]
  (let [opts (-> opts
                 (assoc ::prs/parse-type-in-ns prs-ns)
                 (prs/with-unparse-ns prs-ns))
        ptype (c/fully-resolve-type ptype opts)
        ptype (or (when (r/Intersection? ptype)
                    (some #(when (r/-Poly? %) %)
                          (:types ptype)))
                  ptype)
        ; support (inst :kw ...)
        ptype (cond-> ptype
                (c/keyword-value? ptype) (c/KeywordValue->Fn opts))]
    (if-not (r/-Poly? ptype)
      (err/tc-delayed-error (str "Cannot instantiate non-polymorphic type: " (prs/unparse-type ptype opts))
                            {:return (cu/error-ret expected)}
                            opts)
      (let [[targs-syn kwargs] (split-with (complement keyword?) targs-syn)
            _ (when-not (even? (count kwargs))
                (err/int-error (str "Expected an even number of keyword options to inst, given: " (vec kwargs)) opts))
            _ (when (seq kwargs)
                (when-not (apply distinct? (map first (partition 2 kwargs)))
                  (err/int-error (str "Gave repeated keyword args to inst: " (vec kwargs)) opts)))
            {:keys [named] :as kwargs} kwargs
            _ (when-some [unsupported (not-empty (disj (set (keys kwargs)) :named))]
                (err/int-error (str "Unsupported keyword argument(s) to inst " unsupported) opts))
            _ (when (contains? kwargs :named)
                (when-not (and (map? named)
                               (every? symbol? (keys named)))
                  (err/int-error (str ":named keyword argument to inst must be a map of symbols to types, given: " (pr-str named)) opts)))
            named (update-vals named #(prs/parse-type % opts))
            allowed-regex-pos (when (= :PolyDots (:kind ptype))
                                (let [nrequired (nfixed-args ptype)]
                                  (when (= (count targs-syn) nrequired)
                                    #{(dec nrequired)})))
            targs (into [] (map-indexed (fn [i tsyn]
                                          (prs/parse-type (cond-> tsyn
                                                            (contains? allowed-regex-pos i) prs/allow-regex)
                                                          opts)))
                        targs-syn)]
        (below/maybe-check-below
          (r/ret (manual-inst ptype targs named opts))
          expected
          opts)))))
