;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.inst
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

(defn manual-inst
  "Poly (Vec Type) (Map Sym Type) -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys named opts]
  {:pre [((some-fn r/Poly? r/PolyDots?) ptype)
         (vector? argtys)
         (every? r/Type? argtys)
         (every? symbol? (keys named))
         (every? r/Type? (vals named))]
   :post [(r/Type? %)]}
  (let [dotted? (r/PolyDots? ptype)
        nrequired ((if dotted? dec identity)
                   (- (:nbound ptype) (count (:named ptype))))
        _ (when-not ((if dotted? <= =) nrequired (count argtys))
            (err/int-error
              (str "Wrong number of arguments to instantiate polymorphic type (expected " 
                   (when dotted? "at least ")
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
        ;; splice :named arguments between fixed and dotted params
        argtys-before-named-subst (let [[fixedtys dottedtys] [(subvec argtys 0 nrequired)
                                                              (subvec argtys nrequired)]]
                                    ;; :named arguments default to t/Any
                                    (into fixedtys
                                          (concat (repeat (count expected-named) r/-any)
                                                  dottedtys)))
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
    (cond
      (r/Poly? ptype)
      (let [names (c/Poly-fresh-symbols* ptype)
            body (c/Poly-body* names ptype opts)
            bbnds (c/Poly-bbnds* names ptype opts)
            opts (free-ops/with-bounded-frees opts (zipmap (map r/make-F names) bbnds))
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

      (r/PolyDots? ptype)
      (let [names (vec (c/PolyDots-fresh-symbols* ptype))
            body (c/PolyDots-body* names ptype opts)
            bbnds (c/PolyDots-bbnds* names ptype opts)
            _ (assert (= r/dotted-no-bounds (peek bbnds)) "TODO interesting dotted bound")
            dotted-argtys-start (dec (:nbound ptype))
            opts (free-ops/with-bounded-frees opts (zipmap (-> (map r/make-F names) butlast) (pop bbnds)))
            _ (doseq [[i nme ty bnds] (map vector (range) (pop names) argtys bbnds)]
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
        (-> body
            ; expand dotted pre-types in body
            (trans/trans-dots (peek names) ;the bound
                              (subvec argtys dotted-argtys-start)  ;the types to expand pre-type with
                              opts)
            ; substitute normal variables
            (subst/substitute-many (subvec argtys 0 dotted-argtys-start) (pop names) opts))))))

(defn inst-from-targs-syn [ptype targs-syn prs-ns expected opts]
  (binding [prs/*unparse-type-in-ns* prs-ns]
    (let [opts (-> opts
                   (assoc ::prs/parse-type-in-ns prs-ns))
          ptype (c/fully-resolve-type ptype opts)
          ptype (or (when (r/Intersection? ptype)
                      (some #(when ((some-fn r/Poly? r/PolyDots?) %)
                               %)
                            (:types ptype)))
                    ptype)
          ; support (inst :kw ...)
          ptype (cond-> ptype
                  (c/keyword-value? ptype) (c/KeywordValue->Fn opts))]
      (if-not ((some-fn r/Poly? r/PolyDots?) ptype)
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
              _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
                  (when (seq unsupported)
                    (err/int-error (str "Unsupported keyword argument(s) to inst " unsupported) opts)))
              _ (when (contains? kwargs :named)
                  (when-not (and (map? named)
                                 (every? symbol? (keys named)))
                    (err/int-error (str ":named keyword argument to inst must be a map of symbols to types, given: " (pr-str named)) opts)))
              named (into {}
                          (map (fn [[k v]]
                                 [k (prs/parse-type v opts)]))
                          named)
              targs (mapv #(prs/parse-type % opts) targs-syn)]
          (below/maybe-check-below
            (r/ret (manual-inst ptype targs named opts))
            expected
            opts))))))
