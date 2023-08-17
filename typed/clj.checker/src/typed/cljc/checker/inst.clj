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
            [typed.cljc.checker.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic type instantiation

(defn same-or-narrower-bounds?
  "True if arg-bounds is inside bounds, false otherwise"
  [bounds arg-bounds]
  {:pre [(r/Bounds? bounds)
         (r/Bounds? arg-bounds)]
   :post [(boolean? %)]}
  (and (sub/subtype? (:lower-bound bounds) (:lower-bound arg-bounds))
       (sub/subtype? (:upper-bound arg-bounds) (:upper-bound bounds))
       ; make sure bounds make sense
       (sub/subtype? (:lower-bound bounds) (:upper-bound bounds))
       (sub/subtype? (:lower-bound arg-bounds) (:upper-bound arg-bounds))))

(defn satisfies-bounds?
  "True if type t is inside bounds"
  [t bnds]
  (and (sub/subtype? t (:upper-bound bnds))
       (sub/subtype? (:lower-bound bnds) t)))

(defn manual-inst
  "Poly (Vec Type) (Map Sym Type) -> Type
  Substitute the type parameters of the polymorphic type
  with given types"
  [ptype argtys named]
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
                   "\n\nTarget:\n" (prs/unparse-type ptype)
                   "\n\nActual arguments:\n" (str/join " " (map prs/unparse-type argtys)))))
        expected-named (:named ptype)
        _ (when (empty? expected-named)
            (when (seq named)
              (err/int-error (str "Passed :named types to instantiate first argument of inst, but this type doesn't have :named arguments: "
                                  (pr-str ptype)))))
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
                                               " expected one of: " (pr-str (keys expected-named))))))
                       argtys-before-named-subst
                       named)]
    (cond
      (r/Poly? ptype)
      (let [names (c/Poly-fresh-symbols* ptype)
            body (c/Poly-body* names ptype)
            bbnds (c/Poly-bbnds* names ptype)]
        (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
          (doseq [[i nme ty bnds] (map vector (range) names argtys bbnds)]
            (assert (not (:higher-kind bnds)))
            (let [lower-bound (subst/substitute-many (:lower-bound bnds) (take i argtys) (take i names))
                  upper-bound (subst/substitute-many (:upper-bound bnds) (take i argtys) (take i names))]
              (when-not (sub/subtype? lower-bound upper-bound)
                (err/int-error
                  (str "Lower-bound " (prs/unparse-type lower-bound)
                       " is not below upper-bound " (prs/unparse-type upper-bound))))
              (when-not (and (sub/subtype? ty upper-bound)
                             (sub/subtype? lower-bound ty))
                (err/int-error
                  (str "Manually instantiated type " (prs/unparse-type ty)
                       " is not between bounds " (prs/unparse-type lower-bound)
                       " and " (prs/unparse-type upper-bound))))))
          (subst/substitute-many body argtys names)))

      (r/PolyDots? ptype)
      (let [names (vec (c/PolyDots-fresh-symbols* ptype))
            body (c/PolyDots-body* names ptype)
            bbnds (c/PolyDots-bbnds* names ptype)
            dotted-argtys-start (dec (:nbound ptype))]
        (free-ops/with-bounded-frees (zipmap (-> (map r/make-F names) butlast) (butlast bbnds))
          (doseq [[i nme ty bnds] (map vector (range) (pop names) argtys bbnds)]
            (let [lower-bound (subst/substitute-many (:lower-bound bnds) (take i argtys) (take i names))
                  upper-bound (subst/substitute-many (:upper-bound bnds) (take i argtys) (take i names))]
              (when-not (sub/subtype? lower-bound upper-bound)
                (err/int-error
                  (str "Lower-bound " (prs/unparse-type lower-bound)
                       " is not below upper-bound " (prs/unparse-type upper-bound))))
              (when-not (and (sub/subtype? ty upper-bound)
                             (sub/subtype? lower-bound ty))
                (err/int-error
                  (str "Manually instantiated type " (prs/unparse-type ty)
                       " is not between bounds " (prs/unparse-type lower-bound)
                       " and " (prs/unparse-type upper-bound))))))
          (-> body
            ; expand dotted pre-types in body
            (trans/trans-dots (peek names) ;the bound
                              (subvec argtys dotted-argtys-start)) ;the types to expand pre-type with
            ; substitute normal variables
            (subst/substitute-many (subvec argtys 0 dotted-argtys-start) (pop names))))))))

(defn inst-from-targs-syn [ptype targs-syn prs-ns expected]
  (binding [prs/*parse-type-in-ns* prs-ns
            prs/*unparse-type-in-ns* prs-ns]
    (let [ptype (c/fully-resolve-type ptype)
          ; support (inst :kw ...)
          ptype (if (c/keyword-value? ptype)
                  (c/KeywordValue->Fn ptype)
                  ptype)]
      (if-not ((some-fn r/Poly? r/PolyDots?) ptype)
        (err/tc-delayed-error (str "Cannot instantiate non-polymorphic type: " (prs/unparse-type ptype))
                              :return (cu/error-ret expected))
        (let [[targs-syn kwargs] (split-with (complement keyword?) targs-syn)
              _ (when-not (even? (count kwargs))
                  (err/int-error (str "Expected an even number of keyword options to inst, given: " (vec kwargs))))
              _ (when (seq kwargs)
                  (when-not (apply distinct? (map first (partition 2 kwargs)))
                    (err/int-error (str "Gave repeated keyword args to inst: " (vec kwargs)))))
              {:keys [named] :as kwargs} kwargs
              _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
                  (when (seq unsupported)
                    (err/int-error (str "Unsupported keyword argument(s) to inst " unsupported))))
              _ (when (contains? kwargs :named)
                  (when-not (and (map? named)
                                 (every? symbol? (keys named)))
                    (err/int-error (str ":named keyword argument to inst must be a map of symbols to types, given: " (pr-str named)))))
              named (into {}
                          (map (fn [[k v]]
                                 [k (prs/parse-type v)]))
                          named)
              targs (mapv prs/parse-type targs-syn)]
          (below/maybe-check-below
            (r/ret (manual-inst ptype targs named))
            expected))))))
