;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.promote-demote
  (:require [typed.cljc.checker.utils :as u]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.clojure :as t]
            [typed.cljc.checker.hset-utils :as hset]
            [clojure.set :as set]
            [typed.cljc.checker.impl-protocols :as p]
            typed.cljc.checker.filter-rep)
  (:import (typed.cljc.checker.type_rep NotType Intersection Union FnIntersection
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly
                                        Mu HeterogeneousMap Bounds
                                        CountRange Name Value Top Wildcard Unchecked TopFunction B F Result AnyValue
                                        TCError Extends JSNominal
                                        JSString JSBoolean JSNumber CLJSInteger JSObject
                                        ArrayCLJS FunctionCLJS KwArgsSeq HSequential HSet
                                        AnyValue TopFunction Scope DissocType AssocType MergeType
                                        GetType JSUndefined JSNull JSSymbol JSObj TypeOf
                                        SymbolicClosure Instance Satisfies)
           (typed.cljc.checker.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter)))

;TODO automatically check for completeness

(comment
(defn completeness-check []
  (let [vs (-> IPromoteDemote :impls keys set)
        expecteds (set (map resolve @u/all-types))
        missing (set/difference expecteds vs)]
    (when (seq missing)
      {:missing missing})))
)

;; Note: paths and path elements are currently ignored because they cannot contain variables.
;; FIXME this should probably consider filters too.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable Elimination

(t/defalias ElimVars
  "A set of variables to be eliminated via promotion
  or demotion."
  (t/Set clojure.lang.Symbol))

;(t/ann ^:no-check promote [r/AnyType ElimVars -> r/AnyType])
;(t/ann ^:no-check demote [r/AnyType ElimVars -> r/AnyType])
(defprotocol IPromoteDemote
  (promote [T V] "Eliminate all variables V in t by promotion")
  (demote [T V] "Eliminate all variables V in T by demotion"))

(t/ann promote-var [r/AnyType ElimVars -> r/AnyType])
(defn promote-var [T V]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/AnyType? %)]}
  (promote T V))

(t/ann demote-var [r/AnyType ElimVars -> r/AnyType])
(defn demote-var [T V]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/AnyType? %)]}
  (demote T V))

(defmacro promote-demote [cls & fbody]
  `(extend-type ~cls
     IPromoteDemote
     (promote [T# V#]
       (let [~'promote promote
             ~'demote demote
             f# (fn ~@fbody)]
         (f# T# V#)))
     (demote [T# V#] 
       (let [~'promote demote
             ~'demote promote
             f# (fn ~@fbody)]
         (f# T# V#)))))

(promote-demote ArrayCLJS 
  [T V]
  (-> T
    (update :input-type demote V)
    (update :output-type promote V)))

(promote-demote PrimitiveArray
  [T V]
  (-> T
    (update :input-type demote V)
    (update :output-type promote V)))

(defn promote-Kind->Type [T V]
  {:pre [(r/Kind? T)]}
  (let [rec #(promote-Kind->Type % V)
        pmt #(promote % V)]
    (cond
      (r/Bounds? T) (pmt (:upper-bound T))
      (r/Regex? T) (update T :types #(mapv rec %))
      :else (err/nyi-error (str "demote-Kind->Type: " (class T))))))

(defn demote-Kind->Type [T V]
  {:pre [(r/Kind? T)]}
  (let [rec #(promote-Kind->Type % V)
        dmt #(demote % V)]
    (cond
      (r/Bounds? T) (dmt (:lower-bound T))
      (r/Regex? T) (update T :types #(mapv rec %))
      :else (err/nyi-error (str "demote-Kind->Type: " (class T))))))

(extend-type F
  IPromoteDemote
  (promote [{:keys [name] :as T} V]
    (if (V name)
      (let [bnd (free-ops/free-in-scope-bnds name)]
        (when-not bnd
          (err/int-error (str "Missing kind for type variable " name)))
        (promote-Kind->Type bnd V))
      T))
  (demote [{:keys [name] :as T} V]
    (if (V name)
      (let [bnd (free-ops/free-in-scope-bnds name)]
        (when-not bnd
          (err/int-error (str "Missing kind for type variable " name)))
        (demote-Kind->Type bnd V))
      T)))

(extend-type Bounds
  IPromoteDemote
  (promote [T V]
    (-> T
        (update :upper-bound promote V)
        (update :lower-bound promote V)))
  (demote [T V]
    (-> T
        (update :upper-bound demote V)
        (update :lower-bound demote V))))

(defn handle-kw-map [m p-or-d-fn V]
  (c/visit-type-map m #(p-or-d-fn % V)))

(promote-demote KwArgsSeq
  [T V]
  (-> T
    (update :mandatory handle-kw-map promote V)
    (update :optional handle-kw-map promote V)))

(promote-demote HeterogeneousMap
  [T V]
  (-> T
    (update :types handle-kw-map promote V)
    (update :optional handle-kw-map promote V)))

(promote-demote JSObj
  [T V]
  (-> T
    (update :types handle-kw-map promote V)))

(promote-demote HSequential
  [T V]
  (let [pmt #(promote % V)
        latent-filter-vs (set/intersection (into #{} (mapcat frees/fv) (:fs T))
                                           (into #{} (mapcat frees/fi) (:fs T)))]
    (cond
      ;if filter contains V, give up
      (seq (set/intersection V latent-filter-vs)) (case (:kind T)
                                                    :vector (c/RClass-of clojure.lang.IPersistentVector [r/-any])
                                                    :seq (c/RClass-of clojure.lang.ISeq [r/-any])
                                                    :list (c/RClass-of clojure.lang.IPersistentList [r/-any])
                                                    :sequential
                                                    (c/In (c/RClass-of clojure.lang.Sequential)
                                                          (c/RClass-of clojure.lang.IPersistentCollection [r/-any])))

      ;if dotted bound is in V, transfer to rest args
      (and (:drest T) (V (-> T :drest :name)))
      (r/-hsequential (mapv pmt (:types T))
                      :filters (:fs T)
                      :objects (:objects T)
                      :rest (pmt (-> T :drest :pre-type))
                      :kind (:kind T))

      :else
      (r/-hsequential (mapv pmt (:types T))
                      ; we know no filters contain V
                      :filters (:fs T)
                      :objects (:objects T)
                      :rest (some-> (:rest T)
                                    pmt)
                      :drest (some-> (:drest T)
                                     (update :pre-type pmt))
                      :repeat (:repeat T)
                      :kind (:kind T)))))

(promote-demote HSet
  [T V]
  (let [fixed (set (map promote (:fixed T) (repeat V)))
        h (r/-hset fixed :complete? (:complete? T))]
    (if (every? (fn [a] 
                  (and (r/Value? a)
                       (hset/valid-fixed? (:val a))))
                fixed)
      h
      (c/upcast-hset h))))

(promote-demote Value [T V] T)

(promote-demote JSNominal [T V]
  (-> T
      (update :poly? #(when %
                        (mapv promote % (repeat V))))))

(promote-demote DataType [T V]
  (-> T
      (update :poly? #(when %
                        (mapv promote % (repeat V))))
      #_(update :fields #(apply array-map
                                (apply concat
                                       (for [[k v] %]
                                         [k (promote v V)]))))))

(defmacro promote-demote-id [& cs]
  `(do ~@(map (fn [c]
                `(promote-demote ~c [T# V#] T#))
              cs)))

(promote-demote-id B Name Top Wildcard Unchecked TCError CountRange JSString
                   JSBoolean JSNumber JSObject CLJSInteger
                   FunctionCLJS AnyValue TopFunction JSUndefined
                   JSNull JSSymbol TypeOf
                   ;;FIXME ???
                   SymbolicClosure Instance Satisfies)

(promote-demote GetType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update :target pmt)
        (update :key pmt)
        (update :not-found pmt)
        ;(update :target-fs pmt)
        ;(update :target-object pmt)
        )))

(promote-demote AssocType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update :target pmt)
        (update :entries (fn [entries] (mapv #(mapv pmt %) entries)))
        (update :dentries #(some-> % (update :pre-type pmt))))))

(promote-demote MergeType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update :types #(mapv pmt %)))))

(promote-demote DissocType
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update :target pmt)
        (update :keys (fn [keys] (mapv pmt keys)))
        (update :dkeys #(some-> % (update :pre-type pmt))))))

(promote-demote Scope
  [T V]
  (-> T
      (update :body #(promote % V))))

(promote-demote TApp
  [T V]
  (-> T
      (update :rator #(promote % V))
      (update :rands (fn [rands] (mapv #(promote % V) rands)))))

(promote-demote App
  [T V]
  (-> T
      (update :rator #(promote % V))
      (update :rands (fn [rands] (mapv #(promote % V) rands)))))

(promote-demote Union 
  [T V]
  (apply c/Un (map promote (:types T) (repeat V))))

; FIXME is this correct? Promoting NotType should make the inner type smaller,
; and demoting should make inner type bigger?
(extend-type NotType
  IPromoteDemote
  (promote [T V] 
    (-> T
        (update :type demote V)))
  (demote [T V] 
    (-> T
        (update :type promote V))))

(promote-demote Extends
  [T V] 
  (c/-extends
    (map promote (:extends T) (repeat V))
    :without (map demote (:without T) (repeat V))))

(promote-demote Intersection
  [T V] 
  (apply c/In (map promote (:types T) (repeat V))))

(promote-demote FnIntersection
  [T V] 
  (-> T
      (update :types #(mapv promote % (repeat V)))))

(promote-demote Protocol
  [T V]
  (let [pmt #(promote % V)]
    (-> T
        (update :poly? #(some->> % (mapv pmt)))
        (update :methods update-vals pmt))))

(promote-demote RClass
  [T V]
  (let [pmt #(promote % V)]
    (-> T
      (update :poly? #(some->> % (mapv pmt))))))

(promote-demote TypeFn
  [{:keys [variances] :as T} V]
  (let [names (c/TypeFn-fresh-symbols* T)
        pmt-body (promote (c/TypeFn-body* names T) V)]
    (c/TypeFn* names 
               variances
               (c/TypeFn-bbnds* names T)
               pmt-body)))

(promote-demote Poly [T V]
  (case (:kind T)
    :Poly (let [names (c/Poly-fresh-symbols* T)
                ;;TODO promote?
                bbnds (c/Poly-bbnds* names T)
                pmt-body (promote (c/Poly-body* names T) V)]
            (c/Poly* names 
                     bbnds
                     pmt-body))
    :PolyDots (let [names (c/PolyDots-fresh-symbols* T)
                    ;;TODO promote?
                    bbnds (c/PolyDots-bbnds* names T)
                    pmt-body (promote (c/PolyDots-body* names T) V)]
                (c/PolyDots* names 
                             bbnds
                             pmt-body))))

(promote-demote Mu 
  [T V]
  (let [name (c/Mu-fresh-symbol* T)
        body (c/Mu-body* name T)]
    (c/Mu* name (promote body V))))

;; Note: ignores path elements
(extend-type Function
  IPromoteDemote
  (promote
    [{:keys [dom rng rest drest kws] :as T} V]
    {:pre [(#{:fixed :rest :drest :kws} (:kind T))]}
    (let [pmt #(promote % V)
          dmt #(demote % V)
          dmt-kw #(into {} (for [[k v] %]
                             [k (dmt v)]))
          latent-filter-vs (let [f (r/Result-filter* rng)]
                             (set/intersection (frees/fv f)
                                               (frees/fi f)))]
      (cond 
        ;if filter contains V, give up
        (seq (set/intersection V latent-filter-vs)) (r/TopFunction-maker)

        ;if dotted bound is in V, transfer to rest args
        (and drest (V (:name drest)))
        (-> T
          (update :dom #(mapv dmt %))
          (update :rng pmt)
          (assoc :rest (dmt (:pre-type drest)))
          (assoc :drest nil)
          (assoc :kws (some-> kws
                              (update :mandatory dmt-kw)
                              (update :optional dmt-kw))))

        :else
        (-> T
            (update :dom #(mapv dmt %))
            ;we know no filters contain V
            (update :rng #(-> %
                              (update :t pmt)))
            (update :rest #(some-> % dmt))
            (update :drest #(some-> % (update :pre-type dmt)))
            (update :kws #(some-> %
                                  (update :mandatory dmt-kw)
                                  (update :optional dmt-kw)))))))

  (demote [{:keys [dom rng rest drest kws] :as T} V]
    {:pre [(#{:fixed :rest :drest :kws} (:kind T))]}
    (let [pmt #(promote % V)
          dmt #(demote % V)
          pmt-kw #(into {} (for [[k v] %]
                             [k (pmt v)]))
          latent-filter-vs (let [f (r/Result-filter* rng)]
                             (set/intersection (frees/fv f)
                                               (frees/fi f)))]
      (cond 
        ;if filter contains V, give up
        (seq (set/intersection V latent-filter-vs)) (r/TopFunction-maker)

        ;if dotted bound is in V, transfer to rest args
        (and drest (V (:name drest)))
        (-> T
          (update :dom #(mapv pmt %))
          (update :rng dmt)
          (assoc :rest (pmt (:pre-type drest)))
          (assoc :drest nil)
          (assoc :kws (some-> kws
                              (update :mandatory pmt-kw)
                              (update :optional pmt-kw))))

        :else
        (-> T
            (update :dom #(mapv pmt %))
            ;we know no filters contain V
            (update :rng #(-> %
                              (update :t pmt)))
            (update :rest #(some-> % pmt))
            (update :drest #(some-> %
                                    (update :pre-type pmt)))
            (update :kws #(some-> %
                                  (update :mandatory pmt-kw)
                                  (update :optional pmt-kw))))))))
