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
                                        DottedPretype Function RClass TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly
                                        Mu HeterogeneousMap Bounds
                                        CountRange Name Value Top Wildcard Unchecked TopFunction B F Result
                                        TCError JSNominal
                                        JSString JSBoolean JSNumber CLJSInteger JSObject
                                        ArrayCLJS FunctionCLJS KwArgsSeq HSequential HSet
                                        TopFunction Scope DissocType AssocType MergeType
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
  (promote [T V opts] "Eliminate all variables V in t by promotion")
  (demote [T V opts] "Eliminate all variables V in T by demotion"))

(t/ann promote-var [r/AnyType ElimVars t/Any -> r/AnyType])
(defn promote-var [T V opts]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/AnyType? %)]}
  (promote T V opts))

(t/ann demote-var [r/AnyType ElimVars t/Any -> r/AnyType])
(defn demote-var [T V opts]
  {:pre [(r/AnyType? T)
         (set? V)
         (every? symbol? V)]
   :post [(r/AnyType? %)]}
  (demote T V opts))

(defmacro promote-demote [cls & fbody]
  `(extend-type ~cls
     IPromoteDemote
     (promote [T# V# ~'opts]
       (let [~'promote (fn
                         ([T# V#]       (promote T# V# ~'opts))
                         ([T# V# opts#] (promote T# V# opts#)))
             ~'demote (fn
                        ([T# V#]       (demote T# V# ~'opts))
                        ([T# V# opts#] (demote T# V# opts#)))
             f# (fn ~@fbody)]
         (f# T# V#)))
     (demote [T# V# ~'opts]
       (let [~'promote (fn
                         ([T# V#]       (demote T# V# ~'opts))
                         ([T# V# opts#] (demote T# V# opts#)))
             ~'demote (fn
                        ([T# V#]       (promote T# V# ~'opts))
                        ([T# V# opts#] (promote T# V# opts#)))
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

(defn promote-Kind->Type [T V opts]
  {:pre [(r/Kind? T)]}
  (let [rec #(promote-Kind->Type % V opts)
        pmt #(promote % V opts)]
    (cond
      (r/Bounds? T) (pmt (:upper-bound T))
      (r/Regex? T) (update T :types #(mapv rec %))
      :else (err/nyi-error (str "demote-Kind->Type: " (class T)) opts))))

(defn demote-Kind->Type [T V opts]
  {:pre [(r/Kind? T)]}
  (let [rec #(promote-Kind->Type % V opts)
        dmt #(demote % V opts)]
    (cond
      (r/Bounds? T) (dmt (:lower-bound T))
      (r/Regex? T) (update T :types #(mapv rec %))
      :else (err/nyi-error (str "demote-Kind->Type: " (class T)) opts))))

(extend-type F
  IPromoteDemote
  (promote [{:keys [name] :as T} V opts]
    (if (V name)
      (let [bnd (free-ops/free-in-scope-bnds name opts)]
        (when-not bnd
          (err/int-error (str "Missing kind for type variable " name) opts))
        (promote-Kind->Type bnd V opts))
      T))
  (demote [{:keys [name] :as T} V opts]
    (if (V name)
      (let [bnd (free-ops/free-in-scope-bnds name opts)]
        (when-not bnd
          (err/int-error (str "Missing kind for type variable " name) opts))
        (demote-Kind->Type bnd V opts))
      T)))

(extend-type Bounds
  IPromoteDemote
  (promote [T V opts]
    (-> T
        (update :upper-bound promote V opts)
        (update :lower-bound promote V opts)))
  (demote [T V opts]
    (-> T
        (update :upper-bound demote V opts)
        (update :lower-bound demote V opts))))

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
        latent-filter-vs (set/intersection (into #{} (mapcat #(frees/fv % opts)) (:fs T))
                                           (into #{} (mapcat #(frees/fi % opts)) (:fs T)))]
    (cond
      ;if filter contains V, give up
      (seq (set/intersection V latent-filter-vs)) (case (:kind T)
                                                    :vector (c/RClass-of clojure.lang.IPersistentVector [r/-any] opts)
                                                    :seq (c/RClass-of clojure.lang.ISeq [r/-any] opts)
                                                    :list (c/RClass-of clojure.lang.IPersistentList [r/-any] opts)
                                                    :sequential
                                                    (c/In [(c/RClass-of clojure.lang.Sequential opts)
                                                           (c/RClass-of clojure.lang.IPersistentCollection [r/-any] opts)]
                                                          opts))

      ;if dotted bound is in V, transfer to rest args
      (and (:drest T) (V (-> T :drest :name)))
      (r/-hsequential (mapv pmt (:types T))
                      {:filters (:fs T)
                       :objects (:objects T)
                       :rest (pmt (-> T :drest :pre-type))
                       :kind (:kind T)}
                      opts)

      :else
      (r/-hsequential (mapv pmt (:types T))
                      ; we know no filters contain V
                      {:filters (:fs T)
                       :objects (:objects T)
                       :rest (some-> (:rest T)
                                     pmt)
                       :drest (some-> (:drest T)
                                      (update :pre-type pmt))
                       :repeat (:repeat T)
                       :kind (:kind T)}
                      opts))))

(promote-demote HSet
  [T V]
  (let [fixed (set (map promote (:fixed T) (repeat V)))
        h (r/-hset fixed :complete? (:complete? T))]
    (if (every? (fn [a] 
                  (and (r/Value? a)
                       (hset/valid-fixed? (:val a))))
                fixed)
      h
      (c/upcast-hset h opts))))

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
                   FunctionCLJS TopFunction JSUndefined
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

(promote-demote Union 
  [T V]
  (c/Un (map promote (:types T) (repeat V)) opts))

; FIXME is this correct? Promoting NotType should make the inner type smaller,
; and demoting should make inner type bigger?
(extend-type NotType
  IPromoteDemote
  (promote [T V opts]
    (-> T
        (update :type demote V opts)))
  (demote [T V opts]
    (-> T
        (update :type promote V opts))))

(promote-demote Intersection
  [T V] 
  (c/In (map promote (:types T) (repeat V)) opts))

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
        bbnds (c/TypeFn-bbnds* names T opts)
        opts (free-ops/with-bounded-frees opts {(map r/make-F names) bbnds})
        pmt-body (promote (c/TypeFn-body* names bbnds T opts) V opts)]
    (c/TypeFn* names 
               variances
               bbnds
               pmt-body
               opts)))

(promote-demote Poly [T V]
  (case (:kind T)
    :Poly (let [names (c/Poly-fresh-symbols* T)
                ;;TODO promote?
                bbnds (c/Poly-bbnds* names T opts)
                opts (free-ops/with-bounded-frees opts (zipmap (map r/make-F names) bbnds))
                pmt-body (promote (c/Poly-body* names T opts) V opts)]
            (c/Poly* names 
                     bbnds
                     pmt-body
                     opts))
    :PolyDots (let [names (c/PolyDots-fresh-symbols* T)
                    ;;TODO promote?
                    bbnds (c/PolyDots-bbnds* names T opts)
                    opts (free-ops/with-bounded-frees opts (zipmap (map r/make-F names) bbnds))
                    pmt-body (promote (c/PolyDots-body* names T opts) V opts)]
                (c/PolyDots* names 
                             bbnds
                             pmt-body
                             opts))))

(promote-demote Mu 
  [T V]
  (let [name (c/Mu-fresh-symbol* T)
        opts (free-ops/with-bounded-frees opts {(r/make-F name) r/no-bounds})
        body (c/Mu-body* name T opts)]
    (c/Mu* name (promote body V opts) opts)))

;; Note: ignores path elements
(extend-type Function
  IPromoteDemote
  (promote
    [{:keys [dom rng rest drest kws] :as T} V opts]
    {:pre [(#{:fixed :rest :drest :kws} (:kind T))]}
    (let [pmt #(promote % V opts)
          dmt #(demote % V opts)
          dmt-kw #(into {} (for [[k v] %]
                             [k (dmt v)]))
          latent-filter-vs (let [f (r/Result-filter* rng)]
                             (set/intersection (frees/fv f opts)
                                               (frees/fi f opts)))]
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

  (demote [{:keys [dom rng rest drest kws] :as T} V opts]
    {:pre [(#{:fixed :rest :drest :kws} (:kind T))]}
    (let [pmt #(promote % V opts)
          dmt #(demote % V opts)
          pmt-kw #(into {} (for [[k v] %]
                             [k (pmt v)]))
          latent-filter-vs (let [f (r/Result-filter* rng)]
                             (set/intersection (frees/fv f opts)
                                               (frees/fi f opts)))]
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
