;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.type-ctors
  (:refer-clojure :exclude [defrecord replace])
  (:require [clojure.core.cache :as cache]
            [typed.clojure :as t]
            [typed.cljc.runtime.env-utils :as env-utils]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.reflect :as reflect]
            [clojure.repl :as repl]
            [clojure.set :as set]
            [typed.clj.checker.rclass-env :as rcls]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.datatype-env :as dtenv]
            [typed.cljc.checker.filter-rep]
            [typed.cljc.checker.fold-rep :as f :refer [add-default-fold-case]]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.impl-protocols :as p]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.object-rep :as or]
            [typed.cljc.checker.path-rep :as path]
            [typed.cljc.checker.protocol-env :as prenv]
            [typed.cljc.checker.tvar-bnds :as bnds]
            [typed.cljc.checker.type-rep :as r :refer [ret-t]]
            [typed.cljc.checker.utils :as u]
            typed.cljc.checker.coerce-ann)
  (:import (clojure.lang ASeq)
           (typed.cljc.checker.type_rep HeterogeneousMap Poly TypeFn PolyDots TApp App Value
                                        Union Intersection F Function Mu B KwArgs KwArgsSeq KwArgsArray
                                        RClass Bounds Name Scope CountRange Intersection DataType Extends
                                        JSNominal Protocol GetType HSequential
                                        HSet AssocType TypeOf MergeType)
           (typed.cljc.checker.type_rep NotType DifferenceType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass JSNominal App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Wildcard Unchecked TopFunction B F Result
                                        TCResult TCError Extends
                                        JSNumber CLJSInteger JSObject JSString ArrayCLJS
                                        JSBoolean AssocType GetType KwArgsSeq KwArgs HSequential HSet
                                        JSUndefined JSNull JSSymbol JSObj TypeOf SymbolicClosure)
           (typed.cljc.checker.filter_rep NoFilter TopFilter BotFilter TypeFilter NotTypeFilter
                                          ImpFilter AndFilter OrFilter FilterSet)
           (typed.cljc.checker.object_rep NoObject EmptyObject Path)
           (typed.cljc.checker.path_rep KeyPE KeysPE ValsPE ClassPE NthPE CountPE KeywordPE)))

(set! *warn-on-reflection* true)

(t/ann ^:no-check with-original-names [r/Type (t/U t/Sym (t/Seqable t/Sym)) -> r/Type])
(defn- with-original-names [t names]
  (vary-meta t assoc ::names names))

(t/ann ^:no-check get-original-names [r/Type -> (t/U t/Sym (t/Seqable t/Sym))])
(defn get-original-names [t]
  (-> t meta ::names))

(t/ann fresh-symbol [t/Sym -> t/Sym])
(defn fresh-symbol [s]
  {:pre [(symbol? s)]
   :post [(symbol? %)]}
  (with-meta (gensym s) {:original-name s}))

(declare Un make-Union make-Intersection fully-resolve-type fully-resolve-non-rec-type flatten-unions)

(t/ann bottom r/Type)
(def ^:private bottom (r/Union-maker (sorted-set-by u/type-comparator)))

(t/ann ^:no-check make-Union [(t/Seqable r/Type) -> r/Type])
(defn make-Union
  "Does not resolve types."
  [args]
  (let [ts (flatten-unions args)]
    (case (count ts)
      0 bottom
      1 (first ts)
      (r/Union-maker ts))))

;; Heterogeneous maps

(declare make-HMap)

(t/ann -complete-hmap [(t/Map r/Type r/Type) -> r/Type])
(defn -complete-hmap [types]
  (make-HMap :mandatory types 
             :complete? true))

(t/ann -partial-hmap (t/IFn [(t/Map r/Type r/Type) -> r/Type]
                            [(t/Map r/Type r/Type) (t/Set r/Type) -> r/Type]))
(defn -partial-hmap 
  ([types] (-partial-hmap types #{}))
  ([types absent-keys] (make-HMap :mandatory types 
                                  :absent-keys absent-keys)))

(t/defalias TypeMap
  "A regular map with types as keys and vals."
  (t/Map r/Type r/Type))

(declare In keyword-value? RClass-of Protocol-of complete-hmap? -name)

(t/ann ^:no-check allowed-hmap-key? [r/Type -> t/Bool])
(defn allowed-hmap-key? [k]
  (keyword-value? k))

; Partial HMaps do not record absence of fields, only subtype to (APersistentMap t/Any t/Any)
(t/ann ^:no-check upcast-hmap* 
       [(t/Map r/Type r/Type) (t/Map r/Type r/Type) (t/Set r/Type) t/Bool -> r/Type])
(defn upcast-hmap* [mandatory optional absent-keys complete?]
  (let [upcast-ctor (fn [ks vs]
                      (impl/impl-case
                        :clojure (RClass-of 'clojure.lang.APersistentMap [ks vs])
                        :cljs (-name 'typed.clojure/Map ks vs)))]
    (if complete?
      (In (upcast-ctor (apply Un (mapcat keys [mandatory optional]))
                       (apply Un (mapcat vals [mandatory optional])))
          (r/make-CountRange 
            ; assume all optional entries are absent
            #_:lower
            (count mandatory)
            ; assume all optional entries are present
            #_:upper
            (+ (count mandatory)
               (count optional))))
      (In (upcast-ctor r/-any r/-any)
          (r/make-CountRange 
            ; assume all optional entries are absent
            #_:lower
            (count mandatory)
            ; partial hmap can be infinite count
            #_:upper
            nil)))))

(t/ann ^:no-check upcast-hmap [HeterogeneousMap -> r/Type])
(defn upcast-hmap [hmap]
  {:pre [(r/HeterogeneousMap? hmap)]
   :post [(r/Type? %)]}
  (upcast-hmap* (:types hmap)
                (:optional hmap)
                (:absent-keys hmap)
                (complete-hmap? hmap)))

(t/ann ^:no-check make-HMap [& :optional {:mandatory (t/Map r/Type r/Type) :optional (t/Map r/Type r/Type)
                                          :absent-keys (t/Set r/Type) :complete? t/Bool} 
                             -> r/Type])
(defn make-HMap 
  "Make a heterogeneous map type for the given options.
  Handles duplicate keys between map properties.
  
  Options:
  - :mandatory    a map of mandatory entries
                  Default: {}
  - :optional     a map of optional entries
                  Default: {}
  - :absent-keys  a set of types that are not keys this/these maps
                  Default: #{}
  - :complete?    creates a complete map if true, or a partial map if false
                  Default: false"
  [& {:keys [mandatory optional complete? absent-keys]
      :or {mandatory {} optional {} complete? false absent-keys #{}}
      :as opt}]
  {:post [(r/Type? %)]}
  (assert (set/subset? (set (keys opt))
                       #{:mandatory :optional :complete? :absent-keys})
          (set (keys opt)))
  (assert ((con/hash-c? r/Type? r/Type?) mandatory)
          (pr-str mandatory))
  (assert ((con/hash-c? r/Type? r/Type?) optional)
          (pr-str optional))
  (assert ((con/set-c? r/Type?) absent-keys)
          (pr-str absent-keys))
  (assert (boolean? complete?)
          (pr-str complete?))
  ; simplifies to bottom with contradictory keys
  (cond 
    (not-every? allowed-hmap-key?
                (concat (keys mandatory)
                        (keys optional)
                        absent-keys))
    (upcast-hmap* mandatory optional absent-keys complete?)

    :else
    (let [optional-now-mandatory (set/intersection
                                   (set (keys optional))
                                   (set (keys mandatory)))
          optional-now-absent (set/intersection
                                (set (keys optional))
                                absent-keys)
          _ (assert (empty? 
                      (set/intersection optional-now-mandatory
                                        optional-now-absent)))
          mandatory (merge-with In mandatory (select-keys optional optional-now-mandatory))
          optional (apply dissoc optional (set/union optional-now-absent
                                                     optional-now-mandatory))
          ; throw away absents if complete
          absent (if complete?
                   #{}
                   (set/union absent-keys optional-now-absent))]
      (cond
        (or (seq (set/intersection (set (keys mandatory))
                                   (set absent-keys)))
            (some #{bottom} (vals mandatory)))
        bottom

        :else (r/HeterogeneousMap-maker 
                mandatory
                optional
                absent
                (not complete?))))))

;TODO to type check this, need to un-munge instance field names
(t/ann complete-hmap? [HeterogeneousMap -> t/Bool])
(defn complete-hmap? [^HeterogeneousMap hmap]
  {:pre [(r/HeterogeneousMap? hmap)]}
  (not (:other-keys? hmap)))

(t/ann partial-hmap? [HeterogeneousMap -> t/Bool])
(defn partial-hmap? [^HeterogeneousMap hmap]
  {:pre [(r/HeterogeneousMap? hmap)]}
  (:other-keys? hmap))

(t/ann ^:no-check upcast-hset [HSet -> r/Type])
(defn upcast-hset [{:keys [fixed complete?] :as hset}]
  {:pre [(r/HSet? hset)]
   :post [(r/Type? %)]}
  (let [tp (if complete?
             (apply Un (:fixed hset))
             r/-any)]
    (impl/impl-case
      :clojure (RClass-of 'clojure.lang.APersistentSet [tp])
      :cljs (-name 'typed.clojure/Set tp))))

; TODO Should update this with prest
(t/ann ^:no-check upcast-HSequential [HSequential -> r/Type])
(defn upcast-HSequential [{:keys [types rest drest kind] :as hsequential}]
  {:pre [(r/HSequential? hsequential)]
   :post [(r/Type? %)]}
  ;; Note: make-Union and make-Intersection used to be Un and In,
  ;; but something funny happened with Ping/Pong.
  ;; See typed-test.cljc.name-utils/find-recursive-names-test.
  (let [tp (if-not drest
             (make-Union
               (concat types
                       (when rest
                         [rest])))
             r/-any)]
    (make-Intersection
      (cond->
        [(impl/impl-case
           :clojure (case kind
                      :vector (RClass-of clojure.lang.APersistentVector [tp])
                      :seq (RClass-of clojure.lang.ASeq [tp])
                      :list (RClass-of clojure.lang.PersistentList [tp])
                      :sequential (In (RClass-of clojure.lang.IPersistentCollection [tp])
                                      (RClass-of clojure.lang.Sequential)))
           :cljs (case kind
                   :vector (-name 'typed.clojure/Vec tp)
                   :seq (-name 'typed.clojure/Seq tp)
                   :list (-name 'typed.clojure/List tp)
                   :sequential (In (-name 'typed.clojure/Coll tp)
                                   (-name 'cljs.core/ISequential))))]
        (not drest) (conj (r/make-CountRange
                            (count types)
                            (when-not rest
                              (count types))))))))

(defn upcast-kw-args-seq [{kws :kw-args-regex
                           :as kwseq}]
  {:pre [(r/KwArgsSeq? kwseq)]
   :post [(r/Type? %)]}
  (let [ss (if (and (:complete? kws)
                    (not (:maybe-trailing-nilable-non-empty-map? kws)))
             (apply Un
                    (concat
                      (apply concat (:mandatory kws))
                      (apply concat (:optional kws))))
             r/-any)
        min-count (* 2 (count (:mandatory kws)))
        max-count (when (:complete? kws)
                    (cond-> (+ (* 2 (count (:mandatory kws)))
                               (* 2 (count (:optional kws))))
                      (:maybe-trailing-nilable-non-empty-map? kws) inc))]
    (when max-count
      (assert (<= min-count max-count)))
    (In (r/make-CountRange min-count max-count)
        (impl/impl-case
          :clojure (RClass-of ASeq [ss])
          :cljs (Protocol-of 'cljs.core/ISeq [ss])))))

;; Unions

(t/defalias TypeCache 
  (t/Map (t/Set r/Type) r/Type))

(t/ann ^:no-check initial-Un-cache TypeCache)
(def ^:private initial-Un-cache (cache/lu-cache-factory {} :threshold 256))

(t/ann ^:no-check Un-cache (t/Atom1 TypeCache))
(defonce Un-cache (atom initial-Un-cache))

(t/ann ^:no-check reset-Un-cache [-> nil])
(defn reset-Un-cache []
  (reset! Un-cache initial-Un-cache)
  nil)

(declare flatten-intersections)

(t/ann ^:no-check Un [r/Type * -> r/Type])
(defn Un [& types]
  {:pre [(every? r/Type? types)]
   :post [(r/Type? %)]}
  (let [cache-key (set types)]
    (if-let [hit (get @Un-cache cache-key)]
      hit
      (let [res (letfn [;; a is a Type (not a union type)
                        ;; b is a Set[Type] (non overlapping, non Union-types)
                        ;; The output is a non overlapping list of non Union types.
                        (merge-type [a b]
                          {:pre [(set? b)
                                 (do (assert (r/Type? a) a)
                                     true)
                                 (not (r/Union? a))]
                           :post [(set? %)]}
                          #_(prn "merge-type" a b)
                          (let [b* (make-Union b)
                                ;_ (prn "merge-type" a b*)
                                res (cond
                                      ; don't resolve type applications in case types aren't
                                      ; fully defined yet
                                      ; TODO basic error checking, eg. number of params
                                      (some (some-fn r/Name? r/TApp?) (conj b a)) (conj b a)
                                      (ind/subtype? a b*) b
                                      (ind/subtype? b* a) #{a}
                                      :else (into #{a}
                                                  (remove #(ind/subtype? % a))
                                                  b))]
                            ;(prn "res" res)
                            res))]
                  (let [types (flatten-unions (map fully-resolve-non-rec-type types))]
                    (cond
                      (empty? types) r/empty-union
                      (= 1 (count types)) (first types)
                      :else 
                      (make-Union
                        (reduce (fn [acc t] (merge-type t acc))
                                (sorted-set-by u/type-comparator)
                                types)))))]
        (swap! Un-cache assoc cache-key res)
        res))))

;; Intersections

(declare overlap In)

(t/ann In-cache (t/Atom1 TypeCache))
(defonce In-cache (atom {}))

(t/ann intersect-cache (t/Atom1 TypeCache))
(defonce intersect-cache (atom {}))

(t/ann reset-In-cache [-> nil])
(defn reset-In-cache []
  (reset! In-cache {})
  (reset! intersect-cache {})
  nil)

(t/ann ^:no-check make-Intersection [(t/Seqable r/Type) -> r/Type])
(defn make-Intersection
  "Does not resolve types."
  [types]
  (let [ts (flatten-intersections types)]
    (case (count ts)
      0 r/-any
      1 (first ts)
      (r/Intersection-maker ts))))

(declare RClass-of)

(t/ann ^:no-check HMap-with-Value-keys? [HeterogeneousMap * -> t/Bool])
(defn HMap-with-Value-keys? [& args]
  {:pre [(every? r/HeterogeneousMap? args)]}
  (every? r/Value? 
          (apply concat 
                 (mapcat (juxt (comp keys :types)
                               (comp keys :optional)
                               :absent-keys) 
                         args))))

(t/ann ^:no-check intersect-HMap [HeterogeneousMap HeterogeneousMap -> r/Type])
(defn ^:private intersect-HMap
  [t1 t2]
  {:pre [(r/HeterogeneousMap? t1)
         (r/HeterogeneousMap? t2)
         (HMap-with-Value-keys? t1 t2)]
   :post [(r/Type? %)]}
  ; make-HMap handles duplicates
  (make-HMap :mandatory (apply merge-with In (map :types [t1 t2]))
             :optional (apply merge-with In (map :optional [t1 t2]))
             :absent-keys (apply set/union (map :absent-keys [t1 t2]))
             :complete? (not-any? :other-keys? [t1 t2])))

(defn intersect-CountRange
  [t1 t2]
  {:pre [(r/CountRange? t1)
         (r/CountRange? t2)]
   :post [(r/Type? %)]}
  ;(prn "intersect-CountRange" t1 t2)
  (let [lower (max (:lower t1)
                   (:lower t2))
        upper (if-some [upper1 (:upper t1)]
                (if-some [upper2 (:upper t2)]
                  (min upper1
                       upper2)
                  upper1)
                (:upper t2))]
    (if (and upper (< upper lower))
      bottom
      (r/make-CountRange lower upper))))

(t/ann ^:no-check intersect [r/Type r/Type -> r/Type])
(defn intersect [t1 t2]
  {:pre [(r/Type? t1)
         (r/Type? t2)
         #_(not (r/Union? t1))
         #_(not (r/Union? t2))]
   :post [(r/Type? %)]}
  (let [cache-key (hash-set t1 t2)]
    (if-let [hit (@intersect-cache cache-key)]
      hit
      (let [t (cond
                ; Unchecked is "sticky" even though it's a subtype/supertype
                ; of everything
                (or (and (r/Unchecked? t1) (not (r/Unchecked? t2)))
                    (and (not (r/Unchecked? t1)) (r/Unchecked? t2)))
                (make-Intersection [t1 t2])

                (and (r/HeterogeneousMap? t1)
                     (r/HeterogeneousMap? t2))
                (intersect-HMap t1 t2)

                ;RClass's with the same base, intersect args pairwise
                (and (r/RClass? t1)
                     (r/RClass? t2)
                     (= (:the-class t1) (:the-class t2)))
                (let [args (map intersect (:poly? t1) (:poly? t2))]
                  ; if a new arg is bottom when none of the old args are bottom,
                  ; reduce type to bottom 
                  ;; FIXME is this sound? eg., (ISeq Num) ^ (ISeq Bool) = (ISeq Nothing)
                  ;; but is not equivalent to Nothing. perhaps variance has influence here?
                  (if (some (fn [[new old1 old2]]
                              (and (not-any? #{bottom} [old1 old2])
                                   (#{bottom} new)))
                            (map vector args (:poly? t1) (:poly? t2)))
                    (Un)
                    (RClass-of (:the-class t1) args)))

                (and (r/CountRange? t1)
                     (r/CountRange? t2))
                (intersect-CountRange t1 t2)

                (not (overlap t1 t2)) bottom

                (ind/subtype? t1 t2) t1
                (ind/subtype? t2 t1) t2
                :else (do
                        #_(prn "failed to eliminate intersection" (make-Intersection [t1 t2]))
                        (make-Intersection [t1 t2])))]
        (swap! intersect-cache assoc cache-key t)
        ;(prn "intersect miss" (ind/unparse-type t))
        t))))

(t/ann ^:no-check flatten-intersections [(t/Seqable r/Type) -> (t/Set r/Type)])
(defn flatten-intersections
  "Does not resolve types."
  [types]
  {:pre [(every? r/Type? types)]
   :post [(set? %)
          (sorted? %)
          (every? r/Type? %)]}
  (loop [work types
         result (sorted-set-by u/type-comparator)]
    (if (empty? work)
      result
      (let [{intersections true non-intersections false} (group-by r/Intersection? work)]
        (recur (mapcat :types intersections)
               (into result non-intersections))))))

(t/ann ^:no-check flatten-unions [(t/Seqable r/Type) -> (t/Set r/Type)])
(defn flatten-unions
  "Does not resolve types."
  [types]
  {:pre [(every? r/Type? types)]
   :post [(set? %)
          (sorted? %)
          (every? (every-pred r/Type? (complement r/Union?)) %)]}
  (loop [work types
         result (sorted-set-by u/type-comparator)]
    (if (empty? work)
      result
      (let [{unions true non-unions false} (group-by r/Union? work)]
        (recur (mapcat :types unions)
               (into result non-unions))))))

(t/ann ^:no-check In [r/Type * -> r/Type])
(defn In [& types]
  {:pre [(every? r/Type? types)]
   :post [(r/Type? %)]}
  (let [res (let [ts (flatten-intersections (map fully-resolve-type types))]
              (cond
                ; empty intersection is Top
                (empty? ts) r/-any

                ; intersection containing Bottom is Bottom
                (contains? ts bottom) r/-nothing

                (= 1 (count ts)) (first ts)

                ; normalise (I t1 t2 (t/U t3 t4))
                ; to (t/U (I t1 t2) (I t1 t2 t3) (t/I t1 t2 t4))
                :else (let [{:keys [unions count-ranges hmaps non-unions]}
                            (group-by (fn [t]
                                        {:pre [(not (r/Intersection? t))]}
                                        (cond
                                          (r/Union? t) :unions
                                          (r/CountRange? t) :count-ranges
                                          (r/HeterogeneousMap? t) :hmaps
                                          :else :non-unions))
                                      ts)
                            non-unions (concat non-unions
                                               ;; FIXME hmm some of these can return unions... 
                                               (some->> (seq count-ranges)
                                                        (reduce intersect-CountRange)
                                                        list)
                                               (some->> (seq hmaps)
                                                        (reduce intersect-HMap)
                                                        list))
                            ;_ (prn "unions" (map ind/unparse-type unions))
                            ;_ (prn "non-unions" (map ind/unparse-type non-unions))
                            ;intersect all the non-unions to get a possibly-nil type
                            intersect-non-unions (some->> (seq non-unions)
                                                          (reduce intersect))
                            ;if we have an intersection above, use it to update each
                            ;member of the unions we're intersecting
                            flat-unions (flatten-unions unions)
                            intersect-union-ts (cond 
                                                 intersect-non-unions
                                                 (if (seq flat-unions)
                                                   (reduce (fn [acc union-m]
                                                             (conj acc (intersect intersect-non-unions union-m)))
                                                           #{} flat-unions)
                                                   #{intersect-non-unions})

                                                 :else flat-unions)
                            _ (assert (every? r/Type? intersect-union-ts)
                                      intersect-union-ts)]
                        (apply Un intersect-union-ts))))]
    res))

(declare TypeFn* instantiate-typefn abstract-many instantiate-many)

;; JS Nominal

(t/ann ^:no-check JSNominal*
  (t/IFn [t/Sym -> r/Type]
         [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Seqable Bounds) -> r/Type]))
(defn JSNominal* 
  ([name] (JSNominal* nil nil nil name nil))
  ([names variances poly? name bnds]
   {:pre [(every? symbol? names)
          (every? r/variance? variances)
          (= (count variances) (count poly?))
          (every? r/Type? poly?)
          (every? r/Bounds? bnds)
          (symbol? name)]
    :post [(r/Type? %)]}
   ;; FIXME most of these are placeholders
   (let [p (r/JSNominal-maker (seq variances) :class (seq poly?) name nil {} {})]
     (if (seq variances)
       (TypeFn* names variances bnds p)
       p))))

(declare TypeFn-fresh-symbols*)

(def ^:private get-jsnominal #((requiring-resolve 'typed.cljs.checker.jsnominal-env/get-jsnominal)
                               %))

(t/ann ^:no-check JSNominal-of (t/IFn [t/Sym -> r/Type]
                                      [t/Sym (t/Seqable r/Type) -> r/Type]))
(defn JSNominal-of
  ([sym] (JSNominal-of sym nil))
  ([sym args]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (get-jsnominal sym)]
     (assert ((some-fn r/TypeFn? r/JSNominal? nil?) p))
     ; parameterised nominals must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic JS nominal " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args)
       (r/JSNominal? p) p
       ; allow unannotated nominals if unparameterised
       :else (JSNominal* sym)))))

;Datatype

(t/ann ^:no-check DataType*
  [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Seqable Bounds) -> r/Type])
(defn DataType* [names variances poly? name bnds fields record?]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? name)]
   :post [(r/Type? %)]}
  (let [p (r/DataType-maker name (seq variances) (seq poly?) fields record?)]
    (if (seq variances)
      (TypeFn* names variances bnds p)
      p)))

(t/ann ^:no-check DataType-of (t/IFn [t/Sym -> r/Type]
                                     [t/Sym (t/Seqable r/Type) -> r/Type]))
(defn DataType-of
  ([sym] (DataType-of sym nil))
  ([sym args]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (dtenv/get-datatype sym)]
     (assert ((some-fn r/TypeFn? r/DataType? nil?) p))
     ; parameterised datatypes must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic datatype " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args)
       (r/DataType? p) p
       ; allow unannotated datatypes if unparameterised
       :else (DataType* nil nil nil sym nil {} false)))))


;; Protocol

(t/ann ^:no-check Protocol*
  [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym t/Sym (t/Map t/Sym r/Type) (t/Seqable Bounds) 
   & :optional {:declared? t/Bool} -> r/Type])
(defn Protocol* [names variances poly? the-var on-class methods bnds
                 & {:keys [declared?] :or {declared? false}}]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? the-var)
         (symbol? on-class)]
   :post [(r/Type? %)]}
  (let [p (r/Protocol-maker the-var (seq variances) (seq poly?) on-class methods declared?)]
    (if (seq variances)
      (TypeFn* names variances bnds p)
      p)))

(t/ann ^:no-check Protocol-var->on-class [t/Sym -> t/Sym])
(defn Protocol-var->on-class 
  "Given the var symbol of a protocol, returns the corresponding
  class the protocol is based on as a munged symbol."
  [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (symbol (str (munge (namespace sym)) \. (name sym))))

(t/ann ^:no-check Protocol-interface->on-var [t/Sym -> t/Sym])
(defn Protocol-interface->on-var
  "Given the interface symbol of a protocol, returns the corresponding
  var the protocol is based on as a symbol. Assumes the interface is possible
  to demunge. Only useful for Clojure implementation."
  [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (impl/assert-clojure)
  (let [segs (vec (partition-by #{\.} (str (repl/demunge (str sym)))))
        segs (assoc-in segs [(- (count segs) 2)] '(\/))
        var-sym (symbol (apply str (apply concat segs)))]
    var-sym))

(t/ann resolve-Protocol [Protocol -> (t/Var2 t/Nothing t/Any)])
(defn resolve-Protocol
  [{:keys [the-var]}]
  {:post [(var? %)]}
  (impl/assert-clojure)
  (let [v (resolve the-var)]
    (assert (var? v) (str "Cannot resolve protocol: " the-var))
    v))

(t/ann Protocol-normal-extenders [Protocol -> (t/Set (t/U nil Class))])
(defn Protocol-normal-extenders
  [p]
  (set (extenders @(resolve-Protocol p))))

(t/ann ^:no-check Protocol-of (t/IFn [t/Sym -> r/Type]
                                     [t/Sym (t/Seqable r/Type) -> r/Type]))
(defn Protocol-of
  ([sym] (Protocol-of sym nil))
  ([sym args]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (prenv/get-protocol sym)]
     (assert ((some-fn r/TypeFn? r/Protocol? nil?) p))
     ; parameterised protocols must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic Protocol " sym
                  " "
                  (if p
                    "(protocol is annotated)"
                    "(protocol is not annotated)")))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args)
       (r/Protocol? p) p
       ; allow unannotated protocols if unparameterised
       :else (r/Protocol-maker sym nil nil (Protocol-var->on-class sym) {} false)))))

;; RClass

(t/ann *current-RClass-super* (t/U nil t/Sym))
(defonce ^:dynamic *current-RClass-super* nil)

;smart constructor
(t/ann ^:no-check RClass* 
  (t/IFn [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Map t/Sym r/Type) -> r/Type]
         [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym 
          (t/Map t/Sym r/Type) (t/Set r/Type) -> r/Type]))
(defn RClass*
  ([names variances poly? the-class replacements]
   (RClass* names variances poly? the-class replacements (r/sorted-type-set [])))
  ([names variances poly? the-class replacements unchecked-ancestors]
   (RClass* names variances poly? the-class replacements unchecked-ancestors (repeat (count names) r/no-bounds)))
  ([names variances poly? the-class replacements unchecked-ancestors bnds]
   {:pre [(every? symbol? names)
          (every? r/variance? variances)
          (= (count variances) (count poly?))
          (every? r/Type? poly?)
          (every? r/Bounds? bnds)
          (symbol? the-class)]
    :post [((some-fn r/TypeFn? r/RClass?) %)]}
   (let [replacements ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/abstract-rclass-replacements)
                       the-class
                       names
                       replacements)
         unchecked-ancestors ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/abstract-rclass-ancestors)
                              the-class
                              names
                              unchecked-ancestors)]
     (if (seq variances)
       (TypeFn* names variances bnds (r/RClass-maker variances poly? the-class replacements unchecked-ancestors))
       (r/RClass-maker nil nil the-class replacements unchecked-ancestors)))))

(t/ann ^:no-check isa-DataType? [(t/U t/Sym Class) -> t/Any])
(defn isa-DataType? [sym-or-cls]
  {:pre [((some-fn symbol? class?) sym-or-cls)]}
  (let [cls (cond-> sym-or-cls
              (not (class? sym-or-cls)) coerce/symbol->Class)]
    (and (isa? cls clojure.lang.IType)
         (not= cls clojure.lang.IType))))

(t/ann ^:no-check isa-Record? [(t/U t/Sym Class) -> t/Any])
(defn isa-Record? [sym-or-cls]
  {:pre [((some-fn symbol? class?) sym-or-cls)]}
  (let [cls (cond-> sym-or-cls
              (not (class? sym-or-cls)) coerce/symbol->Class)]
    (and (isa? cls clojure.lang.IRecord)
         (not= cls clojure.lang.IRecord))))

(t/ann ^:no-check Record->HMap [DataType -> r/Type])
(defn Record->HMap [r]
  {:pre [(r/Record? r)]
   :post [(r/Type? %)]}
  (let [kf (zipmap (map (comp r/-val keyword) (keys (:fields r)))
                   (vals (:fields r)))]
    (make-HMap :mandatory kf)))

(t/ann RClass-of-cache (t/Atom1 (t/Map t/Any r/Type)))
(defonce ^:private RClass-of-cache (atom {}))

(t/ann reset-RClass-of-cache! [-> nil])
(defn reset-RClass-of-cache! []
  (reset! RClass-of-cache {})
  nil)

(t/ann ^:no-check RClass-of (t/IFn [(t/U t/Sym Class) -> r/Type]
                                   [(t/U t/Sym Class) (t/Seqable r/Type) -> r/Type]))
(defn RClass-of 
  ([sym-or-cls] (RClass-of sym-or-cls nil))
  ([sym-or-cls args]
   {:pre [((some-fn class? symbol?) sym-or-cls)
          (every? r/Type? args)]
    :post [((some-fn r/RClass? r/DataType?) %)]}
   (let [sym (cond-> sym-or-cls
               (class? sym-or-cls) coerce/Class->symbol)
         cache-key-hash [(keyword sym) args]
         cache-hit (@RClass-of-cache cache-key-hash)]
     (if cache-hit
       cache-hit
       (let [rc ((some-fn dtenv/get-datatype rcls/get-rclass) 
                 sym)
             _ (assert ((some-fn r/TypeFn? r/RClass? r/DataType? nil?) rc))
             _ (when-not (or (r/TypeFn? rc) (empty? args))
                 (err/int-error
                   (str "Cannot instantiate non-polymorphic RClass " sym
                        (when *current-RClass-super*
                          (str " when checking supertypes of RClass " *current-RClass-super*)))))
             res (cond 
                   (r/TypeFn? rc) (instantiate-typefn rc args)
                   ((some-fn r/DataType? r/RClass?) rc) rc
                   :else
                   (let [cls (coerce/symbol->Class sym)]
                     (if (isa-DataType? cls)
                       (do (println (str "WARNING: Assuming unannotated Clojure type " sym
                                         " is a datatype"))
                           (flush)
                           (when (isa-Record? cls)
                             (println (str "WARNING: " sym " is probably a record because it extends IRecord."
                                           " Annotate with ann-record above the first time it is parsed"))
                             (flush))
                         (r/DataType-maker sym nil nil (array-map) (isa-Record? cls)))
                       (r/RClass-maker nil nil sym {} (r/sorted-type-set #{})))))]
         (swap! RClass-of-cache assoc cache-key-hash res)
         res)))))

(t/ann ^:no-check most-general-on-variance [(t/Seqable r/Variance) (t/Seqable Bounds) -> r/Type])
(defn most-general-on-variance [variances bnds]
  (doall
    (for [[variance {:keys [upper-bound lower-bound] :as bnd}] 
          (map vector variances bnds)]
      (case variance
        (:invariant :constant :covariant) upper-bound
        :contravariant lower-bound))))

(declare TypeFn-bbnds* TypeFn-fresh-symbols*)

;FIXME rename to RClass-with-unknown-params
(t/ann ^:no-check RClass-of-with-unknown-params [(t/U t/Sym Class) & :optional {:warn-msg (t/U nil t/Str)} -> r/Type])
(defn RClass-of-with-unknown-params
  ([sym-or-cls & {:keys [warn-msg]}]
   {:pre [((some-fn class? symbol?) sym-or-cls)]
    :post [((some-fn r/RClass? r/DataType?) %)]}
   (let [sym (cond-> sym-or-cls
               (class? sym-or-cls) coerce/Class->symbol)
         rc ((some-fn dtenv/get-datatype rcls/get-rclass) sym)
         args (when (r/TypeFn? rc)
                (when warn-msg
                  (println "WARNING: " warn-msg ": " sym))
                (let [syms (TypeFn-fresh-symbols* rc)]
                  (most-general-on-variance (:variances rc)
                                            (TypeFn-bbnds* syms rc))))]
     (RClass-of sym args))))

(t/ann ^:no-check DataType-with-unknown-params [t/Sym -> r/Type])
(defn DataType-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/DataType?) %)]}
   (let [t (dtenv/get-datatype sym)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t))))]
     (DataType-of sym args))))

(t/ann ^:no-check JSNominal-with-unknown-params [t/Sym -> r/Type])
(defn JSNominal-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/JSNominal?) %)]}
   (let [t (get-jsnominal sym)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t))))]
     (JSNominal-of sym args))))

(t/ann ^:no-check JSNominal-method* [JSNominal t/Sym -> r/Type])
(defn JSNominal-method*
  [{:keys [name poly?] :as jsnom} msym]
  {:pre [(r/JSNominal? jsnom)
         (symbol? msym)]
   :post [(r/Type? %)]}
  (if-let [t ((requiring-resolve 'typed.cljs.checker.jsnominal-env/get-method) name poly? msym)]
    t
    (assert nil (str "JS nominal type " name " does not have method " msym))))

(t/ann ^:no-check JSNominal-field* [JSNominal t/Sym -> r/Type])
(defn JSNominal-field*
  [{:keys [name poly?] :as jsnom} fsym]
  {:pre [(r/JSNominal? jsnom)
         (symbol? fsym)]
   :post [(r/Type? %)]}
  (if-let [t ((requiring-resolve 'typed.cljs.checker.jsnominal-env/get-field) name poly? fsym)]
    t
    (assert nil (str "JS nominal type " name " does not have field " fsym))))

(t/ann ^:no-check JSNominal-ctor* [JSNominal -> r/Type])
(defn JSNominal-ctor*
  [{:keys [name poly?] :as jsnom}]
  {:pre [(r/JSNominal? jsnom)]
   :post [(r/Type? %)]}
  (if-let [t ((requiring-resolve 'typed.cljs.checker.jsnominal-env/get-ctor) name poly?)]
    t
    (assert nil (str "JS nominal type " name " does not have a constructor."))))

(t/ann ^:no-check Protocol-with-unknown-params [t/Sym -> r/Type])
(defn Protocol-with-unknown-params
  ([sym]
   {:pre [(symbol? sym)]
    :post [((some-fn r/Protocol?) %)]}
   (let [t (prenv/get-protocol sym)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t))))]
     (Protocol-of sym args))))

(t/tc-ignore
(defn- infer-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.cs-gen) 'infer)]
    (assert (var? v) "infer unbound")
    v))
  )

(t/tc-ignore
(defn- subst-all-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.subst) 'subst-all)]
    (assert (var? v) "subst-all unbound")
    v))
  )

(declare make-simple-substitution)

(t/ann ^:no-check inst-and-subst [(t/U r/Type Scope) (t/Seqable r/Type) -> r/Type])
(defn inst-and-subst 
  "Instantiate target type with ts number of
  free names. Target must be wrapped in ts number
  of Scopes. Substitutes the temporary names with
  types ts."
  [target ts]
  {:pre [((some-fn r/Type? r/Scope?) target)
         (every? r/Type? ts)]
   :post [(r/Type? %)]}
  (let [subst-all @(subst-all-var)
        ; these names are eliminated immediately, they don't need to be
        ; created with fresh-symbol
        names (repeatedly (count ts) gensym)
        ;_ (prn "inst-and-subst" names)
        fs (map r/make-F names)
        t (instantiate-many names target)
        _ (assert (r/Type? t))
        subst (make-simple-substitution names ts)]
    (subst-all subst t)))

(t/ann ^:no-check RClass-replacements* [RClass -> (t/Map t/Sym r/Type)])
(defn RClass-replacements*
  "Return the replacements map for the RClass"
  [rcls]
  {:pre [(r/RClass? rcls)]
   :post [((con/hash-c? symbol? r/Type?) %)]}
  ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/rclass-replacements) rcls))

(t/ann ^:no-check RClass-unchecked-ancestors* [RClass -> (t/SortedSet r/Type)])
(defn RClass-unchecked-ancestors*
  [rcls]
  {:pre [(r/RClass? rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/rclass-ancestors) rcls))

(t/ann ^:no-check supers-cache (t/Atom1 (t/Map Number (t/SortedSet r/Type))))
(defonce ^:private supers-cache (atom {}
                                      #_#_
                                      :validator (con/hash-c? r/RClass?
                                                              (con/sorted-set-c? r/Type?))))

(t/ann reset-supers-cache! [-> nil])
(defn reset-supers-cache! []
  (reset! supers-cache {})
  nil)

;TODO won't type check because records+destructuring
(t/ann ^:no-check RClass-supers* [RClass -> (t/SortedSet r/Type)])
(defn RClass-supers* 
  "Return a set of ancestors to the RClass"
  [{:keys [the-class] :as rcls}]
  {:pre [(r/RClass? rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  (let [cache-key rcls
        cache-hit (@supers-cache cache-key)]
    (if cache-hit
      cache-hit
      (let [unchecked-ancestors (RClass-unchecked-ancestors* rcls)
            ;_ (prn "unchecked-ancestors" unchecked-ancestors)
            replacements (RClass-replacements* rcls)
            ;_ (prn "replacements" (map ind/unparse-type (vals replacements)))
            ;set of symbols of Classes we haven't explicitly replaced
            java-supers (into #{} (map coerce/Class->symbol) (-> the-class coerce/symbol->Class supers))
            replace-keys (set (keys replacements))
            not-replaced (set/difference java-supers
                                         replace-keys)
            ;(prn "not-replaced" not-replaced)
            bad-replacements (set/difference replace-keys
                                             java-supers)
            _ (when (seq bad-replacements)
                (err/int-error (str "Bad RClass replacements for " the-class ": " bad-replacements)))
            res (r/sorted-type-set
                  (set/union (binding [*current-RClass-super* the-class]
                               (let [rs (for [csym not-replaced]
                                          (RClass-of-with-unknown-params
                                            csym
                                            :warn-msg (when (.contains (str the-class) "clojure.lang")
                                                        (str "RClass ancestor for " the-class " defaulting "
                                                             "to most general parameters"))))]
                                 (apply set/union (set rs) (map (t/fn [r :- r/Type]
                                                                  {:pre [(r/RClass? r)]}
                                                                  (RClass-supers* r))
                                                                rs))))
                             (set (vals replacements))
                             #{(RClass-of Object)}
                             unchecked-ancestors))]
        ;(prn "supers" the-class res)
        (when-not (<= (count (filter (some-fn r/FnIntersection? r/Poly? r/PolyDots?) res))
                      1)
          (err/int-error 
            (str "Found more than one function supertype for RClass " (ind/unparse-type rcls) ": \n"
                 (mapv ind/unparse-type (filter (some-fn r/FnIntersection? r/Poly? r/PolyDots?) res))
                 "\nReplacements:" (into {}
                                         (map (t/fn [[k v] :- '[t/Any r/Type]] [k (ind/unparse-type v)]))
                                         replacements)
                 "\nNot replaced:" not-replaced
                 (try (throw (Exception. ""))
                      (catch Exception e
                        (with-out-str (repl/pst e 40)))))))
        (t/tc-ignore
          (swap! supers-cache assoc cache-key res))
        res))))

(t/ann ^:no-check DataType-fields* [DataType -> (t/Map t/Sym r/Type)])
(defn DataType-fields* [^DataType dt]
  {:pre [(r/DataType? dt)]
   :post [((con/array-map-c? symbol? r/Type?) %)]}
  (:fields dt))

;; TypeFn

;smart constructor
(t/ann ^:no-check TypeFn* 
       [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable Bounds) r/Type
        (t/HMap :optional {:meta (t/U nil (t/Map t/Any t/Any))}) :? -> r/Type])
(defn TypeFn* 
  ([names variances bbnds body] (TypeFn* names variances bbnds body {}))
  ([names variances bbnds body {:keys [meta] :as opt}]
  {:pre [(every? symbol names)
         (every? r/variance? variances)
         (every? r/Bounds? bbnds)
         (apply = (map count [names variances bbnds]))
         ((some-fn r/TypeFn? r/Type?) body)
         (map? opt)
         ((some-fn nil? map?) meta)]
   :post [(r/Type? %)]}
  (let [original-names (mapv (comp r/F-original-name r/make-F) names)]
    (if (empty? names)
      body
      (let [t (r/TypeFn-maker (count names)
                              variances
                              (mapv (fn [bnd] (r/visit-bounds bnd #(abstract-many names %)))
                                    bbnds)
                              (abstract-many names body)
                              meta)]
        (with-original-names t original-names))))))

;only set to true if throwing an error and need to print a TypeFn
(t/ann *TypeFn-variance-check* t/Bool)
(def ^:dynamic *TypeFn-variance-check* true)

;smart destructor
(t/ann ^:no-check TypeFn-body* [(t/Seqable t/Sym) TypeFn -> r/Type])
(defn TypeFn-body* [names typefn]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]}
  (assert (= (:nbound typefn) (count names)) "Wrong number of names")
  (let [bbnds (TypeFn-bbnds* names typefn)
        body (free-ops/with-bounded-frees
               (zipmap (map r/make-F names) bbnds)
               (instantiate-many names (:scope typefn)))
        vs (free-ops/with-bounded-frees 
             (zipmap (map r/make-F names) bbnds)
             ; We don't check variances are consistent at parse-time. Instead
             ; we check at instantiation time. This avoids some implementation headaches,
             ; like dealing with partially defined types.
             ((requiring-resolve 'typed.cljc.checker.frees/fv-variances) body))
        _ (when *TypeFn-variance-check*
            (doseq [[nme variance] (map vector names (:variances typefn))]
              (when-let [actual-v (vs nme)]
                (when-not (= (vs nme) variance)
                  (binding [vs/*current-env* (or (some-> typefn meta :env)
                                                 vs/*current-env*)]
                    (err/int-error (str "Type variable " (-> nme r/make-F r/F-original-name) 
                                        " appears in " (name actual-v) " position "
                                        "when declared " (name variance)
                                        ", in " (binding [*TypeFn-variance-check* false]
                                                  (ind/unparse-type typefn)))))))))]
    body))

(t/ann ^:no-check TypeFn-bbnds* [(t/Seqable t/Sym) TypeFn -> (t/Seqable Bounds)])
(defn TypeFn-bbnds* [names typefn]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]
   :post [(every? r/Bounds? %)]}
  (assert (= (:nbound typefn) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (:bbnds typefn)))

(t/ann ^:no-check TypeFn-free-names* [TypeFn -> (t/Seqable t/Sym)])
(defn ^:private TypeFn-free-names* [tfn]
  {:pre [(r/TypeFn? tfn)]
   :post [((some-fn nil? 
                    (every-pred seq (con/every-c? symbol?))) 
           %)]}
  (get-original-names tfn))

(t/ann ^:no-check TypeFn-fresh-symbols* [TypeFn -> (t/Seqable t/Sym)])
(defn TypeFn-fresh-symbols* [tfn]
  {:pre [(r/TypeFn? tfn)]
   :post [((every-pred seq (con/every-c? symbol?)) %)]}
  (map fresh-symbol (or (TypeFn-free-names* tfn)
                        (repeatedly (:nbound tfn) #(gensym "fresh-sym")))))

;; Poly

;smart constructor
;;
;; Corresponds to closing a type in locally nameless representation
;; (turns free `names` into bound De Bruijn vars)
;; Also keeps track of the original name in a table to recover names
;; for debugging or to correlate with surface syntax
;;
;; Provide #:original-names if the names that you are closing off
;; are *different* from the names you want recorded in the table.
;;
(t/ann ^:no-check Poly* [(t/Seqable t/Sym) (t/Seqable Bounds) r/Type
                         & :optional {:original-names (t/Seqable t/Sym)
                                      :named (t/U nil (t/Map t/Sym t/Int))}
                         -> r/Type])
(defn Poly* [names bbnds body & {:keys [original-names named]
                                 :or {original-names 
                                      (map (comp r/F-original-name r/make-F) names)}}]
  {:pre [(every? simple-symbol? names)
         (every? r/Bounds? bbnds)
         (r/Type? body)
         (every? simple-symbol? original-names)
         (apply = (map count [names bbnds original-names]))
         ((some-fn nil? map?) named)]}
  (if (empty? names)
    body
    (let [v (r/Poly-maker (count names)
                          (mapv (fn [bnd]
                                  (r/visit-bounds bnd #(abstract-many names %)))
                                bbnds)
                          (abstract-many names body)
                          (or named {}))]
      (with-original-names v original-names))))

(t/ann ^:no-check Poly-free-names* [Poly -> (t/Seqable t/Sym)])
(defn Poly-free-names* [poly]
  {:pre [(r/Poly? poly)]
   :post [((some-fn nil? 
                    (every-pred seq (con/every-c? symbol?)))
           %)]}
  (get-original-names poly))

(t/ann ^:no-check Poly-fresh-symbols* [Poly -> (t/Seqable t/Sym)])
(defn Poly-fresh-symbols* [poly]
  {:pre [(r/Poly? poly)]
   :post [((every-pred seq (con/every-c? symbol?)) %)]}
  ;(prn "Poly-fresh-symbols*" (:scope poly))
  (map fresh-symbol (or (Poly-free-names* poly)
                        ;(assert nil "no poly free names")
                        (repeatedly (:nbound poly) #(gensym "Poly-fresh-sym")))))

(t/ann ^:no-check Poly-bbnds* [(t/Seqable t/Sym) Poly -> (t/Seqable Bounds)])
(defn Poly-bbnds* [names poly]
  {:pre [(every? symbol? names)
         (r/Poly? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (:bbnds poly)))

;smart destructor
(t/ann ^:no-check Poly-body* [(t/Seqable t/Sym) Poly -> r/Type])
(defn Poly-body* [names poly]
  {:pre [(every? symbol? names)
         (r/Poly? poly)]}
  (let [bbnds (Poly-bbnds* names poly)]
    (assert (= (:nbound poly) (count names)) "Wrong number of names")
    (free-ops/with-bounded-frees
      (zipmap (map r/make-F names) bbnds)
      (instantiate-many names (:scope poly)))))

;; PolyDots

;smart constructor
(t/ann ^:no-check PolyDots* [(t/Seqable t/Sym) (t/Seqable Bounds) r/Type 
                             & :optional {:original-names (t/Seqable t/Sym)
                                          :named (t/U nil (t/Map t/Sym t/Int))}
                             -> r/Type])
(defn PolyDots* [names bbnds body & {:keys [original-names named] 
                                     :or {original-names (map (comp r/F-original-name r/make-F) names)}}]
  {:pre [(every? symbol names)
         (every? r/Bounds? bbnds)
         (r/Type? body)
         ((some-fn nil? map?) named)]}
  (assert (= (count names) (count bbnds)) "Wrong number of names")
  (if (empty? names)
    body
    (let [v (r/PolyDots-maker (count names) 
                              (mapv (fn [bnd] 
                                      (r/visit-bounds bnd #(abstract-many names %)))
                                    bbnds)
                              (abstract-many names body)
                              (or named {}))]
      (with-original-names v original-names))))

;smart destructor
(t/ann ^:no-check PolyDots-body* [(t/Seqable t/Sym) PolyDots -> r/Type])
(defn PolyDots-body* [names ^PolyDots poly]
  {:pre [(every? symbol? names)
         (r/PolyDots? poly)]}
  (assert (= (.nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (.scope poly)))

(t/ann ^:no-check PolyDots-bbnds* [(t/Seqable t/Sym) PolyDots -> (t/Seqable Bounds)])
(defn PolyDots-bbnds* [names poly]
  {:pre [(every? symbol? names)
         (r/PolyDots? poly)]
   :post [(vector? %)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (mapv (fn [b]
          (r/visit-bounds b #(instantiate-many names %)))
        (:bbnds poly)))

(t/ann ^:no-check PolyDots-free-names* [Poly -> (t/Seqable t/Sym)])
(defn ^:private PolyDots-free-names* [poly]
  {:pre [(r/PolyDots? poly)]
   :post [((some-fn nil? 
                    (every-pred seq (con/every-c? symbol?))) 
           %)]}
  (get-original-names poly))

(t/ann ^:no-check PolyDots-fresh-symbols* [PolyDots -> (t/Seqable t/Sym)])
(defn PolyDots-fresh-symbols* [poly]
  {:pre [(r/PolyDots? poly)]
   :post [((every-pred seq (con/every-c? symbol?)) %)]}
  (map fresh-symbol (or (PolyDots-free-names* poly)
                        (repeatedly (:nbound poly) gensym))))

;; Instantiate ops

(t/ann ^:no-check make-simple-substitution [(t/Seqable t/Sym) (t/Seqable r/Type) -> crep/SubstMap])
(defn make-simple-substitution [vs ts]
  {:pre [(every? symbol? vs)
         (every? r/Type? ts)
         (= (count vs)
            (count ts))]}
  (into {}
        (map (fn [[v t]]
               [v (crep/t-subst-maker t r/no-bounds)]))
        (map vector vs ts)))

(t/ann ^:no-check instantiate-typefn [TypeFn (t/Seqable r/Type) -> r/Type])
(defn instantiate-typefn [t types & {:keys [names]
                                     :or {names (TypeFn-fresh-symbols* t)}}]
  (let [subst-all @(subst-all-var)]
    (when-not (r/TypeFn? t) (err/int-error (str "instantiate-typefn requires a TypeFn: " (ind/unparse-type t))))
    (do (when-not (= (:nbound t) (count types)) 
          (err/int-error
            (str "Wrong number of arguments passed to type function. Expected "
                 (:nbound t) ", actual " (count types) ": "
                 (ind/unparse-type t) " " (mapv ind/unparse-type types))))
        (let [bbnds (TypeFn-bbnds* names t)
              body (TypeFn-body* names t)]
          ;(prn "subst" names (map meta names))
          (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)
            (subst-all (make-simple-substitution names types) body))))))

(t/ann ^:no-check instantiate-poly [Poly (t/Seqable r/Type) -> r/Type])
(defn instantiate-poly [t types]
  (let [subst-all @(subst-all-var)]
    (cond
      (r/Poly? t) (let [_ (when-not (= (:nbound t) (count types)) 
                            (err/int-error 
                              (str "Wrong number of types (" (count types) 
                                   ") used to instantiate polymorphic type: "
                                   (ind/unparse-type t)
                                   (when-some [current-RClass-super *current-RClass-super*]
                                     (str " when checking ancestors of " current-RClass-super)))))
                        nms (Poly-fresh-symbols* t)
                        bbnds (Poly-bbnds* nms t)
                        body (Poly-body* nms t)]
                    (free-ops/with-bounded-frees
                      (zipmap (map r/make-F nms) bbnds)
                      (subst-all (make-simple-substitution nms types) body)))
      ;PolyDots NYI
      :else (err/nyi-error (str "instantiate-poly: requires Poly, and PolyDots NYI")))))

;; Resolve

(declare resolve-tapp* resolve-app*)

(t/ann ^:no-check resolve-TApp [TApp -> r/Type])
(defn resolve-TApp [app]
  {:pre [(r/TApp? app)]
   :post [(r/Type? %)]}
  (resolve-tapp* (:rator app) (:rands app) :tapp app))

(t/ann ^:no-check resolve-tapp* [r/Type (t/Seqable r/Type) -> r/Type])
(defn resolve-tapp* [rator rands & {:keys [tapp]}]
  {:pre [(r/TApp? tapp)]}
  (let [rator (fully-resolve-type rator)
        _ (when-not (r/TypeFn? rator) 
            (err/int-error (str "First argument to TApp must be TFn, actual: " (ind/unparse-type rator))))]
    (when-not (= (count rands) (:nbound rator))
      (binding [vs/*current-env* (-> tapp meta :env)] ;must override env, or clear it
        (err/int-error (str "Wrong number of arguments (" (count rands) ") passed to type function: "
                          (ind/unparse-type tapp) 
                          (when-let [syn (-> tapp meta :syn)]
                            (str " in " (pr-str syn)))))))
    (instantiate-typefn rator rands)))

(t/ann ^:no-check resolve-App [App -> r/Type])
(defn resolve-App [app]
  {:pre [(r/App? app)]}
  (resolve-app* (:rator app) (:rands app)))

(t/ann ^:no-check resolve-app* [r/Type (t/Seqable r/Type) -> r/Type])
(defn resolve-app* [rator rands]
  (let [rator (fully-resolve-type rator)]
    (cond
      (r/Poly? rator) (do (when-not (= (count rands) (.nbound ^Poly rator))
                            (err/int-error (str "Wrong number of arguments provided to polymorphic type"
                                              (ind/unparse-type rator))))
                          (instantiate-poly rator rands))
      ;PolyDots NYI
      :else (throw (Exception. (str (when vs/*current-env*
                                      (str (:line vs/*current-env*) ": "))
                                    "Cannot apply non-polymorphic type " (ind/unparse-type rator)))))))

(declare resolve-Name unfold fully-resolve-type find-val-type)

(t/ann ^:no-check resolve-Get [GetType -> r/Type])
(defn resolve-Get [{:keys [target key not-found] :as t}]
  {:pre [(r/GetType? t)]
   :post [(r/Type? %)]}
  (find-val-type target key not-found))

(t/ann ^:no-check resolve-Merge [MergeType -> r/Type])
(defn resolve-Merge [{:keys [types] :as t}]
  {:pre [(r/MergeType? t)]
   :post [(r/Type? %)]}
  (or (apply (requiring-resolve 'typed.clj.checker.assoc-utils/merge-types) (map r/ret types))
      (println (str "WARNING: t/Merge resolved to t/Any"))
      r/-any))

(t/ann ^:no-check resolve-TypeOf [TypeOf -> r/Type])
(defn resolve-TypeOf [{:keys [vsym] :as t}]
  {:pre [(r/TypeOf? t)]
   :post [(r/Type? %)]}
  (or (ind/type-of-nofail vsym)
      (err/int-error (str "Could not resolve TypeOf " vsym))))

(t/ann -resolve [r/Type -> r/Type])
(defn -resolve [ty]
  {:pre [(r/AnyType? ty)]
   :post [(r/AnyType? %)]}
  (cond 
    (r/Name? ty) (resolve-Name ty)
    (r/Mu? ty) (unfold ty)
    (r/App? ty) (resolve-App ty)
    (r/TApp? ty) (resolve-TApp ty)
    (r/GetType? ty) (resolve-Get ty)
    (r/MergeType? ty) (resolve-Merge ty)
    (r/TypeOf? ty) (resolve-TypeOf ty)
    :else ty))

(defn Get-requires-resolving? [ty]
  {:pre [(r/GetType? ty)]}
  (not (r/F? (fully-resolve-type (:target ty)))))

(defn Merge-requires-resolving? [ty]
  {:pre [(r/MergeType? ty)]}
  (not-any? (comp r/F? fully-resolve-type) (:types ty)))

(t/ann requires-resolving? [r/Type -> t/Any])
(defn requires-resolving? [ty]
  {:pre [(r/AnyType? ty)]}
  (or (r/Name? ty)
      (r/App? ty)
      (and (r/TApp? ty)
           (not (r/F? (fully-resolve-type (:rator ty)))))
      (and (r/GetType? ty)
           (Get-requires-resolving? ty))
      (and (r/MergeType? ty)
           (Merge-requires-resolving? ty))
      (r/Mu? ty)
      (r/TypeOf? ty)))

(t/ann ^:no-check resolve-Name [Name -> r/Type])
(defn resolve-Name [nme]
  {:pre [(r/Name? nme)]
   :post [(r/Type? %)]}
  (let [resolve-name* (requiring-resolve 'typed.cljc.checker.name-env/resolve-name*)]
    (resolve-name* (:id nme))))

(t/ann fully-resolve-type 
       (t/IFn [r/Type -> r/Type]
              [r/Type (t/Set r/Type) -> r/Type]))
(defn fully-resolve-type 
  ([t seen]
   (let [_ (assert (not (seen t)) "Infinite non-Rec type detected")
         seen (conj seen t)]
     (if (requires-resolving? t)
       (recur (-resolve t) seen)
       t)))
  ([t] (fully-resolve-type t #{})))

(t/ann fully-resolve-non-rec-type 
       (t/IFn [r/Type -> r/Type]
           [r/Type (t/Set r/Type) -> r/Type]))
(defn fully-resolve-non-rec-type 
  ([t seen]
   (let [_ (assert (not (seen t)) "Infinite non-Rec type detected")
         seen (conj seen t)]
     (if (and (not (r/Mu? t))
              (requires-resolving? t))
       (recur (-resolve t) seen)
       t)))
  ([t] (fully-resolve-non-rec-type t #{})))

;; Mu

(declare abstract instantiate)

;smart constructor
(t/ann Mu* [t/Sym r/Type -> r/Type])
(defn Mu* [name body]
  (let [original-name (-> name r/make-F r/F-original-name)
        v (r/Mu-maker (abstract name body))]
    (with-original-names v original-name)))

;smart destructor
(t/ann Mu-body* [t/Sym Mu -> r/Type])
(defn Mu-body* [name t]
  {:pre [(r/Mu? t)
         (symbol? name)]}
  (instantiate name (p/mu-scope t)))

(t/ann ^:no-check Mu-free-name* [Mu -> (t/U nil t/Sym)])
(defn Mu-free-name* [t]
  {:pre [(r/Mu? t)]
   :post [((some-fn symbol? nil?) %)]}
  (get-original-names t))

(t/ann ^:no-check Mu-fresh-symbol* [Mu -> t/Sym])
(defn Mu-fresh-symbol* [t]
  {:pre [(r/Mu? t)]
   :post [(symbol? %)]}
  (let [s (or (Mu-free-name* t)
              (gensym))]
    (fresh-symbol s)))

(t/tc-ignore
(defn- substitute-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.subst) 'substitute)]
    (assert (var? v) "substitute unbound")
    v))
  )

(t/ann ^:no-check unfold-Mu-with [Mu r/Type -> r/Type])
(defn unfold-Mu-with [t tsubst]
  {:pre [(r/Mu? t)]
   :post [(r/Type? %)]}
  (let [substitute @(substitute-var)
        sym (Mu-fresh-symbol* t)
        body (Mu-body* sym t)]
    (substitute tsubst sym body)))

(t/ann ^:no-check unfold [Mu -> r/Type])
(defn unfold [t]
  {:pre [(r/Mu? t)]
   :post [(r/Type? %)]}
  (unfold-Mu-with t t))

;; Utils

(t/ann Value->Class [Value -> (t/Option Class)])
(defn ^Class Value->Class [tval]
  {:post [(t/tc-ignore
            (or (class? %)
                (nil? %)))]}
  ;; workaround bug with paths
  ((-> class (t/ann-form [t/Any :-> (t/Option Class)]))
   (:val tval)))

(t/ann keyword-value? [t/Any -> t/Any])
(defn keyword-value? [val]
  (boolean
    (when (r/Value? val)
      (keyword? (:val val)))))

(t/ann number-value? [t/Any -> t/Any])
(defn number-value? [val]
  (boolean
    (when (r/Value? val)
      (number? (:val val)))))

;; Overlap

;; FIXME much better algorithms around I'm sure
(t/ann ^:no-check countrange-overlap? [CountRange CountRange -> t/Any])
(defn countrange-overlap? 
  [{lowerl :lower upperl :upper :as l}
   {lowerr :lower upperr :upper :as r}]
  {:pre [(r/CountRange? l)
         (r/CountRange? r)]}
  (cond 
    (and upperl upperr)
        (or 
          ;; -----
          ;;   -------
          ;; and
          ;;   ---
          ;;   -------
          (<= lowerl lowerr upperl upperr)

          ;;    --
          ;;   -------
          (<= lowerr lowerl upperl upperr)

          ;;     ------
          ;; -------
          ;; and
          ;;     ---
          ;; -------
          (<= lowerr lowerl upperr upperl)

          ;; otherwise no overlap
          false)

    upperl ;; and (not upperr)
      (or 
        ;; ----
        ;;  ----->>
        ;; and
        ;;  ---
        ;;  ----->>
        (<= lowerl lowerr upperl)
        ;;   ---
        ;;  ----->>
        (<= lowerr lowerl)
        ;; otherwise no overlap
        false)
    upperr
      (or
        ;; ------>>
        ;;  ----
        ;; and
        ;;  ----->>
        ;;  ---
        (<= lowerl lowerr)
        
        ;;   --->>
        ;; ----
        (<= lowerr lowerl upperr)

        ;; else no overlap
        false)
    :else ;; (and (not upperl) (not upperr))
    ;; ---->>
    ;;   -->>
    ;; and
    ;;   -->>
    ;; ---->>
    true))

(def ^:dynamic *overlap-seen* #{})

(defn overlap-CountRange-KwArgsSeq?
  [A {:keys [lower upper] :as cr} kws]
  {:pre [(r/CountRange? cr)
         (r/KwArgsSeq? kws)]
   :post [(boolean? %)]}
  ;(prn "overlap-CountRange-KwArgsSeq?" kws cr)
  (if (not (overlap (upcast-kw-args-seq kws) cr))
    false
    ;; seq has even numbered count without trailing map
    (or (-> kws :kw-args-regex :maybe-trailing-nilable-non-empty-map? boolean)
        (not= lower upper)
        (let [exact-count lower]
          ;; an odd exact count without a trailing map derives a contradiction
          (even? exact-count)))))

;true if types t1 and t2 overlap (NYI)
(t/ann ^:no-check overlap [r/Type r/Type -> t/Any])
(defn overlap 
  ([t1 t2] (overlap *overlap-seen* t1 t2))
  ([A t1 t2]
  (if (contains? A [t1 t2])
    true
    (let [A* (conj A [t1 t2])
          overlap #(overlap A* %1 %2)
          ;; handle mutual recursion between subtyping and overlap
          subtype? #(binding [*overlap-seen* A*]
                      (ind/subtype? %1 %2))

          t1 (fully-resolve-type t1)
          t2 (fully-resolve-type t2)
          eq (= t1 t2)
          hmap-and-seq? (fn [h s] (and (r/HeterogeneousMap? h)
                                       (impl/impl-case
                                         :clojure (and (r/RClass? s)
                                                       ('#{clojure.lang.ISeq} (:the-class s)))
                                         :cljs (and (r/Protocol? s)
                                                    ('#{cljs.core/ISeq} (:the-var s))))))
          hvec-and-seq? (fn [h s] (and (r/HeterogeneousVector? h)
                                       (impl/impl-case
                                         :clojure (and (r/RClass? s)
                                                       ('#{clojure.lang.ISeq} (:the-class s)))
                                         :cljs (and (r/Protocol? s)
                                                    ('#{cljs.core/ISeq} (:the-var s))))))
          record-and-iseq? (fn [r s]
                             (and (r/Record? r)
                                  (ind/subtype? s (impl/impl-case
                                                    :clojure (RClass-of clojure.lang.ISeq [r/-any])
                                                    :cljs (Protocol-of 'cljs.core/ISeq [r/-any])))))]
      (cond 
        eq eq

        (or (r/F? t1)
            (r/F? t2)
            (r/B? t1)
            (r/B? t2))
        true

        (and (r/Value? t1)
             (r/Value? t2))
        eq

        (r/Union? t1)
        (boolean 
          (some #(overlap % t2) (:types t1)))

        (r/Union? t2)
        (boolean 
          (some #(overlap t1 %) (:types t2)))

        (r/Intersection? t1)
        (every? #(overlap % t2) (:types t1))

        (r/Intersection? t2)
        (every? #(overlap t1 %) (:types t2))

        (and (r/NotType? t1)
             (r/NotType? t2))
        ;FIXME what if both are Not's?
        true

        ; eg. (overlap (Not Number) Integer) => false
        ;     (overlap (Not Integer) Number) => true
        ;     (overlap (Not y) x) => true
        (r/NotType? t1)
        (let [neg-type (fully-resolve-type (:type t1))]
          (or (some (some-fn r/B? r/F?) [neg-type t2])
              (not (overlap neg-type t2))))

        (r/NotType? t2)
        ;switch arguments to catch above case
        (overlap t2 t1)

        ;if both are Classes, and at least one isn't an interface, then they must be subtypes to have overlap
        ;      (and (r/RClass? t1)
        ;           (r/RClass? t2)
        ;           (let [{t1-flags :flags} (reflect/type-reflect (r/RClass->Class t1))
        ;                 {t2-flags :flags} (reflect/type-reflect (r/RClass->Class t2))]
        ;             (some (complement :interface) [t1-flags t2-flags])))
        ;      (or (ind/subtype? t1 t2)
        ;          (ind/subtype? t2 t1))
        (and (r/RClass? t1)
             (r/RClass? t2))
        (let [_ (impl/assert-clojure)
              {t1-flags :flags} (reflect/type-reflect (r/RClass->Class t1))
              {t2-flags :flags} (reflect/type-reflect (r/RClass->Class t2))]
          ; there is only an overlap if a class could have both classes as parents
          (or (ind/subtype? t1 t2)
              (ind/subtype? t2 t1)
              ; from here they are disjoint

              (cond
                ; no potential ancestors
                (some :final [t1-flags t2-flags]) false
                ; if we have two things that are not interfaces, ie. abstract, normal
                ; classes, there is no possibility of overlap
                (every? (complement :interface) [t1-flags t2-flags]) false
                :else true)))

        (some r/Extends? [t1 t2])
        (let [[the-extends other-type] (if (r/Extends? t1)
                                         [t1 t2]
                                         [t2 t1])]
          ; returns true if at least one +ve type overlaps, and if
          ; no negative types overlap, else false
          (boolean
            (and (some (fn [pos] (overlap pos other-type)) (:extends the-extends))
                 (not-any? (fn [neg] (overlap neg other-type)) (:without the-extends)))))

        (and (impl/checking-clojurescript?)
             (or (and (r/JSNull? t1)
                      (r/JSUndefined? t2))
                 (and (r/JSNull? t2)
                      (r/JSUndefined? t1))))
        false

        ;; already rules out free variables, so this is safe.
        (or (r/Value? t1)
            (r/Value? t2))
        (or (ind/subtype? t1 t2)
            (ind/subtype? t2 t1))

        (and (r/CountRange? t1)
             (r/CountRange? t2)) 
        (countrange-overlap? t1 t2)

        (and (r/CountRange? t1)
             (r/KwArgsSeq? t2))
        (overlap-CountRange-KwArgsSeq? A t1 t2)
        (and (r/CountRange? t2)
             (r/KwArgsSeq? t1))
        (overlap-CountRange-KwArgsSeq? A t2 t1)

        (and (r/HeterogeneousMap? t1)
             (r/HeterogeneousMap? t2)) 
        (let [common-mkeys (set/intersection 
                             (set (-> t1 :types keys))
                             (set (-> t2 :types keys)))]
          (cond 
            ; if there is an intersection in the mandatory keys
            ; each entry in common should overlap
            (seq common-mkeys)
            (every? (fn [[k1 v1]]
                      (let [v2 ((:types t2) k1)]
                        (assert v2)
                        (overlap v1 v2)))
                    (select-keys (:types t1) common-mkeys))
            ;TODO more cases. incorporate completeness
            :else true))

                ;for map destructuring mexpansion
        (or (hmap-and-seq? t1 t2)
            (hmap-and-seq? t2 t1))
        false

        ;for vector destructuring mexpansion
        (or (hvec-and-seq? t1 t2)
            (hvec-and-seq? t2 t1))
        false

        ;for map destructuring of records. A record is never an ISeq
        (or (record-and-iseq? t1 t2)
            (record-and-iseq? t2 t1))
        false

;;FIXME check compatibility between HSequentials
        (and (r/HSequential? t1)
             (r/HSequential? t2))
        (let [rest-sub? (fn [t1 t2]
                          ; punt on drest
                          (and (not-any? :drest [t1 t2])
                               (or (== (count (:types t1))
                                       (count (:types t2)))
                                   (and (<= (count (:types t1))
                                            (count (:types t2)))
                                        (:rest t1)))
                               (every? identity
                                       (mapv overlap
                                             ; rest type is non-nil if needed.
                                             (u/pad-right (count (:types t2))
                                                          (:types t1)
                                                          (:rest t1))
                                             (:types t2)))
                               (if (every? :rest [t1 t2])
                                 (overlap (:rest t1) (:rest t2))
                                 true)))]
          (or (rest-sub? t1 t2)
              (rest-sub? t2 t1)))

        :else true))))) ;FIXME conservative result

; restrict t1 to be a subtype of t2
(t/ann ^:no-check restrict [r/Type r/Type -> r/Type])
(defn restrict [t1 t2]
  (let [t1 (fully-resolve-type t1)
        t2 (fully-resolve-type t2)
        subst-all @(subst-all-var)
        infer @(infer-var)]
    (cond
      (ind/subtype? t1 t2) t1 ;; already a subtype

      (not (overlap t1 t2)) (Un) ;there's no overlap, so the restriction is empty

      (r/Union? t1) (apply Un (map (fn [e] (restrict e t2)) (:types t1)))
      (r/Union? t2) (apply Un (map (fn [e] (restrict t1 e)) (:types t2)))

      (r/Poly? t2)
      (let [names (Poly-fresh-symbols* t2)
            t (Poly-body* names t2)
            bbnds (Poly-bbnds* names t2)
            subst (u/handle-cs-gen-failure
                    (infer (zipmap names bbnds) {} (list t1) (list t) t1))]
        (and subst (restrict t1 (subst-all subst t1))))

      ;TODO other cases
      :else (In t2 t1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

(t/ann ^:no-check add-scopes [t/AnyInteger r/Type -> (t/U r/Type Scope)])
(defn add-scopes 
  "Wrap type in n Scopes"
  [n t]
  {:pre [(nat-int? n)
         (r/Type? t)]
   :post [((some-fn r/Scope? r/Type?) %)]}
  (last 
    (take (inc n) (iterate r/Scope-maker t))))

(t/ann ^:no-check remove-scopes [t/AnyInteger (t/U Scope r/Type) -> (t/U Scope r/Type)])
(defn remove-scopes 
  "Unwrap n Scopes"
  [^long n sc]
  {:pre [(nat-int? n)
         (or (zero? n)
             (r/Scope? sc))]
   :post [(r/Type? %)]}
  (loop [n n
         sc sc]
    (if (zero? n)
      (do
        (assert (r/Type? sc) (str "Did not remove enough scopes" sc))
        sc)
      (do
        (assert (r/Scope? sc) (str "Tried to remove too many Scopes: " sc))
        (recur (dec n) (:body sc))))))

(t/ann ^:no-check rev-indexed (t/All [x] [(t/Seqable x) -> (t/Seqable '[t/AnyInteger x])]))
(defn- rev-indexed 
  "'(a b c) -> '([2 a] [1 b] [0 c])"
  [c]
  (map vector (iterate dec (dec (count c))) c))

(t/tc-ignore
(f/def-derived-fold IAbstractMany ^:private abstract-many* [name count outer sb name-to])

(f/add-fold-case
  IAbstractMany abstract-many*
  F
  (fn [{name* :name :as t} name count outer sb name-to]
    (if (= name name*)
      (r/B-maker (+ count outer))
      t)))

(f/add-fold-case
  IAbstractMany abstract-many*
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot] :as ty} name count outer sb name-to]
    {:pre [(#{:fixed :rest :drest :kws :prest :pdot} (:kind ty))]}
    (r/make-Function (mapv sb dom)
                     (sb rng)
                     :rest (some-> rest sb)
                     :drest (some-> drest
                                    (update :pre-type sb)
                                    (update :name #(if (= % name)
                                                     (+ count outer)
                                                     %)))
                     :kws (letfn [(abstract-kw-map [m]
                                    {:pre [(map? m)]}
                                    (update-vals m sb))]
                            (some-> kws
                              (update :mandatory abstract-kw-map)
                              (update :optional abstract-kw-map)))
                     :prest (some-> prest sb)
                     :pdot (some-> pdot
                                   (update :pre-type sb)
                                   (update :name #(if (= % name)
                                                    (+ count outer)
                                                    %))))))

(f/add-fold-case
  IAbstractMany abstract-many*
  HSequential
  (fn [ty name count outer sb name-to]
    (r/-hsequential 
      (mapv sb (:types ty))
      :filters (mapv sb (:fs ty))
      :objects (mapv sb (:objects ty))
      :rest (some-> (:rest ty) sb)
      :drest (some-> (:drest ty)
                     (update :pre-type sb)
                     (update :name #(if (= % name)
                                      (+ count outer)
                                      %)))
      :repeat (:repeat ty)
      :kind (:kind ty))))

(f/add-fold-case
  IAbstractMany abstract-many*
  AssocType
  (fn [{:keys [target entries dentries]} name count outer sb name-to]
   (r/AssocType-maker (sb target)
                      (mapv (fn [[k v]] [(sb k) (sb v)]) entries)
                      (some-> dentries
                              (update :pre-type sb)
                              (update :name #(if (= % name)
                                               (+ count outer)
                                               %))))))

(f/add-fold-case
  IAbstractMany abstract-many*
  MergeType
  (fn [{:keys [types]} name count outer sb name-to]
    (r/MergeType-maker (mapv sb types))))

(f/add-fold-case
  IAbstractMany abstract-many*
  Mu
  (fn [{:keys [scope] :as mu} name count outer sb name-to]
   (let [body (remove-scopes 1 scope)]
     (r/Mu-maker (r/Scope-maker (name-to name count (inc outer) body))
                 (meta mu)))))

(f/add-fold-case
  IAbstractMany abstract-many*
  PolyDots
  (fn [{:keys [named] bbnds* :bbnds n :nbound body* :scope :as ty} name count outer sb name-to]
   (let [rs #(remove-scopes n %)
         body (rs body*)
         bbnds (mapv #(r/visit-bounds % rs) bbnds*)
         as #(add-scopes n (name-to name count (+ n outer) %))]
     (r/PolyDots-maker n 
                       (mapv #(r/visit-bounds % as) bbnds)
                       (as body)
                       named
                       (meta ty)))))

(f/add-fold-case
  IAbstractMany abstract-many*
  Poly
  (fn [{:keys [named] bbnds* :bbnds n :nbound body* :scope :as poly} name count outer sb name-to]
   (let [rs #(remove-scopes n %)
         body (rs body*)
         bbnds (mapv #(r/visit-bounds % rs) bbnds*)
         as #(add-scopes n (name-to name count (+ n outer) %))]
     (r/Poly-maker n 
                   (mapv #(r/visit-bounds % as) bbnds)
                   (as body)
                   named
                   (meta poly)))))

(f/add-fold-case
  IAbstractMany abstract-many*
  TypeFn
  (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances] :as t}
       name count outer sb name-to]
  (let [rs #(remove-scopes n %)
        body (rs body*)
        bbnds (mapv #(r/visit-bounds % rs) bbnds*)
        as #(add-scopes n (name-to name count (+ n outer) %))]
     (r/TypeFn-maker n 
                     variances
                     (mapv #(r/visit-bounds % as) bbnds)
                     (as body)
                     (meta t)))))

(t/ann ^:no-check abstract-many [(t/Seqable t/Sym) r/Type -> (t/U r/Type Scope)])
(defn abstract-many 
  "Names Type -> Scope^n  where n is (count names)"
  [names ty]
  {:pre [(every? symbol? names)
         ((some-fn r/Type? r/TypeFn?) ty)]}
  (letfn [(name-to 
            ([name count type] (name-to name count 0 type))
            ([name count outer ty]
             (letfn [(sb 
                       ([t _info] (sb t))
                       ([t] (name-to name count outer t)))]
               (call-abstract-many*
                 ty
                 {:type-rec sb
                  :filter-rec (f/sub-f sb `call-abstract-many*)
                  :object-rec (f/sub-o sb `call-abstract-many*)
                  :name name
                  :count count
                  :outer outer
                  :sb sb
                  :name-to name-to}))))]
    (if (empty? names)
      ty
      (let [n (count names)]
        (loop [ty ty
               names names
               count (dec n)]
          (if (zero? count)
            (add-scopes n (name-to (first names) 0 ty))
            (recur (name-to (first names) count ty)
                   (next names)
                   (dec count))))))))

(f/def-derived-fold IInstantiateMany instantiate-many* [count outer image sb replace])

(f/add-fold-case
  IInstantiateMany instantiate-many*
  B
  (fn [{:keys [idx] :as t} count outer image sb replace]
    (if (= (+ count outer) idx)
      (r/F-maker image)
      t)))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot kind]} count outer image sb replace]
    {:pre [(#{:fixed :rest :drest :kws :prest :pdot} kind)]}
    (r/make-Function
      (map sb dom)
      (sb rng)
      :rest (some-> rest sb)
      :drest (some-> drest
                     (update :pre-type sb)
                     (update :name #(if (= (+ count outer) %)
                                      image
                                      %)))
      :kws (letfn [(instantiate-kw-map [m]
                     {:pre [(map? m)]}
                     (reduce-kv (fn [m k v]
                                  (assoc m k (sb v)))
                                {} m))]
             (some-> kws
               (update :mandatory instantiate-kw-map)
               (update :optional instantiate-kw-map)))
      :prest (some-> prest sb)
      :pdot (some-> pdot
                    (update :pre-type sb)
                    (update :name #(if (= (+ count outer) %)
                                     image
                                     %))))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  HSequential
  (fn [ty count outer image sb replace]
    (r/-hsequential 
      (mapv sb (:types ty))
      :filters (mapv sb (:fs ty))
      :objects (mapv sb (:objects ty))
      :rest (some-> (:rest ty) sb)
      :drest (some-> (:drest ty)
                     (update :pre-type sb)
                     (update :name #(if (= (+ count outer) %)
                                      image
                                      %)))
      :repeat (:repeat ty)
      :kind (:kind ty))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  AssocType
  (fn [{:keys [target entries dentries]} count outer image sb replace]
    (r/AssocType-maker (sb target)
                       (map (fn [[k v]] [(sb k) (sb v)]) entries)
                       (some-> dentries
                               (update :pre-type sb)
                               (update :name #(if (= (+ count outer) %)
                                                image
                                                %))))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  Mu
  (fn [{:keys [scope] :as mu} count outer image sb replace]
    (let [body (remove-scopes 1 scope)]
      (r/Mu-maker (r/Scope-maker (replace image count (inc outer) body))
                  (meta mu)))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  PolyDots
  (fn [{:keys [named] bbnds* :bbnds n :nbound body* :scope :as ty} count outer image sb replace]
    (let [rs #(remove-scopes n %)
          body (rs body*)
          bbnds (mapv #(r/visit-bounds % rs) bbnds*)
          as #(add-scopes n (replace image count (+ n outer) %))]
      (r/PolyDots-maker n 
                        (mapv #(r/visit-bounds % as) bbnds)
                        (as body)
                        named
                        (meta ty)))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  Poly
  (fn [{:keys [named] bbnds* :bbnds n :nbound body* :scope :as poly} 
       count outer image sb replace]
    (let [rs #(remove-scopes n %)
          body (rs body*)
          bbnds (mapv #(r/visit-bounds % rs) bbnds*)
          as #(add-scopes n (replace image count (+ n outer) %))]
      (r/Poly-maker n 
                    (mapv #(r/visit-bounds % as) bbnds)
                    (as body)
                    named
                    (meta poly)))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  TypeFn
  (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances] :as t} 
       count outer image sb replace]
    (let [rs #(remove-scopes n %)
          body (rs body*)
          bbnds (mapv #(r/visit-bounds % rs) bbnds*)
          as #(add-scopes n (replace image count (+ n outer) %))]
      (r/TypeFn-maker n 
                      variances
                      (mapv #(r/visit-bounds % as) bbnds)
                      (as body)
                      (meta t)))))
)

(t/ann ^:no-check instantiate-many [(t/Seqable t/Sym) p/IScope -> r/Type])
(defn instantiate-many 
  "instantiate-many : List[Symbols] Scope^n -> Type
  Instantiate de Bruijn indices in sc to frees named by
  images, preserving upper/lower bounds"
  [images sc]
  {:pre [(every? symbol? images)
         (or (r/Scope? sc)
             (empty? images))]
   :post [((some-fn r/Type? r/TypeFn?) %)]}
  (letfn [(replace 
            ([image count type] (replace image count 0 type))
            ([image count outer ty]
             (letfn [(sb 
                       ([t _info] (sb t))
                       ([t] (replace image count outer t)))]
               (let [sf (f/sub-f sb `call-instantiate-many*)]
                 (call-instantiate-many*
                   ty
                   {:type-rec sb
                    :filter-rec sf
                    :object-rec (f/sub-o sb `call-instantiate-many*)
                    :count count
                    :outer outer
                    :image image
                    :sb sb
                    :replace replace})))))]
    (if (empty? images)
      sc
      (let [n (count images)]
        (loop [ty (remove-scopes n sc)
               images images
               count (dec n)]
          (if (zero? count)
            (replace (first images) 0 ty)
            (recur (replace (first images) count ty)
                   (next images)
                   (dec count))))))))

(t/ann abstract [t/Sym r/Type -> Scope])
(defn abstract 
  "Make free name bound"
  [name ty]
  {:pre [(symbol? name)
         (r/Type? ty)]
   :post [(r/Scope? %)]}
  (abstract-many [name] ty))

(t/ann instantiate [t/Sym p/IScope -> r/Type])
(defn instantiate 
  "Instantiate bound name to free"
  [f sc]
  {:pre [(symbol? f)
         (p/IScope? sc)]}
  (instantiate-many [f] sc))

;TODO not sure why this fails to type check
;(t/All [x]
;  (t/IFn ['{kw x} -> x :object {:id 0, :path [Key]}]
;      [(t/U '{kw x} (HMap :without [(Value kw)]) nil) -> (t/U x nil) :object {:id 0, :path [Key]}]
;      [t/Any -> t/Any :object {:id 0, :path [Key]}]))
(t/ann ^:no-check keyword->Fn [t/Kw -> r/Type])
(defn keyword->Fn [kw]
  {:pre [(keyword? kw)]
   :post [(r/Type? %)]}
  (Poly* ['x]
         [r/no-bounds]
         (r/make-FnIntersection
           (r/make-Function
             [(-partial-hmap {(r/-val kw) (r/make-F 'x)})]
             (r/make-F 'x)
             :object (or/-path [(path/-kpe kw)] 0))
           (r/make-Function
             [(Un (make-HMap
                    :optional {(r/-val kw) (r/make-F 'x)})
                  r/-nil)]
             (Un r/-nil (r/make-F 'x))
             :object (or/-path [(path/-kpe kw)] 0))
           (r/make-Function
             [r/-any]
             r/-any
             :object (or/-path [(path/-kpe kw)] 0)))))

(t/ann KeywordValue->Fn [Value -> r/Type])
(defn KeywordValue->Fn [{:keys [val] :as t}]
  {:pre [(keyword-value? t)]}
  (impl/assert-clojure)
  (keyword->Fn
    ;; workaround occurrence typing failing to propagate types
    (->> val
         #_(t/cast t/Keyword))))

;; Extends

(t/tc-ignore
(defn -extends [clss & {:keys [without]}]
  (r/Extends-maker (r/sorted-type-set clss) (r/sorted-type-set without)))
  )

;;; KwArgs

(t/ann KwArgs->Type [KwArgs -> r/Type])
(defn KwArgs->Type [kws]
  {:pre [(r/KwArgs? kws)]
   :post [(r/Type? %)]}
  (impl/assert-clojure)
  (let [nilable-non-empty? (empty? (:mandatory kws))]
    (cond-> (r/-kw-args-seq :mandatory (:mandatory kws)
                            :optional (:optional kws)
                            :complete? false)
      nilable-non-empty? (In (r/make-CountRange 1))
      nilable-non-empty? (Un r/-nil))))

(t/ann KwArgs->HMap [KwArgs -> r/Type])
(defn KwArgs->HMap [kws]
  {:pre [(r/KwArgs? kws)]
   :post [(r/Type? %)]}
  (make-HMap :mandatory (:mandatory kws) 
             :optional (:optional kws)
             :complete? (:complete? kws)))

(t/ann KwArgsSeq->HMap [KwArgsSeq -> r/Type])
(defn KwArgsSeq->HMap [kws]
  {:pre [(r/KwArgsSeq? kws)]
   :post [(r/Type? %)]}
  (KwArgs->HMap (:kw-args-regex kws)))

(t/ann KwArgsArray->HMap [KwArgsArray -> r/Type])
(defn KwArgsArray->HMap [kws]
  {:pre [(r/KwArgsArray? kws)]
   :post [(r/Type? %)]}
  (KwArgs->HMap (:kw-args-regex kws)))

(t/ann KwArgsSeq->KwArgsArray [KwArgsSeq :-> KwArgsArray])
(defn KwArgsSeq->KwArgsArray [kws]
  {:pre [(r/KwArgsSeq? kws)]
   :post [(r/KwArgsArray? %)]}
  (r/KwArgsArray-maker (:kw-args-regex kws)))

(t/ann HMap->KwArgsSeq [HeterogeneousMap t/Bool -> r/Type])
(defn HMap->KwArgsSeq [kws]
  {:pre [(r/HeterogeneousMap? kws)]
   :post [(r/Type? %)]}
  (r/-kw-args-seq :mandatory (:types kws)
                  :optional (:optional kws)
                  :complete? (complete-hmap? kws)
                  :maybe-trailing-nilable-non-empty-map? false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heterogeneous type ops
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; utility functions

(defn ifn-ancestor 
  "If this type can be treated like a function, return one of its
  possibly polymorphic function ancestors.
  
  Assumes the type is not a union"
  [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [t (fully-resolve-type t)]
    (cond
      (r/RClass? t)
      (first (filter (some-fn r/Poly? r/FnIntersection?) (RClass-supers* t)))
      ;handle other types here
      )))

(t/tc-ignore
(defn type-into-vector [x] (if (r/Union? x) (:types x) [x]))

(defn resolved-type-vector [t]
  {:post [(every? r/Type? %)]}
  (cond
   (r/TCResult? t)
   (mapv fully-resolve-type
         (type-into-vector (-> t :t fully-resolve-type)))
   
   (r/Type? t)
   (mapv fully-resolve-type (type-into-vector (fully-resolve-type t)))
   
   :else
   [t]))

(defn union-or-nil [ts]
  (if (some nil? ts) nil (apply Un ts)))

(defn reduce-type-transform
  "Given a function f, left hand type t, and arguments, reduce the function
  over the left hand types with each argument in turn.
  
  Arguments will not be touched, it is up to f to resolve TCResults as needed.
  However, unions returned by f will be expanded, so the left hand type argument
  will not be a (raw) Union.
  
  Reduction is skipped once nil is returned, or optional predicate :when
  returns false."
  [func t args & {pred :when}]
  {:post [((some-fn nil? r/Type?) %)]}
  (let [ok? #(and % (if pred (pred %) true))]
    (union-or-nil
     (reduce
      (fn [left-types arg]
        (if (every? ok? left-types)
          (for [left left-types
                res (type-into-vector (func left arg))]
            res)
          [nil]))
      (resolved-type-vector t)
      args))))

;; Inferring bounds

(defn find-bound* 
  "Find upper bound if polarity is true, otherwise lower bound"
  [t* polarity]
  {:pre [(r/Type? t*)]}
  (let [fnd-bnd #(find-bound* % polarity)
        t t*]
    (cond
      (r/Name? t) (fnd-bnd (resolve-Name t))
      (r/App? t) (fnd-bnd (resolve-App t))
      (r/TApp? t) (fnd-bnd (resolve-TApp t))
      (r/Mu? t) (let [name (Mu-fresh-symbol* t)
                      body (Mu-body* name t)
                      new-body (fnd-bnd body)]
                  (Mu* name new-body))
      (r/Poly? t) (fnd-bnd (Poly-body* (Poly-fresh-symbols* t) t))
      (r/TypeFn? t) (let [names (TypeFn-fresh-symbols* t)
                          body (TypeFn-body* names t)
                          bbnds (TypeFn-bbnds* names t)
                          new-body (fnd-bnd body)]
                      (TypeFn* names
                               (:variances t)
                               bbnds
                               new-body))
      :else (if polarity
              r/-any
              r/-nothing))))

(defn find-upper-bound [t]
  {:pre [(r/Type? t)]}
  (find-bound* t true))

(defn find-lower-bound [t]
  {:pre [(r/Type? t)]}
  (find-bound* t false))

(defn infer-bounds
  "Returns a Bounds that attempts to fill in meaningful
  upper/lower bounds of the same rank"
  [upper-or-nil lower-or-nil]
  {:pre [(every? (some-fn nil? r/AnyType?) [upper-or-nil lower-or-nil])]
   :post [(r/Bounds? %)]}
  (let [{:keys [upper lower]} (cond 
                                ;both bounds provided
                                (and upper-or-nil lower-or-nil) {:upper upper-or-nil :lower lower-or-nil}
                                ;only upper
                                upper-or-nil {:upper upper-or-nil :lower (find-lower-bound upper-or-nil)}
                                ;only lower
                                lower-or-nil {:upper (find-upper-bound lower-or-nil) :lower lower-or-nil}
                                ;no bounds provided, default to Nothing <: t/Any
                                :else {:upper r/-any :lower r/-nothing})]
    (r/Bounds-maker upper lower nil)))

(defn find-val-type 
  ([t k default] (find-val-type t k default #{}))
  ([t k default seen]
  {:pre [(r/Type? t)
         (r/Type? k)
         (r/Type? default)]
   :post [(r/Type? %)]}
  (let [_ (when (seen t)
            (err/int-error
              (str "Infinitely expanding type:" (ind/unparse-type t))))
        t (fully-resolve-type t)
        find-val-type (fn 
                        ([t k default]
                         (find-val-type t k default seen))
                        ([t k default seen]
                         (find-val-type t k default seen)))]
    (cond
      ; propagate the error
      (r/TCError? t) t
      (r/F? t) (let [bnd (free-ops/free-with-name-bnds (:name t))
                     _ (when-not bnd
                         (err/int-error (str "No bounds for type variable: " name bnds/*current-tvar-bnds*)))]
                 (find-val-type (:upper-bound bnd) k default
                                #{}))
      (ind/subtype? t r/-nil) default
      (r/AssocType? t) (let [t* (apply ind/assoc-pairs-noret (:target t) (:entries t))]
                         (cond
                           (:dentries t) (do
                                           ;(prn "dentries NYI")
                                           r/-any)
                           (r/HeterogeneousMap? t*) (find-val-type t* k default)

                           (and (not t*)
                                (r/F? (:target t))
                                (every? keyword-value? (map first (:entries t))))
                           (let [hmap (apply ind/assoc-pairs-noret (-partial-hmap {}) (:entries t))]
                             (if (r/HeterogeneousMap? hmap)
                               (find-val-type hmap k default)
                               r/-any))
                           :else r/-any))
      (r/HeterogeneousMap? t) (let [pres ((:types t) k)
                                    opt  ((:optional t) k)]
                                (cond
                                  ; normal case, we have the key declared present
                                  pres pres

                                  ; absent key, default
                                  ((:absent-keys t) k)
                                  (do
                                    #_(tc-warning
                                        "Looking up key " (ind/unparse-type k) 
                                        " in heterogeneous map type " (ind/unparse-type t)
                                        " that declares the key always absent.")
                                    default)

                                  ; if key is optional the result is the val or the default
                                  opt (Un opt default)

                                  ; if map is complete, entry must be missing
                                  (complete-hmap? t) default

                                  :else
                                  (do #_(tc-warning "Looking up key " (ind/unparse-type k)
                                                    " in heterogeneous map type " (ind/unparse-type t)
                                                    " which does not declare the key absent ")
                                      r/-any)))

      (r/Record? t) (find-val-type (Record->HMap t) k default)

      (r/Intersection? t) (apply In 
                                 (for [t* (:types t)]
                                   (find-val-type t* k default)))
      (r/Union? t) (apply Un
                          (for [t* (:types t)]
                            (find-val-type t* k default)))
      (r/RClass? t)
      (r/ret-t
        (ind/check-funapp nil
                          nil
                          (r/ret
                            (env-utils/force-type
                              ((requiring-resolve 'typed.clj.checker.base-env/get-type))))
                          [(r/ret t) (r/ret k) (r/ret default)]
                          nil))
      :else r/-any))))

(defn find-hsequential-in-non-union [t]
  {:post [((some-fn nil? r/HSequential?) %)]}
  (let [t (fully-resolve-type t)]
    (assert (not (r/Union? t)) t)
    (cond
      (r/HSequential? t) t
      ;; TODO we just pick the first HSequential we find, perhaps 
      ;; there's a better strategy (eg., return a list of them)
      (r/RClass? t) (some find-hsequential-in-non-union
                          (disj (RClass-supers* t)
                                ;; stop at top of class hierarchy to prevent
                                ;; infinite recursion
                                (RClass-of Object))))))

(defn -tapp [op & rands]
  (r/TApp-maker op (seq rands)))

(defn -name [sym & ts]
  (let [nme (r/Name-maker sym)]
    (if ts
      (r/TApp-maker nme ts)
      nme)))

(defn union-Results [r1 r2]
  {:pre [(r/Result? r1)
         (r/Result? r2)]
   :post [(r/Result? %)]}
  (r/make-Result (Un (:t r1) (:t r2))
                 (ind/-FS
                   (ind/-or (-> r1 :fl :then)
                            (-> r1 :fl :then))
                   (ind/-or (-> r1 :fl :else)
                            (-> r1 :fl :else)))
                 (if (= (-> r1 :o) (-> r2 :o))
                   (-> r1 :o)
                   or/-empty)))

(defn intersect-Results [r1 r2]
  {:pre [(r/Result? r1)
         (r/Result? r2)]
   :post [(r/Result? %)]}
  (r/make-Result (In (:t r1) (:t r2))
                 (ind/-FS
                   (ind/-and (-> r1 :fl :then)
                             (-> r1 :fl :then))
                   (ind/-and (-> r1 :fl :else)
                             (-> r1 :fl :else)))
                 (if (= (-> r1 :o) (-> r2 :o))
                   (-> r1 :o)
                   or/-empty)))
)

;; =====================================================
;; Fold defaults

;; if every element of the result is identical to c, then returns c.
;; this works in tandem with the `assoc` implementation of types, which
;; avoids reconstructing the types if the updated value is identical.
(defn into-identical [init f c]
  ;; TODO map support 
  {:pre [((some-fn vector? set?) init)]}
  (let [changed? (volatile! false)
        res (into init (map (fn [e]
                              (let [r (f e)]
                                (when-not (identical? r e)
                                  (vreset! changed? true))
                                r)))
                  c)]
    (if @changed?
      res
      c)))

(add-default-fold-case NotType
                       (fn [ty]
                         (-> ty
                             ; are negative types covariant?
                             (update :type type-rec #_{:variance :contravariant}))))

(add-default-fold-case DifferenceType
                       (fn [ty]
                         (-> ty
                           (update :type type-rec)
                           (update :without #(into-identical [] type-rec %)))))

(add-default-fold-case Intersection
                       (fn [ty]
                         ;(prn "fold-default Intersection" ty)
                         (let [ts (:types ty)
                               ts' (into-identical [] type-rec ts)]
                           (if (identical? ts ts')
                             ty
                             (apply In ts')))))

(add-default-fold-case Union 
                       (fn [ty]
                         ;(prn "union default" (typed.clj.checker.parse-unparse/unparse-type ty))
                         (let [ts (:types ty)
                               ts' (into-identical [] type-rec ts)]
                           (if (identical? ts ts')
                             ty
                             (apply Un ts')))))

(add-default-fold-case FnIntersection
                       (fn [ty]
                         (-> ty
                           (update :types #(into-identical [] type-rec-no-simpl %)))))

(add-default-fold-case Bounds
                       (fn [ty]
                         (r/visit-bounds ty type-rec)))

(add-default-fold-case DottedPretype
                       (fn [ty]
                         (-> ty
                           (update :pre-type type-rec))))

(add-default-fold-case Function
                       (fn [ty]
                         {:pre [(#{:fixed :rest :drest :prest :pdot :kws} (:kind ty))]}
                         ;(prn "fold Function" ty)
                         (-> ty
                           (update :dom #(into-identical [] type-rec %))
                           (update :rng type-rec)
                           (update :rest #(some-> % type-rec))
                           (update :drest #(some-> % type-rec))
                           (update :pdot #(some-> % type-rec))
                           (update :kws #(some-> % type-rec))
                           (update :prest #(when %
                                             (let [t (type-rec %)]
                                               ;; if we fully flatten out the prest, we're left
                                               ;; with no prest
                                               (when (not= r/-nothing t)
                                                 t)))))))

(add-default-fold-case JSNominal
                       (fn [ty]
                         (-> ty
                             (update :poly? #(some->> % (into-identical [] type-rec))))))

(add-default-fold-case RClass
                       (fn [ty]
                         (-> ty
                             (update :poly? #(some->> % (into-identical [] type-rec))))))

(add-default-fold-case App
                       (fn [ty]
                         (-> ty
                           (update :rator type-rec)
                           (update :rands #(into-identical [] type-rec %)))))

(add-default-fold-case TApp
                       (fn [ty]
                         (-> ty
                           (update :rator type-rec)
                           ;;TODO variance
                           (update :rands #(into-identical [] type-rec %)))))

(add-default-fold-case PrimitiveArray
                       (fn [ty]
                         (-> ty
                           (update :input-type type-rec)
                           (update :output-type type-rec))))

(defn visit-args+variances [args variances type-rec]
  (let [changed? (volatile! false)
        res (mapv (fn [arg v]
                    (let [arg' (type-rec arg {:variance v})]
                      (when-not (identical? arg arg')
                        (vreset! changed? true))
                      arg'))
                  args
                  variances)]
    (if changed?
      res
      args)))

(defn visit-DataType-or-Protocol-args [{:keys [variances] :as ty} type-rec]
  (-> ty
      (update :poly? #(some-> % (visit-args+variances variances type-rec)))))

(add-default-fold-case DataType
                       (fn [ty]
                         (-> ty
                             (visit-DataType-or-Protocol-args type-rec)
                             (update :fields (fn [fs]
                                               (let [changed? (volatile! false)
                                                     flat (mapcat (fn [[k t]]
                                                                    (let [t' (type-rec t)]
                                                                      (when-not (identical? t t')
                                                                        (vreset! changed? true))
                                                                      [k t']))
                                                                  fs)]
                                                 (if @changed?
                                                   (apply array-map flat)
                                                   fs)))))))

(add-default-fold-case Protocol
                       (fn [ty]
                         (-> ty
                             (visit-DataType-or-Protocol-args type-rec)
                             ;FIXME this should probably be left alone in fold
                             ; same in promote/demote
                             (update :methods update-vals type-rec))))

(add-default-fold-case TypeFn
                       (fn [ty]
                         (let [names (TypeFn-fresh-symbols* ty)
                               body (TypeFn-body* names ty)
                               bbnds (TypeFn-bbnds* names ty)
                               bmap (zipmap (map r/make-F names) bbnds)
                               bbnds' (free-ops/with-bounded-frees bmap
                                        (into-identical [] #(r/visit-bounds % type-rec) bbnds))
                               body' (free-ops/with-bounded-frees bmap
                                       (type-rec body))
                               changed? (or (not (identical? bbnds bbnds'))
                                            (not (identical? body body')))]
                           (if changed?
                             (TypeFn* names (:variances ty) bbnds' body')
                             ty))))


(add-default-fold-case Poly
                       (fn [ty]
                         (let [names (Poly-fresh-symbols* ty)
                               body (Poly-body* names ty)
                               bbnds (Poly-bbnds* names ty)
                               bmap (zipmap (map r/make-F names) bbnds)
                               bbnds' (free-ops/with-bounded-frees bmap
                                        (into-identical [] #(r/visit-bounds % type-rec) bbnds))
                               body' (free-ops/with-bounded-frees bmap
                                       (type-rec body))
                               changed? (or (not (identical? bbnds bbnds'))
                                            (not (identical? body body')))]
                           (if changed?
                             (Poly* names bbnds' body' :named (:named ty))
                             ty))))

(add-default-fold-case PolyDots
                       (fn [ty]
                         (let [names (PolyDots-fresh-symbols* ty)
                               body (PolyDots-body* names ty)
                               bbnds (PolyDots-bbnds* names ty)
                               ; don't scope the dotted bound
                               bmap (zipmap (map r/make-F (rest names)) (rest bbnds))
                               bbnds' (free-ops/with-bounded-frees bmap
                                        (into-identical [] #(r/visit-bounds % type-rec) bbnds))
                               body' (free-ops/with-bounded-frees bmap
                                       (type-rec body))
                               changed? (or (not (identical? bbnds bbnds'))
                                            (not (identical? body body')))]
                           (if changed?
                             (PolyDots* names bbnds' body' :named (:named ty))
                             ty))))

(add-default-fold-case Mu
                       (fn [ty]
                         (let [name (Mu-fresh-symbol* ty)
                               body (Mu-body* name ty)
                               body' (type-rec body)
                               changed? (not (identical? body body'))]
                           (if changed?
                             (Mu* name body')
                             ty))))

(add-default-fold-case HSequential 
                       (fn [{:keys [types fs objects rest drest repeat kind] :as ty}]
                         (let [types' (into-identical [] type-rec types)
                               fs' (into-identical [] filter-rec fs)
                               objects' (into-identical [] object-rec objects)
                               rest' (some-> rest type-rec)
                               drest' (some-> drest type-rec)
                               changed? (or (not (identical? types types'))
                                            (not (identical? fs fs'))
                                            (not (identical? objects objects'))
                                            (not (identical? rest rest'))
                                            (not (identical? drest drest')))]
                           (if changed?
                             (r/-hsequential types'
                                             :filters fs'
                                             :objects objects'
                                             :rest rest'
                                             :drest drest'
                                             :repeat repeat
                                             :kind kind)
                             ty))))

(add-default-fold-case HSet
                       (fn [{:keys [fixed] :as ty}]
                         (let [fixed' (into-identical #{} type-rec fixed)
                               changed? (not (identical? fixed fixed'))]
                           (if changed?
                             (r/-hset fixed')
                             ty))))

(defn- visit-type-map [m f]
  (let [changed? (volatile! false)
        res (reduce-kv (fn [m k v]
                         (let [k' (f k)
                               v' (f v)]
                           (when-not (and (identical? k k')
                                          (identical? v v'))
                             (vreset! changed? true))
                           (assoc m k' v')))
                       {} m)]
    (if changed?
      res
      m)))

(defn- visit-type-paired-vector [v f]
  (let [changed? (volatile! false)
        res (mapv (fn [[k v]]
                    (let [k' (f k)
                          v' (f v)]
                      (when-not (and (identical? k k')
                                     (identical? v v'))
                        (vreset! changed? true))
                      [k' v']))
                  v)]
    (if changed?
      res
      v)))

(add-default-fold-case HeterogeneousMap
                       (fn [ty]
                         (let [mandatory (visit-type-map (:types ty) type-rec)]
                           (if (some #{r/-nothing} (apply concat mandatory))
                             r/-nothing
                             (-> ty 
                                 (assoc :types mandatory)
                                 (update :optional visit-type-map type-rec))))))

(add-default-fold-case JSObj
                       (fn [ty]
                         (-> ty 
                           (update :types #(let [vs (vals %)
                                                 vs' (into-identical [] type-rec vs)
                                                 changed? (not (identical? vs vs'))]
                                             (if changed?
                                               (zipmap (keys %) vs')
                                               %))))))

(add-default-fold-case KwArgs
                       (fn [ty]
                         (-> ty 
                             (update :mandatory visit-type-map type-rec)
                             (update :optional visit-type-map type-rec))))


(add-default-fold-case KwArgsSeq
                       (fn [ty]
                         (-> ty 
                             (update :kw-args-regex type-rec))))

(add-default-fold-case Extends
                       (fn [{:keys [extends without] :as ty}]
                         (let [extends' (into-identical [] type-rec extends)
                               without' (into-identical [] type-rec without)
                               changed? (or (not (identical? extends extends'))
                                            (not (identical? without without')))]
                           (if changed?
                             (-extends extends' :without without')
                             ty))))

(add-default-fold-case GetType
                       (fn [ty]
                         (-> ty
                             (update :target type-rec)
                             (update :key type-rec)
                             (update :not-found type-rec)
                             (update :target-fs filter-rec)
                             (update :target-object object-rec))))

(add-default-fold-case AssocType
                       (fn [{:keys [target entries dentries] :as ty}]
                         (let [target' (type-rec target)
                               entries' (visit-type-paired-vector entries type-rec)
                               changed? (or (not (identical? target target'))
                                            (not (identical? entries entries')))]
                           (or (when-not dentries
                                 (if changed?
                                   (apply (requiring-resolve 'typed.clj.checker.assoc-utils/assoc-pairs-noret)
                                          target' entries')
                                   ty))
                               (-> ty
                                   (assoc :target target')
                                   (assoc :entries entries')
                                   (assoc :dentries (some-> dentries type-rec)))))))

(def ^:private ret-first identity)

(add-default-fold-case CountRange ret-first)
(add-default-fold-case Name ret-first)
(add-default-fold-case Value ret-first)
(add-default-fold-case Top ret-first)
(add-default-fold-case Wildcard ret-first)
(add-default-fold-case Unchecked ret-first)
(add-default-fold-case TCError ret-first)
(add-default-fold-case TopFunction ret-first)
(add-default-fold-case B ret-first)
(add-default-fold-case F ret-first)
(add-default-fold-case TypeOf ret-first)
(add-default-fold-case SymbolicClosure ret-first)

(add-default-fold-case Result 
                       (fn [ty]
                         (-> ty
                             (update :t type-rec)
                             (update :fl filter-rec)
                             (update :o object-rec))))

(comment
  (repeatedly)
  (cycle)
  (->
    (iterate
      macroexpand-1
      '(add-default-fold-case Result 
                              (fn [ty]
                                (-> ty
                                    (update :t type-rec)
                                    (update :fl filter-rec)
                                    (update :o object-rec)))))
    (nth 2)
    clojure.pprint/pprint)
)

(defmacro ^:private ret-first-many [& cls]
  `(do ~@(map #(list `add-default-fold-case % `ret-first) cls)))

; CLJS types

(ret-first-many JSNumber CLJSInteger JSObject JSString JSBoolean JSUndefined
                JSNull JSSymbol)

(add-default-fold-case ArrayCLJS
                       (fn [ty]
                         (-> ty
                             (update :input-type type-rec)
                             (update :output-type type-rec))))

;filters

(add-default-fold-case NoFilter ret-first)
(add-default-fold-case TopFilter ret-first)
(add-default-fold-case BotFilter ret-first)

(add-default-fold-case TypeFilter
                       (fn [ty]
                         (-> ty
                             (update :type type-rec)
                             (update :path #(some->> % (into-identical [] pathelem-rec))))))

(add-default-fold-case NotTypeFilter
                       (fn [ty]
                         (-> ty
                             (update :type type-rec {:variance :contravariant})
                             (update :path #(some->> % (into-identical [] pathelem-rec))))))

(add-default-fold-case ImpFilter
                       (fn [ty]
                         (-> ty
                             (update :a filter-rec)
                             (update :c filter-rec))))

(add-default-fold-case AndFilter
                       (fn [{:keys [fs] :as ty}]
                         (let [fs' (into-identical [] filter-rec (:fs ty))
                               changed? (not (identical? fs fs'))]
                           (if changed?
                             (apply ind/-and fs')
                             ty))))

(add-default-fold-case OrFilter
                       (fn [{:keys [fs] :as ty}]
                         (let [fs' (into-identical [] filter-rec (:fs ty))
                               changed? (not (identical? fs fs'))]
                           (if changed?
                             (apply ind/-or fs')
                             ty))))

(add-default-fold-case FilterSet
                       (fn [ty]
                         (-> ty
                             (update :then filter-rec)
                             (update :else filter-rec))))


;objects
(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty]
                         (-> ty
                             (update :path #(some->> % (into-identical [] pathelem-rec))))))
(add-default-fold-case NoObject ret-first)

;path-elems

(add-default-fold-case KeyPE ret-first)
(add-default-fold-case KeysPE ret-first)
(add-default-fold-case ValsPE ret-first)
(add-default-fold-case ClassPE ret-first)
(add-default-fold-case NthPE ret-first)
(add-default-fold-case CountPE ret-first)
(add-default-fold-case KeywordPE ret-first)

;TCResult

(add-default-fold-case TCResult
                       (fn [ty]
                         (-> ty
                             (update :t type-rec)
                             (update :fl filter-rec)
                             (update :o object-rec))))

(add-default-fold-case MergeType
                       (fn [ty]
                         (-> ty
                             (update :types #(into-identical [] type-rec %)))))

(add-default-fold-case Regex
                       (fn [ty]
                         (-> ty
                             (update :types #(into-identical [] type-rec %)))))
