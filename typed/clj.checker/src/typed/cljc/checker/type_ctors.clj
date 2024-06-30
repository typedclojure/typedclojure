;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.type-ctors
  (:refer-clojure :exclude [defrecord replace type requiring-resolve repeatedly])
  (:require [clojure.core.cache :as cache]
            [typed.clojure :as t]
            [clojure.string :as str]
            [typed.cljc.runtime.env-utils :as env-utils]
            [typed.cljc.runtime.env :as env]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-utils-platform-specific :as plat-con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.reflect :as reflect]
            [clojure.repl :as repl]
            [clojure.set :as set]
            [typed.cljc.runtime.perf-utils :as perf :refer [repeatedly]]
            [typed.clj.checker.rclass-env :as rcls]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.datatype-env :as dtenv]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.fold-rep :as f :refer [add-default-fold-case]]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.impl-protocols :as p]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.object-rep :as or]
            [typed.cljc.checker.path-rep :as path]
            [typed.cljc.checker.protocol-env :as prenv]
            [typed.cljc.checker.tvar-bnds :as bnds]
            [typed.cljc.checker.type-rep :as r :refer [ret-t]]
            [typed.cljc.checker.utils :as u :refer [OR AND]]
            typed.cljc.checker.coerce-ann)
  (:import (clojure.lang ASeq)
           java.lang.reflect.Modifier
           (typed.cljc.checker.type_rep HeterogeneousMap Poly TypeFn TApp App Value
                                        Union Intersection F Function Mu B KwArgs KwArgsSeq KwArgsArray
                                        RClass Bounds Name Scope CountRange Intersection DataType
                                        JSNominal Protocol GetType HSequential
                                        HSet AssocType TypeOf MergeType
                                        NotType Intersection Union FnIntersection
                                        DottedPretype Function JSNominal App TApp
                                        PrimitiveArray DataType Satisfies Instance TypeFn Poly
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Wildcard Unchecked TopFunction B F Result
                                        TCResult TCError
                                        JSNumber CLJSInteger JSObject JSString ArrayCLJS
                                        JSBoolean AssocType GetType KwArgsSeq KwArgs HSequential HSet
                                        JSUndefined JSNull JSSymbol JSObj TypeOf SymbolicClosure Regex
                                        MatchType)
           (typed.cljc.checker.filter_rep NoFilter TopFilter BotFilter TypeFilter NotTypeFilter
                                          ImpFilter AndFilter OrFilter FilterSet)
           (typed.cljc.checker.object_rep NoObject EmptyObject Path)
           (typed.cljc.checker.path_rep KeyPE KeysPE ValsPE ClassPE NthPE CountPE KeywordPE SeqPE)))

(set! *warn-on-reflection* true)

(t/ann ^:no-check with-original-names [r/Type (t/U t/Sym (t/Seqable t/Sym)) -> r/Type])
(defn- with-original-names [t names]
  (vary-meta t assoc ::names names))

(t/ann ^:no-check get-original-names [r/Type -> (t/U t/Sym (t/Seqable t/Sym))])
(defn get-original-names [t]
  (-> t meta ::names))

(t/ann fresh-symbol [t/Sym -> t/Sym])
(defn fresh-symbol
  ([s] (fresh-symbol s nil))
  ([s hint]
   {:pre [(symbol? s)]
    :post [(symbol? %)]}
   (with-meta (gensym (str s hint)) {:original-name s})))

(declare Un make-Union make-Intersection fully-resolve-type fully-resolve-non-rec-type flatten-unions)

(t/ann bottom r/Type)
(def ^:private bottom (r/Union-maker (sorted-set)))

;;TODO accepts IReduce
(t/ann ^:no-check make-Union [(t/Seqable r/Type) t/Any -> r/Type])
(defn make-Union
  "Does not resolve types."
  [args {::vs/keys [no-simpl] :as opts}]
  (if no-simpl
    (r/Union-maker (vec args))
    (let [ts (flatten-unions args)]
      (case (count ts)
        0 bottom
        1 (first ts)
        (r/Union-maker ts)))))

;; Heterogeneous maps

(declare make-HMap)

(t/ann -complete-hmap [(t/Map r/Type r/Type) t/Any -> r/Type])
(defn -complete-hmap [types opts]
  (make-HMap opts {:mandatory types 
                   :complete? true}))

(t/ann -partial-hmap (t/IFn [t/Any (t/Map r/Type r/Type) -> r/Type]
                            [t/Any (t/Map r/Type r/Type) (t/Set r/Type) -> r/Type]))
(defn -partial-hmap
  ([opts types] (-partial-hmap opts types #{}))
  ([opts types absent-keys] (make-HMap opts {:mandatory types 
                                             :absent-keys absent-keys})))

(t/defalias TypeMap
  "A regular map with types as keys and vals."
  (t/Map r/Type r/Type))

(declare In keyword-value? RClass-of Protocol-of complete-hmap? -name)

(t/ann ^:no-check allowed-hmap-key? [r/Type -> t/Bool])
(defn allowed-hmap-key? [k]
  (keyword-value? k))

(defn upcast-PrimitiveArray [^PrimitiveArray t opts]
  (RClass-of clojure.lang.Seqable [(-name 'typed.clojure/NilableNonEmptySeq (.output-type t))] opts))

; Partial HMaps do not record absence of fields, only subtype to (APersistentMap t/Any t/Any)
(t/ann ^:no-check upcast-hmap* 
       [(t/Map r/Type r/Type) (t/Map r/Type r/Type) (t/Set r/Type) t/Bool t/Any :? t/Any -> r/Type])
(defn upcast-hmap*
  ([mandatory optional absent-keys complete? opts] (upcast-hmap* mandatory optional absent-keys complete? nil opts))
  ([mandatory optional absent-keys complete?
    {:keys [visit-ks-type
            visit-vs-type
            elide-upper-count]
     :or {visit-ks-type identity
          visit-vs-type identity}
     :as opt}
    opts]
   {:pre [(-> opt keys set (disj :visit-ks-type :visit-vs-type :elide-upper-count) empty?)]}
   (let [upcast-ctor (fn [ks vs]
                       (let [ks (visit-ks-type ks)
                             vs (visit-vs-type vs)]
                         (impl/impl-case opts
                           :clojure (RClass-of 'clojure.lang.APersistentMap [ks vs] opts)
                           :cljs (-name 'typed.clojure/Map ks vs))))]
     (if complete?
       (In [(upcast-ctor (Un (mapcat keys [mandatory optional]) opts)
                         (Un (mapcat vals [mandatory optional]) opts))
            (r/make-CountRange 
              ; assume all optional entries are absent
              #_:lower
              (count mandatory)
              ; assume all optional entries are present
              #_:upper
              (when-not elide-upper-count
                (+ (count mandatory)
                   (count optional))))]
           opts)
       (In [(upcast-ctor r/-any r/-any)
            (r/make-CountRange 
              ; assume all optional entries are absent
              #_:lower
              (count mandatory)
              ; partial hmap can be infinite count
              #_:upper
              nil)]
           opts)))))

(t/ann ^:no-check upcast-hmap [HeterogeneousMap t/Any :? t/Any -> r/Type])
(defn upcast-hmap
  ([hmap opts] (upcast-hmap hmap nil opts))
  ([hmap opt opts]
   {:pre [(r/HeterogeneousMap? hmap)]
    :post [(r/Type? %)]}
   (upcast-hmap* (:types hmap)
                 (:optional hmap)
                 (:absent-keys hmap)
                 (complete-hmap? hmap)
                 opt
                 opts)))

(t/ann ^:no-check make-HMap [t/Any (t/HMap :optional {:mandatory (t/Map r/Type r/Type) :optional (t/Map r/Type r/Type)
                                                      :absent-keys (t/Set r/Type) :complete? t/Bool}) 
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
  [opts
   {:keys [mandatory optional complete? absent-keys]
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
    (upcast-hmap* mandatory optional absent-keys complete? opts)

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
          mandatory (merge-with #(In [%1 %2] opts) mandatory (select-keys optional optional-now-mandatory))
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

(t/ann ^:no-check upcast-hset [HSet t/Any -> r/Type])
(defn upcast-hset [{:keys [fixed complete?] :as hset} opts]
  {:pre [(r/HSet? hset)]
   :post [(r/Type? %)]}
  (let [tp (if complete?
             (Un (:fixed hset) opts)
             r/-any)]
    (impl/impl-case opts
      :clojure (RClass-of 'clojure.lang.APersistentSet [tp] opts)
      :cljs (-name 'typed.clojure/Set tp))))

; TODO Should update this with prest
(t/ann ^:no-check upcast-HSequential [HSequential -> r/Type])
(defn upcast-HSequential
  ([hsequential opts] (upcast-HSequential hsequential nil opts))
  ([{:keys [types rest drest kind] :as hsequential}
    {:keys [visit-elem-type elide-count] :or {visit-elem-type identity}}
    opts]
   {:pre [(r/HSequential? hsequential)]
    :post [(r/Type? %)]}
   ;; Note: make-Union and make-Intersection used to be Un and In,
   ;; but something funny happened with Ping/Pong.
   ;; See typed-test.cljc.name-utils/find-recursive-names-test.
   (let [tp (-> (if-not drest
                  (make-Union
                    (concat types
                            (when rest
                              [rest]))
                    opts)
                  r/-any)
                visit-elem-type)]
     (make-Intersection
       (cond->
         [(impl/impl-case opts
            :clojure (case kind
                       :vector (RClass-of clojure.lang.APersistentVector [tp] opts)
                       :seq (-name 'typed.clojure/ASeq tp)
                       :list (RClass-of clojure.lang.PersistentList [tp] opts)
                       :sequential (In [(RClass-of clojure.lang.IPersistentCollection [tp] opts)
                                        (RClass-of clojure.lang.Sequential opts)]
                                       opts))
            :cljs (case kind
                    :vector (-name 'typed.clojure/Vec tp)
                    :seq (-name 'typed.clojure/ASeq tp)
                    :list (-name 'typed.clojure/List tp)
                    :sequential (In [(-name 'typed.clojure/Coll tp)
                                     (-name 'cljs.core/ISequential)]
                                    opts)))]
         (and (not drest)
              (not elide-count)) (conj (r/make-CountRange
                                         (count types)
                                         (when-not rest
                                           (count types)))))
       opts))))

(defn upcast-kw-args-seq [{kws :kw-args-regex
                           :as kwseq}
                          opts]
  {:pre [(r/KwArgsSeq? kwseq)]
   :post [(r/Type? %)]}
  (let [ss (if (and (:complete? kws)
                    (not (:maybe-trailing-nilable-non-empty-map? kws)))
             (Un (concat
                   (apply concat (:mandatory kws))
                   (apply concat (:optional kws)))
                 opts)
             r/-any)
        min-count (* 2 (count (:mandatory kws)))
        max-count (when (:complete? kws)
                    (cond-> (+ (* 2 (count (:mandatory kws)))
                               (* 2 (count (:optional kws))))
                      (:maybe-trailing-nilable-non-empty-map? kws) inc))]
    (when max-count
      (assert (<= min-count max-count)))
    (In [(r/make-CountRange min-count max-count)
         (impl/impl-case opts
           :clojure (RClass-of ASeq [ss] opts)
           :cljs (Protocol-of 'cljs.core/ISeq [ss] opts))]
        opts)))

;; Unions

(t/defalias TypeCache 
  (t/Map (t/Set r/Type) r/Type))

(t/ann ^:no-check initial-Un-cache TypeCache)
(def initial-Un-cache (cache/lu-cache-factory {} :threshold 256))

(declare flatten-intersections)

(t/ann ^:no-check Un [(t/Seqable r/Type) t/Any :-> r/Type])
(defn Un [types {::keys [Un-cache]
                 ::vs/keys [no-simpl] :as opts}]
  {:post [(r/Type? %)]}
  (if no-simpl
    (make-Union types opts)
    (let [cache-key (into #{} (map r/assert-Type) types)]
      (if-let [hit (when Un-cache (get @Un-cache cache-key))]
        hit
        (let [res (letfn [;; a is a Type (not a union type)
                          ;; b is a Set[Type] (non overlapping, non Union-types)
                          ;; The output is a non overlapping list of non Union types.
                          (merge-type [a b]
                            {:pre [(set? b)
                                   (r/Type? a)
                                   (not (r/Union? a))]
                             :post [(set? %)]}
                            #_(prn "merge-type" a b)
                            (let [b* (make-Union b opts)
                                  ;_ (prn "merge-type" a b*)
                                  res (cond
                                        ; don't resolve type applications in case types aren't
                                        ; fully defined yet
                                        ; TODO basic error checking, eg. number of params
                                        (some (some-fn r/Name? r/TApp?) (conj b a)) (conj b a)
                                        (ind/subtype? a b* opts) b
                                        (ind/subtype? b* a opts) #{a}
                                        :else (into #{a}
                                                    (remove #(ind/subtype? % a opts))
                                                    b))]
                              ;(prn "res" res)
                              res))]
                    (let [types (flatten-unions (eduction (map #(fully-resolve-non-rec-type % #{} opts)) types))]
                      (cond
                        (empty? types) r/empty-union
                        (= 1 (count types)) (first types)
                        :else 
                        (make-Union
                          (reduce (fn [acc t] (merge-type t acc))
                                  (sorted-set)
                                  types)
                          opts))))]
          (some-> Un-cache (swap! assoc cache-key res))
          res)))))

;; Intersections

(declare overlap In)

(t/ann ^:no-check make-Intersection [(t/Seqable r/Type) -> r/Type])
(defn make-Intersection
  "Does not resolve types."
  [types {::vs/keys [no-simpl] :as opts}]
  (if no-simpl
    (r/Intersection-maker (vec types))
    (let [ts (flatten-intersections types)]
      (case (count ts)
        0 r/-any
        1 (first ts)
        (r/Intersection-maker ts)))))

(declare RClass-of)

(t/ann ^:no-check HMap-with-Value-keys? [HeterogeneousMap :* :-> t/Bool])
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
  [t1 t2 opts]
  {:pre [(r/HeterogeneousMap? t1)
         (r/HeterogeneousMap? t2)
         (HMap-with-Value-keys? t1 t2)]
   :post [(r/Type? %)]}
  ; make-HMap handles duplicates
  (make-HMap opts
             {:mandatory (apply merge-with #(In [%1 %2] opts) (map :types [t1 t2]))
              :optional (apply merge-with #(In [%1 %2] opts) (map :optional [t1 t2]))
              :absent-keys (apply set/union (map :absent-keys [t1 t2]))
              :complete? (not-any? :other-keys? [t1 t2])}))

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
(defn intersect [t1 t2 {::keys [intersect-cache] :as opts}]
  {:pre [(r/Type? t1)
         (r/Type? t2)
         #_(not (r/Union? t1))
         #_(not (r/Union? t2))]
   :post [(r/Type? %)]}
  (let [cache-key (hash-set t1 t2)]
    (if-let [hit (when intersect-cache (@intersect-cache cache-key))]
      hit
      (let [t (cond
                ; Unchecked is "sticky" even though it's a subtype/supertype
                ; of everything
                (or (and (r/Unchecked? t1) (not (r/Unchecked? t2)))
                    (and (not (r/Unchecked? t1)) (r/Unchecked? t2)))
                (make-Intersection [t1 t2] opts)

                (and (r/HeterogeneousMap? t1)
                     (r/HeterogeneousMap? t2))
                (intersect-HMap t1 t2 opts)

                ;RClass's with the same base, intersect args pairwise
                (and (r/RClass? t1)
                     (r/RClass? t2)
                     (= (:the-class t1) (:the-class t2))
                     ;; TODO what about invariant?
                     (every? #{:covariant :contravariant} (:variances t1)))
                (RClass-of (:the-class t1)
                           (map (fn [v l r]
                                  ;;FIXME what if bounds are violated?
                                  ((case v :covariant In :contravariant Un)
                                   [l r]
                                   opts))
                                (:variances t1) (:poly? t1) (:poly? t2))
                           opts)

                (and (r/CountRange? t1)
                     (r/CountRange? t2))
                (intersect-CountRange t1 t2)

                (not (overlap t1 t2 opts)) bottom

                (ind/subtype? t1 t2 opts) t1
                (ind/subtype? t2 t1 opts) t2
                :else (do
                        #_(prn "failed to eliminate intersection" (make-Intersection [t1 t2] opts))
                        (make-Intersection [t1 t2] opts)))]
        (some-> intersect-cache (swap! assoc cache-key t))
        ;(prn "intersect miss" (ind/unparse-type t opts))
        t))))

;;TODO accepts IReduce
(t/ann ^:no-check flatten-intersections [(t/Seqable r/Type) -> (t/Set r/Type)])
(defn flatten-intersections
  "Does not resolve types."
  [types]
  {:post [(set? %)
          (sorted? %)]}
  (reduce (fn flatten-1 [acc type]
            (if (r/Intersection? type)
              (reduce flatten-1 acc (:types type))
              (conj acc (r/assert-Type type))))
          (sorted-set) types))

;;TODO accepts IReduce
(t/ann ^:no-check flatten-unions [(t/Seqable r/Type) -> (t/Set r/Type)])
(defn flatten-unions
  "Does not resolve types."
  [types]
  {:post [(set? %)
          (sorted? %)]}
  (reduce (fn flatten-1 [acc type]
            (if (r/Union? type)
              (reduce flatten-1 acc (:types type))
              (conj acc (r/assert-Type type))))
          (sorted-set) types))

(t/ann ^:no-check In [(t/Seqable r/Type) t/Any :-> r/Type])
(defn In [types {::vs/keys [no-simpl] :as opts}]
  {:post [(r/Type? %)]}
  (if no-simpl
    (make-Intersection types opts)
    (let [res (let [ts (flatten-intersections (eduction (map (comp #(fully-resolve-type % opts) r/assert-Type)) types))]
                (cond
                  ; empty intersection is Top
                  (empty? ts) r/-any

                  ; intersection containing Bottom is Bottom
                  (contains? ts bottom) r/-nothing

                  (= 1 (count ts)) (first ts)

                  ; try and simplify to disjunctive normal form
                  ; normalise (I t1 t2 (t/U t3 t4))
                  ; to (t/U (I t1 t2 t3) (t/I t1 t2 t4))
                  :else (let [;_ (prn "before ts" ts)
                              ;; only move common elements of all inner unions to outer union
                              ; (I (U nil t1) (U nil t2))
                              ; =>
                              ; (U nil (I t1 t2))
                              ; rather than 
                              ; (U nil t1 t2)
                              inner-unions (filter r/Union? ts)
                              ;; only move if there's more than one union
                              inner-unions (when (next inner-unions) inner-unions)
                              outer-union-elements (some->> inner-unions
                                                            (map (comp set :types))
                                                            (apply set/intersection)
                                                            not-empty)
                              ts (cond-> ts
                                   outer-union-elements
                                   (->> (eduction
                                          (keep (fn [t]
                                                  (when (r/Union? t)
                                                    ;; remove inner union if all its elements have been moved to outer union
                                                    ;; (I (U t1 t2) (U t1 t2 t3))
                                                    ;; =>
                                                    ;; (U t1 t2 t3)
                                                    ;; not 
                                                    ;; (U t1 t2 (U) t3)
                                                    (some-> (not-empty
                                                              (set/difference (set (:types t)) outer-union-elements))
                                                            (Un opts))))))
                                        flatten-intersections))
                              ;_ (prn "ts" ts)
                              ;_ (prn "outer-union-elements" outer-union-elements)
                              {:keys [unions count-ranges hmaps tapps non-unions]}
                              (group-by (fn [t]
                                          (cond
                                            (r/Union? t) :unions
                                            (r/CountRange? t) :count-ranges
                                            (r/HeterogeneousMap? t) :hmaps
                                            :else :non-unions))
                                        ts)
                              non-unions (concat non-unions
                                                 ;; FIXME hmm some of these can return unions... 
                                                 (some->> count-ranges
                                                          (reduce intersect-CountRange)
                                                          list)
                                                 (some->> hmaps
                                                          (reduce #(intersect-HMap %1 %2 opts))
                                                          list))
                              ;_ (prn "unions" unions)
                              ;_ (prn "non-unions" non-unions)
                              ;intersect all the non-unions to get a possibly-nil type
                              intersect-non-unions (some->> (seq non-unions)
                                                            (reduce #(intersect %1 %2 opts)))
                              ;_ (prn "intersect-non-unions" intersect-non-unions)
                              ;if we have an intersection above, use it to update each
                              ;member of the unions we're intersecting
                              flat-unions (flatten-unions (concat unions outer-union-elements))
                              ;_ (prn "flat-unions" flat-unions)
                              intersect-union-ts (cond
                                                   intersect-non-unions
                                                   (if (seq flat-unions)
                                                     (reduce (fn [acc union-m]
                                                               (conj acc (intersect intersect-non-unions union-m opts)))
                                                             #{} flat-unions)
                                                     #{intersect-non-unions})

                                                   :else flat-unions)
                              ;_ (prn "intersect-union-ts" intersect-union-ts)
                              _ (assert (every? r/Type? intersect-union-ts)
                                        intersect-union-ts)]
                          (Un (concat outer-union-elements intersect-union-ts) opts))))]
      res)))

(declare TypeFn* instantiate-typefn abstract-many instantiate-many)

;; JS Nominal

(t/ann ^:no-check JSNominal*
  (t/IFn [t/Sym t/Any -> r/Type]
         [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Seqable Bounds) t/Any -> r/Type]))
(defn JSNominal* 
  ([name opts] (JSNominal* nil nil nil name nil))
  ([names variances poly? name bnds opts]
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
       (TypeFn* names variances bnds p opts)
       p))))

(declare TypeFn-fresh-symbols*)

(def ^:private get-jsnominal #((requiring-resolve 'typed.cljs.checker.jsnominal-env/get-jsnominal) %1 %2))

(t/ann ^:no-check JSNominal-of (t/IFn [t/Sym t/Any -> r/Type]
                                      [t/Sym (t/Seqable r/Type) t/Any -> r/Type]))
(defn JSNominal-of
  ([sym opts] (JSNominal-of sym nil opts))
  ([sym args opts]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (get-jsnominal sym opts)]
     (assert ((some-fn r/TypeFn? r/JSNominal? nil?) p))
     ; parameterised nominals must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic JS nominal " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args {} opts)
       (r/JSNominal? p) p
       ; allow unannotated nominals if unparameterised
       :else (JSNominal* sym opts)))))

;Datatype

(t/ann ^:no-check DataType*
  [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Seqable Bounds) t/Any -> r/Type])
(defn DataType* [names variances poly? name bnds fields record? opts]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? name)]
   :post [(r/Type? %)]}
  (let [p (r/DataType-maker name (seq variances) (seq poly?) fields record?)]
    (if (seq variances)
      (TypeFn* names variances bnds p opts)
      p)))

(t/ann ^:no-check DataType-of (t/IFn [t/Sym t/Any -> r/Type]
                                     [t/Sym (t/Seqable r/Type) t/Any -> r/Type]))
(defn DataType-of
  ([sym opts] (DataType-of sym nil opts))
  ([sym args opts]
   {:pre [(symbol? sym)
          (every? r/Type? args)
          (map? opts)]
    :post [(r/Type? %)]}
   (let [checker (env/checker opts)
         p (dtenv/get-datatype checker sym opts)]
     (assert ((some-fn r/TypeFn? r/DataType? nil?) p))
     ; parameterised datatypes must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic datatype " sym))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args {} opts)
       (r/DataType? p) p
       ; allow unannotated datatypes if unparameterised
       :else (DataType* nil nil nil sym nil {} false opts)))))


;; Protocol

(t/ann ^:no-check Protocol*
  [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym t/Sym (t/Map t/Sym r/Type) (t/Seqable Bounds) t/Any -> r/Type])
(defn Protocol* [names variances poly? the-var on-class methods bnds opts]
  {:pre [(every? symbol? names)
         (every? r/variance? variances)
         (= (count variances) (count poly?))
         (every? r/Type? poly?)
         (every? r/Bounds? bnds)
         (symbol? the-var)
         (symbol? on-class)]
   :post [(r/Type? %)]}
  (let [p (r/Protocol-maker the-var (seq variances) (seq poly?) on-class methods)]
    (if (seq variances)
      (TypeFn* names variances bnds p opts)
      p)))

(t/ann ^:no-check Protocol-var->on-class [t/Sym -> t/Sym])
(defn Protocol-var->on-class 
  "Given the var symbol of a protocol, returns the corresponding
  class the protocol is based on as a munged symbol."
  [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (symbol (str (munge (namespace sym)) \. (name sym))))

(t/ann ^:no-check Protocol-interface->on-var [t/Sym t/Any -> t/Sym])
(defn Protocol-interface->on-var
  "Given the interface symbol of a protocol, returns the corresponding
  var the protocol is based on as a symbol, or nil if none. Assumes the interface is possible
  to demunge. Only useful for Clojure implementation."
  [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol? nil?) %)]}
  (impl/assert-clojure opts)
  (let [clstr (repl/demunge (str sym))
        ;; replace final dot with /
        varstr (str/replace clstr #"\.([^.]+)$" "/$1")
        var-sym (symbol varstr)]
    (when (var? (resolve var-sym))
      var-sym)))

(t/ann resolve-Protocol [(t/U Satisfies Protocol) t/Any -> t/AnyVar])
(defn resolve-Protocol
  [{:keys [the-var] :as t} opts]
  {:pre [((some-fn r/Protocol? r/Satisfies?) t)]
   :post [(var? %)]}
  (impl/assert-clojure opts)
  (let [v (requiring-resolve the-var)]
    (assert (var? v) (str "Cannot resolve protocol: " the-var))
    v))

(t/ann Protocol-normal-extenders [(t/U Satisfies Protocol) t/Any -> (t/Set (t/U nil Class))])
(defn Protocol-normal-extenders [p opts]
  (set (extenders @(resolve-Protocol p opts))))

(t/ann ^:no-check Protocol-of [t/Sym (t/Seqable r/Type) :? t/Any -> r/Type])
(defn Protocol-of
  ([sym opts] (Protocol-of sym nil opts))
  ([sym args opts]
   {:pre [(symbol? sym)
          (every? r/Type? args)]
    :post [(r/Type? %)]}
   (let [p (prenv/get-protocol (env/checker opts) sym opts)]
     (assert ((some-fn r/TypeFn? r/Protocol? nil?) p))
     ; parameterised protocols must be previously annotated
     (assert (or (r/TypeFn? p) (empty? args))
             (str "Cannot instantiate non-polymorphic Protocol " sym
                  " "
                  (if p
                    "(protocol is annotated)"
                    "(protocol is not annotated)")))
     (cond 
       (r/TypeFn? p) (instantiate-typefn p args {} opts)
       (r/Protocol? p) p
       ; allow unannotated protocols if unparameterised
       :else (r/Protocol-maker sym nil nil (Protocol-var->on-class sym) {})))))

;; RClass

;smart constructor
(t/ann ^:no-check RClass* 
  (t/IFn [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Map t/Sym r/Type) t/Any -> r/Type]
         [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Map t/Sym r/Type) (t/Set r/Type) t/Any -> r/Type]
         [(t/Seqable t/Sym) (t/Seqable r/Variance) (t/Seqable r/Type) t/Sym (t/Map t/Sym r/Type) (t/Set r/Type) (t/Seqable Bounds) t/Any -> r/Type]))
(defn RClass*
  ([names variances poly? the-class replacements opts]
   (RClass* names variances poly? the-class replacements (r/sorted-type-set []) opts))
  ([names variances poly? the-class replacements unchecked-ancestors opts]
   (RClass* names variances poly? the-class replacements unchecked-ancestors (repeat (count names) r/no-bounds) opts))
  ([names variances poly? the-class replacements unchecked-ancestors bnds opts]
   {:pre [(every? symbol? names)
          (every? r/variance? variances)
          (= (count variances) (count poly?) (count bnds))
          (every? r/Type? poly?)
          (every? r/Bounds? bnds)
          (symbol? the-class)]
    :post [((some-fn r/TypeFn? r/RClass?) %)]}
   (let [replacements ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/abstract-rclass-replacements)
                       the-class
                       names
                       bnds
                       replacements
                       opts)
         unchecked-ancestors ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/abstract-rclass-ancestors)
                              the-class
                              names
                              bnds
                              unchecked-ancestors
                              opts)]
     (if (seq variances)
       (TypeFn* names variances bnds (r/RClass-maker the-class poly? variances replacements unchecked-ancestors) opts)
       (r/RClass-maker the-class nil nil replacements unchecked-ancestors)))))

(t/ann ^:no-check isa-DataType? [(t/U t/Sym Class) -> t/Any])
(defn isa-DataType? [sym-or-cls]
  #_{:pre [((some-fn symbol? class?) sym-or-cls)]}
  (let [cls (if (symbol? sym-or-cls)
              (coerce/symbol->Class sym-or-cls)
              (do (assert (class? sym-or-cls))
                  sym-or-cls))]
    (if (identical? cls clojure.lang.IType)
      false
      (.isAssignableFrom clojure.lang.IType cls))))

(t/ann ^:no-check isa-Record? [(t/U t/Sym Class) -> t/Any])
(defn isa-Record? [sym-or-cls]
  #_{:pre [((some-fn symbol? class?) sym-or-cls)]}
  (let [cls (if (symbol? sym-or-cls)
              (coerce/symbol->Class sym-or-cls)
              (do (assert (class? sym-or-cls))
                  sym-or-cls))]
    (if (identical? cls clojure.lang.IRecord)
      false
      (.isAssignableFrom clojure.lang.IRecord cls))))

(t/ann ^:no-check Record->HMap [DataType t/Any -> r/Type])
(defn Record->HMap [r opts]
  {:pre [(r/Record? r)]
   :post [(r/Type? %)]}
  (let [kf (zipmap (map (comp r/-val keyword) (keys (:fields r)))
                   (vals (:fields r)))]
    (make-HMap opts {:mandatory kf})))

(t/ann ^:no-check RClass-of (t/IFn [(t/U t/Sym Class) t/Any -> r/Type]
                                   [(t/U t/Sym Class) (t/Seqable r/Type) t/Any -> r/Type]))
(defn RClass-of
  ([sym-or-cls opts] (RClass-of sym-or-cls nil opts))
  ([sym-or-cls args {::keys [RClass-of-cache] :as opts}]
   {:pre [#_
          ((some-fn class? symbol?) sym-or-cls)
          ;; checked by instantiate-typefn
          #_
          (every? r/Type? args)
          (or (nil? args) (sequential? args))
          (map? opts)]
    :post [;; checked by final cond
           ((some-fn r/RClass? r/DataType?) %)]}
   (let [checker (env/checker opts)
         cls? (class? sym-or-cls)
         sym (if cls?
               (coerce/Class->symbol sym-or-cls)
               (do #_(assert (symbol? sym-or-cls)) ;; checked by dtenv/get-datatype
                   sym-or-cls))
         args? (seq args)
         cache-key (if args?
                     [sym args]
                     sym)]
     (or (when RClass-of-cache (@RClass-of-cache cache-key))
         (let [rc (or (dtenv/get-datatype checker sym opts)
                      (rcls/get-rclass checker sym opts))
               ;; checked by dtenv/get-datatype and rcls/get-rclass
               ;_ (assert ((some-fn r/TypeFn? r/RClass? r/DataType? nil?) rc))
               res (if (r/TypeFn? rc)
                     (let [res (instantiate-typefn rc args {} opts)]
                       (assert (or (r/RClass? res) (r/DataType? res)))
                       res)
                     (do (when args?
                           (err/int-error
                             (str "Cannot instantiate non-polymorphic RClass " sym
                                  (when-some [current-RClass-super (::current-RClass-super opts)]
                                    (str " when checking supertypes of RClass " current-RClass-super)))
                             opts))
                         (if (nil? rc)
                           (let [cls (if cls?
                                       sym-or-cls
                                       (coerce/symbol->Class sym-or-cls))]
                             (if (isa-DataType? cls)
                               (do (println (str "WARNING: Assuming unannotated Clojure type " sym
                                                 " is a datatype"))
                                   (flush)
                                   (when (isa-Record? cls)
                                     (println (str "WARNING: " sym " is probably a record because it extends IRecord."
                                                   " Annotate with ann-record above the first time it is parsed"))
                                     (flush))
                                   (r/DataType-maker sym nil nil (array-map) (isa-Record? cls)))
                               (r/RClass-maker sym nil nil {} (sorted-set))))
                           (do #_(assert (or (r/RClass? res) (r/DataType? res)))
                               rc))))]
           (some-> RClass-of-cache (swap! assoc cache-key res))
           res)))))

(t/ann ^:no-check most-general-on-variance [(t/Seqable r/Variance) (t/Seqable Bounds) t/Any -> r/Type])
(defn most-general-on-variance [variances bnds opts]
  (mapv (fn [variance {:keys [upper-bound lower-bound] :as bnd}]
          (case variance
            (:constant :covariant) upper-bound
            :contravariant lower-bound
            (err/int-error (str "Cannot find most general type for variance: " (pr-str variance)) opts)))
        variances bnds))

(declare TypeFn-bbnds* TypeFn-fresh-symbols*)

(def variant-variances #{:constant :covariant :contravariant})

;FIXME rename to RClass-with-unknown-params
(t/ann ^:no-check RClass-of-with-unknown-params
       (t/IFn [(t/U t/Sym Class) t/Any -> r/Type]
              [(t/U t/Sym Class) (t/Nilable (t/HMap :optional {:warn-msg (t/U nil t/Str)})) t/Any -> r/Type]))
(defn RClass-of-with-unknown-params
  ([sym-or-cls opts] (RClass-of-with-unknown-params sym-or-cls nil opts))
  ([sym-or-cls {:keys [warn-msg]} opts]
   #_{:pre [((some-fn class? symbol?) sym-or-cls)]
      :post [((some-fn r/RClass? r/DataType? r/Instance?) %)]}
   (let [checker (env/checker opts)
         sym (if (class? sym-or-cls)
               (coerce/Class->symbol sym-or-cls)
               (do #_(assert (symbol? sym-or-cls)) ;; checked by dtenv/get-datatype
                   sym-or-cls))
         rc (or (dtenv/get-datatype checker sym opts)
                (rcls/get-rclass checker sym opts))]
     (if (r/TypeFn? rc)
       (let [{:keys [variances]} rc]
         (when warn-msg
           (println (str "WARNING: " warn-msg ": " sym)))
         (if (every? variant-variances variances)
           (RClass-of sym-or-cls
                      (let [syms (TypeFn-fresh-symbols* rc)]
                        (most-general-on-variance variances
                                                  (TypeFn-bbnds* syms rc opts)
                                                  opts))
                      opts)
           (r/Instance-maker sym)))
       (or rc (RClass-of sym-or-cls opts))))))

(t/ann ^:no-check Instance-of [(t/U t/Sym Class) t/Any -> r/Type])
(defn Instance-of
  [sym-or-cls opts]
  (RClass-of-with-unknown-params sym-or-cls opts))

(t/ann ^:no-check DataType-with-unknown-params [t/Sym t/Any -> r/Type])
(defn DataType-with-unknown-params
  ([sym opts]
   {:pre [(symbol? sym)]
    :post [((some-fn r/DataType?) %)]}
   (let [checker (env/checker opts)
         t (dtenv/get-datatype checker sym opts)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t opts)
                                            opts)))]
     (DataType-of sym args opts))))

(t/ann ^:no-check JSNominal-with-unknown-params [t/Sym t/Any -> r/Type])
(defn JSNominal-with-unknown-params
  ([sym opts]
   {:pre [(symbol? sym)]
    :post [((some-fn r/JSNominal?) %)]}
   (let [t (get-jsnominal sym opts)
         args (when (r/TypeFn? t)
                (let [syms (TypeFn-fresh-symbols* t)]
                  (most-general-on-variance (:variances t)
                                            (TypeFn-bbnds* syms t opts)
                                            opts)))]
     (JSNominal-of sym args opts))))

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

(t/ann ^:no-check Protocol-with-unknown-params [t/Sym t/Any -> r/Type])
(defn Protocol-with-unknown-params
  ([sym opts]
   {:pre [(symbol? sym)]
    :post [((some-fn r/Protocol? r/Satisfies?) %)]}
   (let [t (prenv/get-protocol (env/checker opts) sym opts)]
     (cond
       (r/TypeFn? t) (let [{:keys [variances]} t]
                       (if (every? variant-variances variances)
                         (Protocol-of sym
                                      (let [syms (TypeFn-fresh-symbols* t)]
                                        (most-general-on-variance variances
                                                                  (TypeFn-bbnds* syms t opts)
                                                                  opts))
                                      opts)
                         (r/Satisfies-maker sym (Protocol-var->on-class sym))))
       :else (or t (Protocol-of sym opts))))))

(defn Datatype-ancestors
  "Returns a set of Types which are ancestors of this datatype.
  Only useful when checking Clojure. This is because we need to query datatypes
  for their ancestors, as sometimes datatypes do not appear in `extenders`
  of a protocol (this happens when a protocol is extend directly in a deftype)."
  [{:keys [the-class] :as dt} opts]
  {:pre [(r/DataType? dt)]}
  (impl/assert-clojure opts)
  (let [checker (env/checker opts)
        overidden-by (fn [sym o]
                       ;(prn "overriden by" sym (class o) o)
                       (cond
                         ((some-fn r/DataType? r/RClass?) o)
                         (when (= sym (:the-class o))
                           o)
                         (r/Protocol? o)
                         ; protocols are extended via their interface if they
                         ; show up in the ancestors of the datatype
                         (when (= sym (:on-class o))
                           o)))
        overrides (map #(fully-resolve-type % opts)
                       ((requiring-resolve 'typed.cljc.checker.datatype-ancestor-env/get-datatype-ancestors)
                        checker
                        dt
                        opts))
        ;_ (prn "datatype name" the-class)
        ;_ (prn "datatype overrides" overrides)
        _ (assert (every? (some-fn r/Protocol? r/DataType? r/RClass?) overrides)
                  "Overriding datatypes to things other than datatypes, protocols and classes NYI")
        ; the classes that this datatype extends.
        ; No vars should occur here because protocol are extended via their interface.
        normal-asyms (->> (ancestors (coerce/symbol->Class the-class))
                          (filter class?)
                          (map coerce/Class->symbol))
        ;_ (prn "normal-asyms" normal-asyms)
        post-override (set
                        (for [sym normal-asyms]
                          ; either we override this ancestor ...
                          (if-let [o (some #(overidden-by sym %) overrides)]
                            o
                            (if-some [protocol-varsym (Protocol-interface->on-var sym opts)]
                              ;... or we make a protocol type from the varified interface ...
                              (Protocol-with-unknown-params protocol-varsym opts)
                              ;... or we make an RClass from the actual ancestor.
                              (RClass-of-with-unknown-params sym opts)))))]
    post-override))

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

(t/ann ^:no-check inst-and-subst [(t/U r/Type Scope) (t/Seqable r/Type) t/Any -> r/Type])
(defn inst-and-subst
  "Instantiate target type with ts number of
  free names. Target must be wrapped in ts number
  of Scopes. Substitutes the temporary names with
  types ts."
  [target ts opts]
  {:pre [((some-fn r/Type? r/Scope?) target)
         (every? r/Type? ts)
         (every? #(ind/has-kind? % r/no-bounds opts) ts)]
   :post [(r/Type? %)]}
  (let [subst-all @(subst-all-var)
        ; these names are eliminated immediately, they don't need to be
        ; created with fresh-symbol
        names (mapv (fn [_] (gensym "inst-and-subst")) (range (count ts)))
        opts (free-ops/with-bounded-frees opts (zipmap (map r/make-F names)
                                                       ;; asserted as precondition
                                                       (repeat r/no-bounds)))
        t (r/assert-Type (instantiate-many names target opts))
        subst (make-simple-substitution names ts)]
    (subst-all subst t opts)))

(t/ann ^:no-check RClass-replacements* [RClass t/Any -> (t/Map t/Sym r/Type)])
(defn RClass-replacements*
  "Return the replacements map for the RClass"
  [rcls opts]
  {:pre [(r/RClass? rcls)]
   :post [((con/hash-c? symbol? r/Type?) %)]}
  ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/rclass-replacements) rcls opts))

(t/ann ^:no-check RClass-unchecked-ancestors* [RClass -> (t/SortedSet r/Type)])
(defn RClass-unchecked-ancestors*
  [rcls opts]
  {:pre [(r/RClass? rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  ((requiring-resolve 'typed.clj.checker.rclass-ancestor-env/rclass-ancestors) rcls opts))

(t/ann ^:no-check RClass-supers* [RClass t/Any -> (t/SortedSet r/Type)])
(defn RClass-supers*
  "Return a set of ancestors to the RClass"
  [{:keys [the-class] :as rcls} {::keys [supers-cache] :as opts}]
  {:pre [((some-fn r/RClass? r/Instance?) rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  (let [rclass? (r/RClass? rcls)
        cache-key rcls]
    (or (when supers-cache (@supers-cache cache-key))
        (let [cls (-> the-class coerce/symbol->Class)]
          (if (identical? Object cls)
            (sorted-set (RClass-of Object opts))
            (let [replacements (if rclass?
                                 (RClass-replacements* rcls opts)
                                 {})
                  java-bases (into #{} (map coerce/Class->symbol) (bases cls))
                  warn-msg-opts {:warn-msg (when rclass?
                                             (when (.contains (str the-class) "clojure.lang")
                                               (str "RClass ancestor for " (pr-str rcls) " defaulting "
                                                    "to most general parameters")))} 
                  res (as-> (sorted-set) res
                        (let [opts (assoc opts ::current-RClass-super the-class)]
                          (reduce (fn [res csym]
                                    (if (replacements csym)
                                      res
                                      (let [r (RClass-of-with-unknown-params csym warn-msg-opts opts)]
                                        (-> res (conj r) (into (RClass-supers* r opts))))))
                                  res java-bases))
                        (reduce-kv (fn [res csym t]
                                     (if (and rclass? (not (java-bases csym)))
                                       (err/int-error (str "Bad RClass replacement for " the-class ": " csym) opts)
                                       (let [t (fully-resolve-type t opts)]
                                         (-> res (conj t) (into (RClass-supers* t opts))))))
                                   res replacements)
                        (conj res (RClass-of Object opts))
                        (into res (when rclass? (RClass-unchecked-ancestors* rcls opts))))
                  ;;FIXME just need to do this once generically at RClass declaration
                  _ (when-some [rclass-ancestor-groups (not-empty
                                                         (into #{} (filter (comp next val))
                                                               (group-by :the-class (filter r/RClass? res))))]
                      (err/int-error
                        (str "Found clashing supertypes for RClass ancestors of " (ind/unparse-type rcls opts) ": "
                             (str/join "\n" (map (fn [grp] (str/join ", " (map pr-str grp)))
                                                 (vals rclass-ancestor-groups))))
                        opts))]
              ;(prn "supers" the-class res)
              (when-not (<= (count (filter (some-fn r/FnIntersection? r/Poly? r/PolyDots?) res))
                            1)
                (let [not-replaced (set/difference java-bases (set (keys replacements)))]
                  (err/int-error 
                    (str "Found more than one function supertype for RClass " (ind/unparse-type rcls opts) ": \n"
                         (mapv #(ind/unparse-type % opts) (filter (some-fn r/FnIntersection? r/Poly? r/PolyDots?) res))
                         "\nReplacements:" (into {}
                                                 (map (t/fn [[k v] :- '[t/Any r/Type]] [k (ind/unparse-type v opts)]))
                                                 replacements)
                         "\nNot replaced:" not-replaced)
                    opts)))
              (t/tc-ignore
                (some-> supers-cache (swap! assoc cache-key res)))
              res))))))

(t/ann ^:no-check DataType-fields* [DataType -> (t/Map t/Sym r/Type)])
(defn DataType-fields* [^DataType dt]
  {:pre [(r/DataType? dt)]
   :post [((plat-con/array-map-c? symbol? r/Type?) %)]}
  (:fields dt))

(defn extra-Record-fields [dt opts]
  {:pre [(r/Record? dt)]
   :post [((plat-con/array-map-c? symbol? r/Type?) %)]}
  (let [fields (DataType-fields* dt)]
    (array-map '__meta
               (-name `t/Nilable (-name `t/Map r/-any r/-any))
               '__extmap
               (Un [r/-nil (In [(-name `t/NonEmptyCount)
                                (-partial-hmap opts {}
                                               (into #{} (map (comp r/-val keyword))
                                                     (keys fields)))]
                               opts)]
                   opts))))

;; TypeFn

;smart constructor
(t/ann ^:no-check TypeFn*
       [(t/Seqable t/Sym)
        r/TFnVariancesMaybeFn
        (t/Seqable r/Kind) (t/U r/Type r/Kind)
        (t/HMap :optional {:meta (t/U nil (t/Map t/Any t/Any))}) :?
        t/Any -> r/Type])
(defn TypeFn*
  ([names variances bbnds body opts] (TypeFn* names variances bbnds body {} opts))
  ([names variances bbnds body {:keys [meta]} opts]
  {:pre [(seq names)
         (every? symbol names)
         (or (fn? variances)
             (every? r/variance? variances))
         (every? r/Kind? bbnds)
         (apply = (map count (cond-> [names bbnds]
                               (not (fn? variances)) (conj variances))))
         ((some-fn r/TypeFn? r/Type? r/Kind?) body)
         ((some-fn nil? map?) meta)]
   :post [(r/Type? %)]}
  (let [original-names (mapv (comp r/F-original-name r/make-F) names)
        ab (let [opts (free-ops/with-bounded-frees opts
                        (zipmap (map r/make-F names) bbnds))]
             #(abstract-many names % opts))
        t (r/TypeFn-maker (count names)
                          variances
                          (mapv ab bbnds)
                          (ab body)
                          meta)]
    (with-original-names t original-names))))

;smart destructor
(t/ann ^:no-check TypeFn-body* [(t/Seqable t/Sym) (t/Vec r/Kind) TypeFn t/Any -> r/Type])
(defn TypeFn-body* [names bbnds typefn opts]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]}
  (assert (= (:nbound typefn) (count names)) "Wrong number of names")
  (instantiate-many names (:scope typefn)
                    (free-ops/with-bounded-frees opts (zipmap (map r/make-F names) bbnds))))

(t/ann ^:no-check TypeFn-bbnds* [(t/Seqable t/Sym) TypeFn t/Any -> (t/Vec r/Kind)])
(defn TypeFn-bbnds* [names typefn opts]
  {:pre [(every? symbol? names)
         (r/TypeFn? typefn)]
   :post [(every? r/Kind? %)]}
  (assert (= (:nbound typefn) (count names)) "Wrong number of names")
  (mapv #(instantiate-many names % opts)
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
                         (t/HMap :optional {:original-names (t/Seqable t/Sym)
                                            :named (t/U nil (t/Map t/Sym t/Int))})
                         :?
                         t/Any
                         -> r/Type])
(defn Poly*
  ([names bbnds body opts] (Poly* names bbnds body {} opts))
  ([names bbnds body {:keys [original-names named]
                      :or {original-names 
                           (map (comp r/F-original-name r/make-F) names)}}
    opts]
   {:pre [(every? simple-symbol? names)
          (every? r/Bounds? bbnds)
          (r/Type? body)
          (every? simple-symbol? original-names)
          (apply = (map count [names bbnds original-names]))
          ((some-fn nil? map?) named)]}
   (if (empty? names)
     body
     (let [ab #(abstract-many names % opts)
           v (r/Poly-maker (count names)
                           (mapv ab bbnds)
                           (ab body)
                           (or named {}))]
       (with-original-names v original-names)))))

(t/ann ^:no-check Poly-free-names* [Poly -> (t/Seqable t/Sym)])
(defn Poly-free-names* [poly]
  {:pre [(r/Poly? poly)]
   :post [((some-fn nil?
                    (every-pred seq (con/every-c? symbol?)))
           %)]}
  (get-original-names poly))

(t/ann ^:no-check Poly-fresh-symbols* [Poly -> (t/Vec t/Sym)])
(defn Poly-fresh-symbols* [poly]
  {:pre [(r/Poly? poly)]
   :post [((every-pred seq (con/every-c? symbol?)) %)]}
  ;(prn "Poly-fresh-symbols*" (:scope poly))
  (mapv fresh-symbol (or (Poly-free-names* poly)
                         ;(assert nil "no poly free names")
                         (repeatedly (:nbound poly) #(gensym "Poly-fresh-sym")))))

(t/ann ^:no-check Poly-bbnds* [(t/Seqable t/Sym) Poly t/Any -> (t/Vec Bounds)])
(defn Poly-bbnds* [names poly opts]
  {:pre [(every? symbol? names)
         (r/Poly? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (mapv #(instantiate-many names % opts)
        (:bbnds poly)))

;smart destructor
(t/ann ^:no-check Poly-body* [(t/Seqable t/Sym) Poly t/Any -> r/Type])
(defn Poly-body* [names poly opts]
  {:pre [(every? symbol? names)
         (r/Poly? poly)]}
  (let [bbnds (Poly-bbnds* names poly opts)]
    (assert (= (:nbound poly) (count names)) "Wrong number of names")
    (instantiate-many names (:scope poly)
                      (free-ops/with-bounded-frees
                        opts 
                        (zipmap (map r/make-F names) bbnds)))))

;; PolyDots

;smart constructor
(t/ann ^:no-check PolyDots* [(t/Seqable t/Sym) (t/Seqable r/Kind) r/Type 
                             (t/HMap :optional {:original-names (t/Seqable t/Sym)
                                                :named (t/U nil (t/Map t/Sym t/Int))})
                             :?
                             t/Any
                             -> r/Type])
(defn PolyDots*
  ([names bbnds body opts] (PolyDots* names bbnds body {} opts))
  ([names bbnds body {:keys [original-names named] 
                      :or {original-names (map (comp r/F-original-name r/make-F) names)}
                      :as opt}
    opts]
   {:pre [(or (empty? opts)
              (empty? (dissoc opt :original-names :named)))
          (every? symbol names)
          (every? r/Kind? bbnds)
          (r/Type? body)
          ((some-fn nil? map?) named)]}
   (assert (= (count names) (count bbnds)) "Wrong number of names")
   (if (empty? names)
     body
     (let [ab #(abstract-many names % opts)
           v (r/PolyDots-maker (count names) 
                               (mapv ab bbnds)
                               (ab body)
                               (or named {}))]
       (with-original-names v original-names)))))

;smart destructor
(t/ann ^:no-check PolyDots-body* [(t/Seqable t/Sym) Poly t/Any -> r/Type])
(defn PolyDots-body* [names poly opts]
  {:pre [(every? symbol? names)
         (r/PolyDots? poly)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (instantiate-many names (:scope poly) opts))

(t/ann ^:no-check PolyDots-bbnds* [(t/Seqable t/Sym) Poly t/Any -> (t/Vec r/Kind)])
(defn PolyDots-bbnds* [names poly opts]
  {:pre [(every? symbol? names)
         (r/PolyDots? poly)]
   :post [(vector? %)]}
  (assert (= (:nbound poly) (count names)) "Wrong number of names")
  (mapv #(instantiate-many names % opts)
        (:bbnds poly)))

(t/ann ^:no-check PolyDots-free-names* [Poly -> (t/Seqable t/Sym)])
(defn ^:private PolyDots-free-names* [poly]
  {:pre [(r/PolyDots? poly)]
   :post [((some-fn nil? 
                    (every-pred seq (con/every-c? symbol?))) 
           %)]}
  (get-original-names poly))

(t/ann ^:no-check PolyDots-fresh-symbols* [Poly -> (t/Vec t/Sym)])
(defn PolyDots-fresh-symbols* [poly]
  {:pre [(r/PolyDots? poly)]
   :post [((every-pred seq (con/every-c? symbol?)) %)]}
  (mapv fresh-symbol (or (PolyDots-free-names* poly)
                         (repeatedly (:nbound poly) #(gensym "PolyDots-fresh-symbols*")))))

;; Instantiate ops

(t/ann ^:no-check make-simple-substitution [(t/Seqable t/Sym) (t/Seqable r/Type) -> crep/SubstMap])
(defn make-simple-substitution [vs ts]
  {:pre [(= (count vs)
            (count ts))]}
  (persistent!
   (perf/reduce (fn [acc v t]
                  {:pre [(symbol? v)
                         (r/Type? t)]}
                  (assoc! acc v (crep/t-subst-maker t r/no-bounds)))
                (transient {})
                vs ts)))

(t/ann ^:no-check instantiate-typefn [TypeFn (t/Seqable r/Type) t/Any t/Any -> r/Type])
(defn instantiate-typefn [t types {:keys [names tapp]
                                   :or {names (TypeFn-fresh-symbols* t)}}
                          opts]
  ;(assert (not (::vs/no-simpl opts)))
  (let [subst-all @(subst-all-var)
        cnt (count types)]
    (when-not (r/TypeFn? t) (err/int-error (str "instantiate-typefn requires a TypeFn: " (ind/unparse-type t opts)) opts))
    (when-not (= (:nbound t) cnt)
      (let [opts (assoc opts ::vs/current-env (-> tapp meta :env))]
        (err/int-error
          (str "Wrong number of arguments passed to type function. Expected "
               (:nbound t) ", actual " cnt ": "
               (ind/unparse-type t opts) " " (mapv #(ind/unparse-type % opts) types)
               "\n\nin: "
               (pr-str (or (-> tapp meta :syn)
                           (list* t types))))
          opts)))
    (let [bbnds (TypeFn-bbnds* names t opts)
          body (TypeFn-body* names bbnds t opts)
          opts (-> opts (free-ops/with-bounded-frees (zipmap (map r/make-F names) bbnds)))
          ;;check bounds
          _ (perf/reduce
              (fn [_ argn nm type bnd]
                {:pre [(r/Type? type)]}
                (when-not (ind/has-kind? type bnd opts)
                  (let [opts (assoc opts ::vs/current-env (-> tapp meta :env))]
                    (err/tc-error (str "Type function argument number " argn
                                       " (" (r/F-original-name (r/make-F nm)) ")"
                                       " has kind " (pr-str bnd)
                                       " but given " (pr-str type)
                                       (when (r/F? type)
                                         (if-some [kind (free-ops/free-with-name-bnds (:name type) opts)]
                                           (str " with kind " kind)
                                           (str " with missing bounds")))
                                       "\n\nin: "
                                       (pr-str (or (-> tapp meta :syn)
                                                   (list* t types))))
                                  opts))))
              nil
              (range 1 (inc cnt)) names types bbnds)]
      (subst-all (make-simple-substitution names types) body opts))))

(t/ann ^:no-check instantiate-poly [Poly (t/Seqable r/Type) t/Any -> r/Type])
(defn instantiate-poly [t types opts]
  (let [subst-all @(subst-all-var)]
    (cond
      (r/Poly? t) (let [_ (when-not (= (:nbound t) (count types)) 
                            (err/int-error 
                              (str "Wrong number of types (" (count types) 
                                   ") used to instantiate polymorphic type: "
                                   (ind/unparse-type t opts)
                                   (when-some [current-RClass-super (::current-RClass-super opts)]
                                     (str " when checking ancestors of " current-RClass-super)))
                              opts))
                        nms (Poly-fresh-symbols* t)
                        bbnds (Poly-bbnds* nms t opts)
                        body (Poly-body* nms t opts)
                        opts (-> opts (free-ops/with-bounded-frees
                                        (zipmap (map r/make-F nms) bbnds)))]
                    (dorun (map (fn [nm type bnd]
                                  (when-not (ind/has-kind? type bnd opts)
                                    (err/tc-error (str "Polymorphic type variable " (r/F-original-name (r/make-F nm))
                                                       " has kind " (pr-str bnd)
                                                       " but given " (pr-str type))
                                                  opts)))
                                nms types bbnds)))
      ;PolyDots NYI
      :else (err/nyi-error (str "instantiate-poly: requires Poly, and PolyDots NYI") opts))))

;; Resolve

(declare resolve-tapp* resolve-app*)

(t/ann ^:no-check resolve-TApp [TApp t/Any -> r/Type])
(defn resolve-TApp [app opts]
  {:pre [(r/TApp? app)]
   :post [(r/Type? %)]}
  (resolve-tapp* (:rator app) (:rands app) {:tapp app} opts))

(t/ann ^:no-check resolve-tapp* [r/Type (t/Seqable r/Type) t/Any t/Any -> r/Type])
(defn resolve-tapp* [rator rands {:keys [tapp]} opts]
  {:pre [(r/TApp? tapp)]}
  (let [rator (fully-resolve-type rator opts)
        _ (when-not (r/TypeFn? rator) 
            (err/int-error (str "First argument to TApp must be TFn, actual: " (ind/unparse-type rator opts)) opts))]
    (when-not (= (count rands) (:nbound rator))
      (let [opts (assoc opts ::vs/current-env (-> tapp meta :env))] ;must override env, or clear it
        (err/int-error (str "Wrong number of arguments (" (count rands) ") passed to type function: "
                            (ind/unparse-type tapp opts) 
                            (when-some [syn (-> tapp meta :syn)]
                              (str "\n\nin: " (pr-str syn))))
                       opts)))
    (instantiate-typefn rator rands {:tapp tapp} opts)))

(t/ann ^:no-check resolve-App [App t/Any -> r/Type])
(defn resolve-App [app opts]
  {:pre [(r/App? app)]}
  (resolve-app* (:rator app) (:rands app) opts))

(t/ann ^:no-check resolve-app* [r/Type (t/Seqable r/Type) t/Any -> r/Type])
(defn resolve-app* [rator rands opts]
  (let [rator (fully-resolve-type rator opts)]
    (cond
      (r/Poly? rator) (do (when-not (= (count rands) (:nbound rator))
                            (err/int-error (str "Wrong number of arguments provided to polymorphic type"
                                              (ind/unparse-type rator opts))
                                           opts))
                          (instantiate-poly rator rands opts))
      ;PolyDots NYI
      :else (throw (Exception. (str (when-some [env (::vs/current-env opts)]
                                      (str (:line env) ": "))
                                    "Cannot apply non-polymorphic type " (ind/unparse-type rator opts)))))))

(declare resolve-Name unfold fully-resolve-type find-val-type)

(t/ann ^:no-check resolve-Get [GetType t/Any -> r/Type])
(defn resolve-Get [{:keys [target key not-found] :as t} opts]
  {:pre [(r/GetType? t)]
   :post [(r/Type? %)]}
  (find-val-type target key not-found #{} opts))

(t/ann ^:no-check resolve-Merge [MergeType t/Any -> r/Type])
(defn resolve-Merge [{:keys [types] :as t} opts]
  {:pre [(r/MergeType? t)]
   :post [(r/Type? %)]}
  (or (apply (requiring-resolve 'typed.clj.checker.assoc-utils/merge-types) opts (map r/ret types))
      (println (str "WARNING: t/Merge resolved to t/Any"))
      r/-any))

(t/ann ^:no-check resolve-TypeOf [TypeOf -> r/Type])
(defn resolve-TypeOf [{:keys [vsym] :as t} opts]
  {:pre [(r/TypeOf? t)]
   :post [(r/Type? %)]}
  (or (ind/type-of-nofail vsym opts)
      (err/int-error (str "Could not resolve TypeOf " vsym) opts)))

(defn resolve-Match [{:keys [target clauses] :as t} opts]
  {:pre [(r/MatchType? t)]
   :post [(r/AnyType? %)]}
  (or (some-> (some #(ind/solve (r/ret target) % opts) clauses)
              :t)
      (err/tc-error (str "No matching clause: " (pr-str t)) opts)))

(t/ann -resolve [r/Type t/Any -> r/Type])
(defn -resolve [ty opts]
  {:pre [(r/AnyType? ty)]
   :post [(r/AnyType? %)]}
  (p/-resolve ty opts))

(defn Get-requires-resolving? [ty opts]
  {:pre [(r/GetType? ty)]}
  (not-any? (comp r/F? #(fully-resolve-type % opts)) ((juxt :target :key) ty)))

(defn Merge-requires-resolving? [ty opts]
  {:pre [(r/MergeType? ty)]}
  (not-any? (comp r/F? #(fully-resolve-type % opts)) (:types ty)))

(defn Match-can-resolve? [ty opts]
  {:pre [(r/MatchType? ty)]}
  (not (r/F? (:target ty))))

(t/ann requires-resolving? [r/Type -> t/Any])
(defn requires-resolving? [ty opts]
  {:pre [(r/AnyType? ty)]}
  (p/-requires-resolving? ty opts))

(t/ann ^:no-check resolve-Name [Name t/Any -> r/Type])
(defn resolve-Name [nme opts]
  {:pre [(r/Name? nme)]
   :post [(r/Type? %)]}
  (let [resolve-name* (requiring-resolve 'typed.cljc.checker.name-env/resolve-name*)]
    (resolve-name* (:id nme) opts)))

(t/ann fully-resolve-type [r/Type t/Any -> r/Type])
(defn fully-resolve-type
  ([t opts] (fully-resolve-type t #{} opts))
  ([t seen opts]
   {:pre [(r/AnyType? t)
          (set? seen)]}
   (let [_ (assert (not (seen t)) "Infinite non-Rec type detected")]
     (if (requires-resolving? t opts)
       (recur (-resolve t opts) (conj seen t) opts)
       t))))

(t/ann fully-resolve-non-rec-type
       [r/Type (t/Set r/Type) :? t/Any -> r/Type])
(defn fully-resolve-non-rec-type
  ([t opts] (fully-resolve-non-rec-type t #{} opts))
  ([t seen opts]
   (let [_ (assert (not (seen t)) "Infinite non-Rec type detected")]
     (if (and (not (r/Mu? t))
              (requires-resolving? t opts))
       (recur (-resolve t opts) (conj seen t) opts)
       t))))

;; Mu

(declare abstract instantiate)

;smart constructor
(t/ann Mu* [t/Sym r/Type t/Any -> r/Type])
(defn Mu* [name body opts]
  (let [original-name (-> name r/make-F r/F-original-name)
        v (r/Mu-maker (abstract name body opts))]
    (with-original-names v original-name)))

;smart destructor
(t/ann Mu-body* [t/Sym Mu t/Any -> r/Type])
(defn Mu-body* [name t opts]
  {:pre [(r/Mu? t)
         (symbol? name)]}
  (instantiate name (p/mu-scope t) opts))

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
              (gensym "Mu-fresh-symbol*"))]
    (fresh-symbol s "_Mu-fresh-symbol*")))

(t/tc-ignore
(defn- substitute-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.subst) 'substitute)]
    (assert (var? v) "substitute unbound")
    v))
  )

(t/ann ^:no-check unfold-Mu-with [Mu r/Type t/Any -> r/Type])
(defn unfold-Mu-with [t tsubst opts]
  {:pre [(r/Mu? t)]
   :post [(r/Type? %)]}
  (let [substitute @(substitute-var)
        sym (Mu-fresh-symbol* t)
        opts (free-ops/with-bounded-frees opts {(r/make-F sym) r/no-bounds})
        body (Mu-body* sym t opts)]
    (substitute tsubst sym body opts)))

(t/ann ^:no-check unfold [Mu t/Any -> r/Type])
(defn unfold [t opts]
  {:pre [(r/Mu? t)]
   :post [(r/Type? %)]}
  (unfold-Mu-with t t opts))

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

(defn overlap-CountRange-KwArgsSeq?
  [A {:keys [lower upper] :as cr} kws opts]
  {:pre [(r/CountRange? cr)
         (r/KwArgsSeq? kws)]
   :post [(boolean? %)]}
  ;(prn "overlap-CountRange-KwArgsSeq?" kws cr)
  (if (not (overlap (upcast-kw-args-seq kws opts) cr opts))
    false
    ;; seq has even numbered count without trailing map
    (or (-> kws :kw-args-regex :maybe-trailing-nilable-non-empty-map? boolean)
        (not= lower upper)
        (let [exact-count lower]
          ;; an odd exact count without a trailing map derives a contradiction
          (even? exact-count)))))

;true if types t1 and t2 overlap (NYI)
(t/ann ^:no-check overlap (t/IFn [r/Type r/Type t/Any -> t/Any]
                                 [t/Any r/Type r/Type t/Any -> t/Any]))
(defn overlap
  ([t1 t2 opts] (overlap (::overlap-seen opts #{}) t1 t2 opts))
  ([A t1 t2 opts]
   (if (contains? A [t1 t2])
     true
     (let [A* (conj A [t1 t2])
           overlap #(overlap A* %1 %2 opts)
           ;; handle mutual recursion between subtyping and overlap
           subtype? #(ind/subtype? %1 %2 (assoc opts ::overlap-seen A*))

           t1 (fully-resolve-type t1 opts)
           t2 (fully-resolve-type t2 opts)
           eq (= t1 t2)
           hmap-and-seq? (fn [h s] (and (r/HeterogeneousMap? h)
                                        (impl/impl-case opts
                                          :clojure (and (r/RClass? s)
                                                        ('#{clojure.lang.ISeq} (:the-class s)))
                                          :cljs (and (r/Protocol? s)
                                                     ('#{cljs.core/ISeq} (:the-var s))))))
           hvec-and-seq? (fn [h s] (and (r/HeterogeneousVector? h)
                                        (impl/impl-case opts
                                          :clojure (and (r/RClass? s)
                                                        ('#{clojure.lang.ISeq} (:the-class s)))
                                          :cljs (and (r/Protocol? s)
                                                     ('#{cljs.core/ISeq} (:the-var s))))))
           record-and-iseq? (fn [r s]
                              (and (r/Record? r)
                                   (ind/subtype? s (impl/impl-case opts
                                                     :clojure (RClass-of clojure.lang.ISeq [r/-any] opts)
                                                     :cljs (Protocol-of 'cljs.core/ISeq [r/-any] opts))
                                                 opts)))]
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
         (let [neg-type (fully-resolve-type (:type t1) opts)]
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
         (if (= (:the-class t1)
                (:the-class t2))
           ;; TODO can we say there's no overlap if there are different invariant parameters?
           true
           (let [_ (impl/assert-clojure opts)
                 c1 (r/RClass->Class t1)
                 c2 (r/RClass->Class t2)
                 c1-mods (.getModifiers c1)
                 c2-mods (.getModifiers c2)
                 c1-final? (Modifier/isFinal c1-mods)
                 c2-final? (Modifier/isFinal c2-mods)]
             ; there is only an overlap if a class could have both classes as parents
             ;(prn t1-flags t2-flags)
             (cond
               (or (.isAssignableFrom c1 c2)
                   (.isAssignableFrom c2 c1)) true
               ; no potential ancestors
               (or (Modifier/isFinal c1-mods) (Modifier/isFinal c2-mods)) false
               ; if we have two things that are not interfaces, ie. abstract, normal
               ; classes, there is no possibility of overlap
               (not (or (Modifier/isInterface c1-mods) (Modifier/isInterface c2-mods))) false
               :else true)))

         (and (impl/checking-clojurescript? opts)
              (or (and (r/JSNull? t1)
                       (r/JSUndefined? t2))
                  (and (r/JSNull? t2)
                       (r/JSUndefined? t1))))
         false

        ;; already rules out free variables, so this is safe.
        ;; FIXME review this, I don't understand why it's a good idea, but
        ;; it fails tests if we remove it try something weaker like only put the
        ;; Value on the lhs of subtype.
        (or (r/Value? t1)
            (r/Value? t2))
        (or (ind/subtype? t1 t2 opts)
            (ind/subtype? t2 t1 opts))

         (and (r/CountRange? t1)
              (r/CountRange? t2)) 
         (countrange-overlap? t1 t2)

         (and (r/CountRange? t1)
              (r/KwArgsSeq? t2))
         (overlap-CountRange-KwArgsSeq? A t1 t2 opts)
         (and (r/CountRange? t2)
              (r/KwArgsSeq? t1))
         (overlap-CountRange-KwArgsSeq? A t2 t1 opts)

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

;; FIXME check compatibility between HSequentials
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
(t/ann ^:no-check restrict [r/Type r/Type t/Any -> r/Type])
(defn restrict [t1 t2 opts]
  (let [t1 (fully-resolve-type t1 opts)
        t2 (fully-resolve-type t2 opts)
        subst-all @(subst-all-var)
        infer @(infer-var)]
    (cond
      (ind/subtype? t1 t2 opts) t1 ;; already a subtype

      (not (overlap t1 t2 opts)) (r/Bottom) ;there's no overlap, so the restriction is empty

      (r/Union? t1) (Un (map (fn [e] (restrict e t2 opts)) (:types t1)) opts)
      (r/Union? t2) (Un (map (fn [e] (restrict t1 e opts)) (:types t2)) opts)

      (r/Poly? t2)
      (let [names (Poly-fresh-symbols* t2)
            t (Poly-body* names t2 opts)
            bbnds (Poly-bbnds* names t2 opts)
            subst (u/handle-cs-gen-failure
                    (infer (zipmap names bbnds) {} (list t1) (list t) t1))]
        (and subst (restrict t1 (subst-all subst t1 opts) opts)))

      ;TODO other cases
      :else (In [t2 t1] opts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable rep

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
  (fn [{:keys [dom rng rest drest kws prest pdot regex] :as ty} name count outer sb name-to]
    {:pre [(case (:kind ty)
             (:fixed :rest :drest :kws :prest :pdot :regex) true
             false)]}
    (r/make-Function (mapv sb dom)
                     (sb rng)
                     :rest (some-> rest sb)
                     :drest (some-> drest
                                    (r/update-DottedPretype
                                      [:pre-type sb]
                                      [:name #(if (= % name)
                                                (+ count outer)
                                                %)]))
                     :kws (letfn [(abstract-kw-map [m]
                                    {:pre [(map? m)]}
                                    (update-vals m sb))]
                            (some-> kws
                              (r/update-KwArgs
                                [:mandatory abstract-kw-map]
                                [:optional abstract-kw-map])))
                     :prest (some-> prest sb)
                     :pdot (some-> pdot
                                   (r/update-DottedPretype
                                     [:pre-type sb]
                                     [:name #(if (= % name)
                                               (+ count outer)
                                               %)]))
                     :regex (some-> regex sb))))

(f/add-fold-case
  IAbstractMany abstract-many*
  HSequential
  (fn [ty name count outer sb name-to]
    (r/-hsequential 
      (mapv sb (:types ty))
      {:filters (mapv sb (:fs ty))
       :objects (mapv sb (:objects ty))
       :rest (some-> (:rest ty) sb)
       :drest (some-> (:drest ty)
                      (r/update-DottedPretype
                        [:pre-type sb]
                        [:name #(if (= % name)
                                  (+ count outer)
                                  %)]))
       :repeat (:repeat ty)
       :kind (:kind ty)}
      opts)))

(f/add-fold-case
  IAbstractMany abstract-many*
  AssocType
  (fn [{:keys [target entries dentries]} name count outer sb name-to]
   (r/AssocType-maker (sb target)
                      (mapv (fn [[k v]] [(sb k) (sb v)]) entries)
                      (some-> dentries
                              (r/update-DottedPretype
                                [:pre-type sb]
                                [:name #(if (= % name)
                                          (+ count outer)
                                          %)])))))

(f/add-fold-case
  IAbstractMany abstract-many*
  Mu
  (fn [{:keys [scope] :as mu} name count outer sb name-to]
   (let [body (r/remove-scopes 1 scope)
         body' (name-to name count (inc outer) body opts)]
     (if (identical? body body')
       mu
       (r/Mu-maker (r/add-scopes 1 body')
                   (meta mu))))))

(f/add-fold-case
  IAbstractMany abstract-many*
  Poly
  (fn [{:keys [named kind] bbnds* :bbnds n :nbound body* :scope :as poly} name count outer sb name-to]
   (case kind
     :Poly (let [rs #(r/remove-scopes n %)
                 body (rs body*)
                 bbnds (mapv rs bbnds*)
                 as #(r/add-scopes n (name-to name count (+ n outer) % opts))]
             (r/Poly-maker n
                           (mapv as bbnds)
                           (as body)
                           named
                           (meta poly)))
     :PolyDots (let [rs #(r/remove-scopes n %)
                     body (rs body*)
                     bbnds (mapv rs bbnds*)
                     as #(r/add-scopes n (name-to name count (+ n outer) % opts))]
                 (r/PolyDots-maker n
                                   (mapv as bbnds)
                                   (as body)
                                   named
                                   (meta poly))))))

(f/add-fold-case
  IAbstractMany abstract-many*
  TypeFn
  (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances] :as t}
       name count outer sb name-to]
  (let [rs #(r/remove-scopes n %)
        body (rs body*)
        bbnds (mapv rs bbnds*)
        as #(r/add-scopes n (name-to name count (+ n outer) % opts))]
     (r/TypeFn-maker n 
                     variances
                     (mapv as bbnds)
                     (as body)
                     (meta t)))))

(t/ann ^:no-check abstract-many [(t/Seqable t/Sym) r/Type t/Any -> (t/U r/Type Scope)])
(defn abstract-many
  "Names Type -> Scope^n  where n is (count names)"
  [names ty opts]
  {:pre [((some-fn r/Type? r/TypeFn? r/Kind?) ty)]}
  (letfn [(name-to
            ([name count type opts] (name-to name count 0 type opts))
            ([name count outer ty opts]
             (assert (symbol? name))
             (letfn [(sb
                       ([t] (sb t opts))
                       ([t opts] (name-to name count outer t opts)))]
               (call-abstract-many*
                 ty opts
                 {:type-rec sb
                  :filter-rec (f/sub-f sb `call-abstract-many* opts)
                  :object-rec (f/sub-o sb `call-abstract-many* opts)
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
            (r/add-scopes n (name-to (first names) 0 ty opts))
            (recur (name-to (first names) count ty opts)
                   (next names)
                   (dec count))))))))

(f/def-derived-fold IInstantiateMany instantiate-many* [b->f outer sb replace])

(defn lookup-b->f' [idx ^objects b->f outer]
  (let [i (- idx outer)]
    (when (AND (<= 0 i)
               (< i (alength b->f)))
      (aget b->f i))))

(defn lookup-b->f [idx b->f outer]
  (or (lookup-b->f' idx b->f outer)
      idx))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  B
  (fn [{:keys [idx] :as t} b->f outer sb replace]
    (if-some [image (lookup-b->f' idx b->f outer)]
      (r/F-maker image)
      t)))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot kind regex]} b->f outer sb replace]
    {:pre [(case kind
             (:fixed :rest :drest :kws :prest :pdot :regex) true
             false)]}
    (r/make-Function
      (mapv sb dom)
      (sb rng)
      :rest (some-> rest sb)
      :drest (some-> drest
                     (r/update-DottedPretype
                       [:pre-type sb]
                       [:name lookup-b->f b->f outer]))
      :kws (letfn [(instantiate-kw-map [m]
                     {:pre [(map? m)]}
                     (reduce-kv (fn [m k v]
                                  (assoc m k (sb v)))
                                {} m))]
             (some-> kws
               (r/update-KwArgs
                 [:mandatory instantiate-kw-map]
                 [:optional instantiate-kw-map])))
      :prest (some-> prest sb)
      :pdot (some-> pdot
                    (r/update-DottedPretype
                      [:pre-type sb]
                      [:name lookup-b->f b->f outer]))
      :regex (some-> regex sb))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  HSequential
  (fn [ty b->f outer sb replace]
    (r/-hsequential 
      (mapv sb (:types ty))
      {:filters (mapv sb (:fs ty))
       :objects (mapv sb (:objects ty))
       :rest (some-> (:rest ty) sb)
       :drest (some-> (:drest ty)
                      (r/update-DottedPretype
                        [:pre-type sb]
                        [:name lookup-b->f b->f outer]))
       :repeat (:repeat ty)
       :kind (:kind ty)}
      opts)))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  AssocType
  (fn [{:keys [target entries dentries]} b->f outer sb replace]
    (r/AssocType-maker (sb target)
                       (mapv (fn [[k v]] [(sb k) (sb v)]) entries)
                       (some-> dentries
                               (r/update-DottedPretype
                                 [:pre-type sb]
                                 [:name lookup-b->f b->f outer])))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  Mu
  (fn [{:keys [scope] :as mu} b->f outer sb replace]
    (let [body (r/remove-scopes 1 scope)
          body' (replace body b->f (inc outer) opts)]
      (if (identical? body body')
        mu
        (r/Mu-maker (r/add-scopes 1 body')
                    (meta mu))))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  Poly
  (fn [{:keys [named kind] bbnds* :bbnds n :nbound body* :scope :as poly} 
       b->f outer sb replace]
    (case kind
      :Poly (let [rs #(r/remove-scopes n %)
                  body (rs body*)
                  bbnds (mapv rs bbnds*)
                  as #(r/add-scopes n (replace % b->f (+ n outer) opts))]
              (r/Poly-maker n 
                            (mapv as bbnds)
                            (as body)
                            named
                            (meta poly)))
      :PolyDots (let [rs #(r/remove-scopes n %)
                      body (rs body*)
                      bbnds (mapv rs bbnds*)
                      as #(r/add-scopes n (replace % b->f (+ n outer) opts))]
                  (r/PolyDots-maker n 
                                    (mapv as bbnds)
                                    (as body)
                                    named
                                    (meta poly))))))

(f/add-fold-case
  IInstantiateMany instantiate-many*
  TypeFn
  (fn [{bbnds* :bbnds n :nbound body* :scope :keys [variances] :as t}
       b->f outer sb replace]
    (let [rs #(r/remove-scopes n %)
          body (rs body*)
          bbnds (mapv rs bbnds*)
          as #(r/add-scopes n (replace % b->f (+ n outer) opts))]
      (r/TypeFn-maker n
                      variances
                      (mapv as bbnds)
                      (as body)
                      (meta t)))))
)

(t/ann ^:no-check instantiate-many [(t/Seqable t/Sym) p/IScope t/Any -> r/Type])
(defn instantiate-many
  "instantiate-many : List[Symbols] Scope^n -> Type
  Instantiate de Bruijn indices in sc to frees named by
  images, preserving upper/lower bounds"
  [images sc opts]
  {:pre [(or (r/Scope? sc)
             (empty? images))]
   :post [((some-fn r/Type? r/TypeFn? r/Kind?) %)]}
  (letfn [(replace [t b->f outer opts]
            (letfn [(sb
                      ([t] (sb t opts))
                      ([t opts] (replace t b->f outer opts)))]
              (let [sf (f/sub-f sb `call-instantiate-many* opts)]
                (call-instantiate-many*
                  t opts
                  {:type-rec sb
                   :filter-rec sf
                   :object-rec (f/sub-o sb `call-instantiate-many* opts)
                   :outer outer
                   :b->f b->f
                   :sb sb
                   :replace replace}))))]
    (let [images (vec images)
          n (count images)]
      (if (zero? n)
        sc
        (replace (r/remove-scopes n sc) (object-array (rseq images)) 0 opts)))))

(t/ann abstract [t/Sym r/Type t/Any -> Scope])
(defn abstract
  "Make free name bound"
  [name ty opts]
  {:pre [(symbol? name)
         (r/Type? ty)]
   :post [(r/Scope? %)]}
  (abstract-many [name] ty opts))

(t/ann instantiate [t/Sym p/IScope -> r/Type])
(defn instantiate 
  "Instantiate bound name to free"
  [f sc opts]
  {:pre [(symbol? f)
         (p/IScope? sc)]}
  (instantiate-many [f] sc opts))

;TODO not sure why this fails to type check
;(t/All [x]
;  (t/IFn ['{kw x} -> x :object {:id 0, :path [Key]}]
;      [(t/U '{kw x} (HMap :without [(Value kw)]) nil) -> (t/U x nil) :object {:id 0, :path [Key]}]
;      [t/Any -> t/Any :object {:id 0, :path [Key]}]))
(t/ann ^:no-check keyword->Fn [t/Kw t/Any -> r/Type])
(defn keyword->Fn [kw opts]
  {:pre [(keyword? kw)]
   :post [(r/Type? %)]}
  (let [opts (free-ops/with-bounded-frees opts {(r/make-F 'x) r/no-bounds})]
    (Poly* ['x]
           [r/no-bounds]
           (r/make-FnIntersection
             (r/make-Function
               [(-partial-hmap opts {(r/-val kw) (r/make-F 'x)})]
               (r/make-F 'x)
               :object (or/-path [(path/-kpe kw)] 0))
             (r/make-Function
               [(Un [(make-HMap opts
                                {:optional {(r/-val kw) (r/make-F 'x)}})
                     r/-nil]
                    opts)]
               (Un [r/-nil (r/make-F 'x)] opts)
               :object (or/-path [(path/-kpe kw)] 0))
             (r/make-Function
               [r/-any]
               r/-any
               :object (or/-path [(path/-kpe kw)] 0)))
           opts)))

(t/ann KeywordValue->Fn [Value t/Any -> r/Type])
(defn KeywordValue->Fn [{:keys [val] :as t} opts]
  {:pre [(keyword-value? t)]}
  (impl/assert-clojure opts)
  (keyword->Fn
    ;; workaround occurrence typing failing to propagate types
    (->> val
         #_(t/cast t/Keyword))
    opts))

;;; KwArgs

(t/ann KwArgs->Type [KwArgs t/Any -> r/Type])
(defn KwArgs->Type [kws opts]
  {:pre [(r/KwArgs? kws)]
   :post [(r/Type? %)]}
  (impl/assert-clojure opts)
  (let [nilable-non-empty? (empty? (:mandatory kws))]
    (cond-> (r/-kw-args-seq :mandatory (:mandatory kws)
                            :optional (:optional kws)
                            :complete? false)
      nilable-non-empty? (as-> t
                           (Un [(In [t (r/make-CountRange 1)] opts)
                                r/-nil]
                               opts)))))

(t/ann KwArgs->HMap [KwArgs t/Any -> r/Type])
(defn KwArgs->HMap [kws opts]
  {:pre [(r/KwArgs? kws)]
   :post [(r/Type? %)]}
  (make-HMap opts
             {:mandatory (:mandatory kws) 
              :optional (:optional kws)
              :complete? (:complete? kws)}))

(t/ann KwArgsSeq->HMap [KwArgsSeq t/Any -> r/Type])
(defn KwArgsSeq->HMap [kws opts]
  {:pre [(r/KwArgsSeq? kws)]
   :post [(r/Type? %)]}
  (KwArgs->HMap (:kw-args-regex kws) opts))

(t/ann KwArgsArray->HMap [KwArgsArray t/Any -> r/Type])
(defn KwArgsArray->HMap [kws opts]
  {:pre [(r/KwArgsArray? kws)]
   :post [(r/Type? %)]}
  (KwArgs->HMap (:kw-args-regex kws) opts))

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
  [t opts]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? r/Type?) %)]}
  (let [t (fully-resolve-type t opts)]
    (cond
      (r/RClass? t)
      (first (filter (some-fn r/Poly? r/FnIntersection?) (RClass-supers* t opts)))
      ;handle other types here
      )))

(t/tc-ignore
(defn type-into-vector [x] (if (r/Union? x) (:types x) [x]))

(defn resolved-type-vector [t opts]
  {:post [(every? r/Type? %)]}
  (cond
   (r/TCResult? t)
   (mapv #(fully-resolve-type % opts)
         (type-into-vector (-> t :t (fully-resolve-type opts))))
   
   (r/Type? t)
   (mapv #(fully-resolve-type % opts) (type-into-vector (fully-resolve-type t opts)))
   
   :else
   [t]))

(defn union-or-nil [ts opts]
  (if (some nil? ts) nil (Un ts opts)))

(defn reduce-type-transform
  "Given a function f, left hand type t, and arguments, reduce the function
  over the left hand types with each argument in turn.
  
  Arguments will not be touched, it is up to f to resolve TCResults as needed.
  However, unions returned by f will be expanded, so the left hand type argument
  will not be a (raw) Union.
  
  Reduction is skipped once nil is returned, or optional predicate :when
  returns false."
  [func t args {pred :when} opts]
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
      (resolved-type-vector t opts)
      args)
     opts)))

;; Inferring bounds

(defn find-bound*
  "Find upper bound if polarity is true, otherwise lower bound"
  [t* polarity opts]
  {:pre [(r/Type? t*)]}
  (let [fnd-bnd #(find-bound* % polarity opts)
        t t*]
    (cond
      (r/Name? t) (fnd-bnd (resolve-Name t opts))
      (r/App? t) (fnd-bnd (resolve-App t opts))
      (r/TApp? t) (fnd-bnd (resolve-TApp t opts))
      (r/Mu? t) (let [name (Mu-fresh-symbol* t)
                      opts (free-ops/with-bounded-frees opts {(r/make-F name) r/no-bounds})
                      body (Mu-body* name t opts)
                      new-body (fnd-bnd body)]
                  (Mu* name new-body opts))
      (r/Poly? t) (fnd-bnd (Poly-body* (Poly-fresh-symbols* t) t opts))
      (r/TypeFn? t) (let [names (TypeFn-fresh-symbols* t)
                          bbnds (TypeFn-bbnds* names t opts)
                          body (TypeFn-body* names bbnds t opts)
                          new-body (fnd-bnd body)]
                      (TypeFn* names
                               (:variances t)
                               bbnds
                               new-body
                               opts))
      :else (if polarity
              r/-any
              r/-nothing))))

(defn find-upper-bound [t opts]
  {:pre [(r/Type? t)]}
  (find-bound* t true opts))

(defn find-lower-bound [t opts]
  {:pre [(r/Type? t)]}
  (find-bound* t false opts))

(defn infer-bounds
  "Returns a Bounds that attempts to fill in meaningful
  upper/lower bounds of the same rank"
  [upper-or-nil lower-or-nil opts]
  {:pre [(every? (some-fn nil? r/AnyType?) [upper-or-nil lower-or-nil])]
   :post [(r/Bounds? %)]}
  (let [{:keys [upper lower]} (cond 
                                ;both bounds provided
                                (and upper-or-nil lower-or-nil) {:upper upper-or-nil :lower lower-or-nil}
                                ;only upper
                                upper-or-nil {:upper upper-or-nil :lower (find-lower-bound upper-or-nil opts)}
                                ;only lower
                                lower-or-nil {:upper (find-upper-bound lower-or-nil opts) :lower lower-or-nil}
                                ;no bounds provided, default to Nothing <: t/Any
                                :else {:upper r/-any :lower r/-nothing})]
    (r/-bounds upper lower)))

(defn find-val-type 
  ([t k default seen opts]
  {:pre [(r/Type? t)
         (r/Type? k)
         (r/Type? default)]
   :post [(r/Type? %)]}
  (let [_ (when (seen t)
            (err/int-error
              (str "Infinitely expanding type:" (ind/unparse-type t opts)) opts))
        t (fully-resolve-type t opts)
        find-val-type (fn 
                        ([t k default]
                         (find-val-type t k default seen opts))
                        ([t k default seen]
                         (find-val-type t k default seen opts)))]
    (cond
      ((some-fn r/TCError? r/Bottom?) t) t
      ((some-fn r/TCError? r/Bottom?) k) k
      ((every-pred r/wild?) t k) r/-wild
      (r/F? t) (let [bnd (free-ops/free-with-name-bnds (:name t) opts)
                     _ (when-not bnd
                         (err/int-error (str "No bounds for type variable: " name " " (::bnds/current-tvar-bnds opts)) opts))]
                 (find-val-type (:upper-bound bnd) k default
                                #{}))
      (ind/subtype? t r/-nil opts) default
      (r/AssocType? t) (let [t* (ind/assoc-pairs-noret (:target t) (:entries t) opts)]
                         (cond
                           (:dentries t) (do
                                           ;(prn "dentries NYI")
                                           r/-any)
                           (r/HeterogeneousMap? t*) (find-val-type t* k default)

                           (and (not t*)
                                (r/F? (:target t))
                                (every? keyword-value? (map first (:entries t))))
                           (let [hmap (ind/assoc-pairs-noret (-partial-hmap opts {}) (:entries t) opts)]
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
                                        (str "Looking up key " (ind/unparse-type k opts) 
                                             " in heterogeneous map type " (ind/unparse-type t opts)
                                             " that declares the key always absent.")
                                        opts)
                                    default)

                                  ; if key is optional the result is the val or the default
                                  opt (Un [opt default] opts)

                                  ; if map is complete, entry must be missing
                                  (complete-hmap? t) default

                                  :else
                                  (do #_(tc-warning (str "Looking up key " (ind/unparse-type k opts)
                                                         " in heterogeneous map type " (ind/unparse-type t opts)
                                                         " which does not declare the key absent ")
                                                    opts)
                                      r/-any)))

      (r/Record? t) (find-val-type (Record->HMap t opts) k default)

      (r/Intersection? t) (In (for [t* (:types t)]
                                (find-val-type t* k default))
                              opts)
      (r/Union? t) (Un (for [t* (:types t)]
                         (find-val-type t* k default))
                       opts)
      (r/RClass? t)
      (r/ret-t
        (ind/check-funapp nil
                          nil
                          (r/ret
                            (env-utils/force-type
                              ((requiring-resolve 'typed.clj.checker.base-env/get-type))
                              opts))
                          [(r/ret t) (r/ret k) (r/ret default)]
                          nil
                          {}
                          opts))
      :else r/-any))))

(defn find-hsequential-in-non-union [t opts]
  {:post [((some-fn nil? r/HSequential?) %)]}
  (let [t (fully-resolve-type t opts)]
    (assert (not (r/Union? t)) t)
    (cond
      (r/HSequential? t) t
      ;; TODO we just pick the first HSequential we find, perhaps 
      ;; there's a better strategy (eg., return a list of them)
      (r/RClass? t) (some #(find-hsequential-in-non-union % opts)
                          (disj (RClass-supers* t opts)
                                ;; stop at top of class hierarchy to prevent
                                ;; infinite recursion
                                (RClass-of Object opts))))))

(defn -tapp [op & rands]
  (r/TApp-maker op (seq rands)))

(defn -name [sym & ts]
  (cond-> (r/Name-maker sym)
    ts (r/TApp-maker ts)))

(defn union-Results [r1 r2 opts]
  {:pre [(r/Result? r1)
         (r/Result? r2)]
   :post [(r/Result? %)]}
  (r/make-Result (Un [(:t r1) (:t r2)] opts)
                 (ind/-FS
                   (ind/-or [(-> r1 :fl :then)
                             (-> r1 :fl :then)]
                            opts)
                   (ind/-or [(-> r1 :fl :else)
                             (-> r1 :fl :else)]
                            opts))
                 (if (= (-> r1 :o) (-> r2 :o))
                   (-> r1 :o)
                   or/-empty)))

(defn intersect-Results [r1 r2 opts]
  {:pre [(r/Result? r1)
         (r/Result? r2)]
   :post [(r/Result? %)]}
  (r/make-Result (In [(:t r1) (:t r2)] opts)
                 (ind/-FS
                   (ind/-and (-> r1 :fl :then)
                             (-> r1 :fl :then))
                   (ind/-and (-> r1 :fl :else)
                             (-> r1 :fl :else)))
                 (if (= (-> r1 :o) (-> r2 :o))
                   (-> r1 :o)
                   or/-empty)))
)

(t/tc-ignore
  (extend Name p/IResolve {:-resolve resolve-Name})
  (extend Mu p/IResolve {:-resolve unfold})
  (extend App p/IResolve {:-resolve resolve-App})
  (extend TApp p/IResolve {:-resolve resolve-TApp})
  (extend GetType p/IResolve {:-resolve resolve-Get})
  (extend MergeType p/IResolve {:-resolve resolve-Merge})
  (extend TypeOf p/IResolve {:-resolve resolve-TypeOf})
  (extend MatchType p/IResolve {:-resolve resolve-Match})
  (extend Object p/IResolve {:-resolve (fn [t opts] t)}))

(t/tc-ignore
  (let [T (fn [_ _] true)
        F (fn [_ _] false)]
    (extend Name p/IRequiresResolving {:-requires-resolving? T})
    (extend App p/IRequiresResolving {:-requires-resolving? T})
    (extend TApp p/IRequiresResolving {:-requires-resolving? (fn [ty opts]
                                                               (not (r/F? (fully-resolve-type (:rator ty) opts))))})
    (extend GetType p/IRequiresResolving {:-requires-resolving? Get-requires-resolving?})
    (extend MergeType p/IRequiresResolving {:-requires-resolving? Merge-requires-resolving?})
    (extend Mu p/IRequiresResolving {:-requires-resolving? T})
    (extend TypeOf p/IRequiresResolving {:-requires-resolving? T})
    (extend MatchType p/IRequiresResolving {:-requires-resolving? Match-can-resolve?})
    (extend Object p/IRequiresResolving {:-requires-resolving? F})))

;; =====================================================
;; Fold defaults

(t/ann return-if-changed (t/All [x] [[(t/Volatile t/Bool) :-> x] x :-> x]))
(defn return-if-changed [f default]
  (let [changed? (volatile! false)
        res (f changed?)]
    (if @changed? res default)))

;; if every element of the result is identical to c, then returns c.
;; this works in tandem with the `assoc` implementation of types, which
;; avoids reconstructing the types if the updated value is identical.
(defn into-identical [init f c]
  ;; TODO map support 
  {:pre [((some-fn vector? set?) init)]}
  (return-if-changed
    (fn [changed?]
      (into init (map (fn [e]
                        (let [r (f e)]
                          (when-not (identical? r e)
                            (vreset! changed? true))
                          r)))
            c))
    c))

(defn- mapv!= [coll f]
  (when-not (nil? coll)
    (let [it (.iterator ^Iterable coll)]
      (loop [i 0, acc nil]
        (if (.hasNext it)
          (let [old (.next it)
                new (f old)]
            (recur (inc i)
                   (if (nil? acc)
                     (if (identical? old new)
                       acc
                       (conj (if (vector? coll)
                               (subvec coll 0 i)
                               (into [] (take i) coll))
                             new))
                     (conj acc new))))
          (or acc coll))))))

(add-default-fold-case NotType
                       (fn [ty]
                         (r/update-NotType ty
                           ; are negative types covariant?
                           [:type type-rec #_{:variance :contravariant}])))

(add-default-fold-case Intersection
                       (fn [ty]
                         ;(prn "fold-default Intersection" ty)
                         (let [ts (:types ty)
                               ts' (mapv!= ts type-rec)]
                           (if (identical? ts ts')
                             ty
                             (In ts' opts)))))

(add-default-fold-case Union
                       (fn [ty]
                         ;(prn "union default" ty)
                         (let [ts (:types ty)
                               ts' (mapv!= ts type-rec)]
                           (if (identical? ts ts')
                             ty
                             (Un ts' opts)))))

(add-default-fold-case FnIntersection
                       (fn [ty]
                         (r/update-FnIntersection ty [:types mapv!= type-rec])))

(add-default-fold-case DottedPretype
                       (fn [ty]
                         (r/update-DottedPretype ty [:pre-type type-rec])))

(add-default-fold-case Function
                       (fn [ty]
                         {:pre [(case (:kind ty)
                                  (:fixed :rest :drest :prest :pdot :kws :regex) true
                                  false)]}
                         ;(prn "fold Function" ty)
                         (r/update-Function
                           ty
                           [:dom mapv!= type-rec]
                           [:rng type-rec]
                           [:rest #(some-> % type-rec)]
                           [:drest #(some-> % type-rec)]
                           [:pdot #(some-> % type-rec)]
                           [:kws #(some-> % type-rec)]
                           [:prest #(when %
                                      (let [t (type-rec %)]
                                        ;; if we fully flatten out the prest, we're left
                                        ;; with no prest
                                        (when-not (r/Bottom? t)
                                          t)))]
                           [:regex #(some-> % type-rec)])))

(add-default-fold-case JSNominal
                       (fn [ty]
                         (r/update-JSNominal ty
                           [:poly? mapv!= type-rec])))

(add-default-fold-case RClass
                       (fn [ty]
                         (r/update-RClass ty
                           [:poly? mapv!= type-rec])))

(add-default-fold-case App
                       (fn [ty]
                         (r/update-App ty
                           [:rator type-rec]
                           [:rands mapv!= type-rec])))

(add-default-fold-case TApp
                       (fn [ty]
                         (r/update-TApp ty
                           [:rator type-rec]
                           ;;TODO variance
                           [:rands mapv!= type-rec])))

(add-default-fold-case PrimitiveArray
                       (fn [ty]
                         (r/update-PrimitiveArray ty
                           [:input-type type-rec]
                           [:output-type type-rec])))

(defn visit-args+variances [args variances type-rec]
  (return-if-changed
    (fn [changed?]
      (mapv (fn [arg v]
              (let [arg' (type-rec arg #_{:variance v})]
                (when-not (identical? arg arg')
                  (vreset! changed? true))
                arg'))
            args
            variances))
    args))

(defn poly?-visitor-DataType-or-Protocol [variances type-rec]
  #(some-> % (visit-args+variances variances type-rec)))

(add-default-fold-case DataType
                       (fn [{:keys [variances] :as ty}]
                         (r/update-DataType ty
                           [:poly? (poly?-visitor-DataType-or-Protocol variances type-rec)]
                           [:fields (fn [fs]
                                      (let [fs' (mapv!= fs (fn [[k t :as tuple]]
                                                             (let [t' (type-rec t)]
                                                               (if (identical? t t')
                                                                 tuple
                                                                 [k t']))))]
                                        (if (identical? fs fs')
                                          fs
                                          ;;TODO don't require array map
                                          (apply array-map (apply concat fs')))))])))

(add-default-fold-case Protocol
                       (fn [{:keys [variances] :as ty}]
                         (r/update-Protocol ty
                           [:poly? (poly?-visitor-DataType-or-Protocol variances type-rec)]
                           ;FIXME this should probably be left alone in fold
                           ; same in promote/demote
                           ; should use return-if-changed
                           [:methods update-vals type-rec])))

(add-default-fold-case TypeFn
                       (fn [ty]
                         (let [names (TypeFn-fresh-symbols* ty)
                               bbnds (TypeFn-bbnds* names ty opts)
                               body (TypeFn-body* names bbnds ty opts)
                               bmap (zipmap (map r/make-F names) bbnds)
                               ;;FIXME type variables are scoped left-to-right in bounds
                               opts' (free-ops/with-bounded-frees opts bmap)
                               bbnds' (mapv!= bbnds #(type-rec % opts'))
                               body' (type-rec body opts')
                               changed? (or (not (identical? bbnds bbnds'))
                                            (not (identical? body body')))]
                           (if changed?
                             (TypeFn* names (:variances ty) bbnds' body' opts)
                             ty))))

(add-default-fold-case Poly
                       (fn [ty]
                         (case (:kind ty)
                           :Poly (let [names (Poly-fresh-symbols* ty)
                                       body (Poly-body* names ty opts)
                                       bbnds (Poly-bbnds* names ty opts)
                                       bmap (zipmap (map r/make-F names) bbnds)
                                       ;;FIXME type variables are scoped left-to-right in bounds
                                       opts' (free-ops/with-bounded-frees opts bmap)
                                       bbnds' (mapv!= bbnds #(type-rec % opts'))
                                       body' (type-rec body opts')
                                       changed? (or (not (identical? bbnds bbnds'))
                                                    (not (identical? body body')))]
                                   (if changed?
                                     (Poly* names bbnds' body' {:named (:named ty)} opts)
                                     ty))
                           :PolyDots (let [names (PolyDots-fresh-symbols* ty)
                                           body (PolyDots-body* names ty opts)
                                           bbnds (PolyDots-bbnds* names ty opts)
                                           bmap (zipmap (map r/make-F names) bbnds)
                                           ;;FIXME type variables are scoped left-to-right in bounds
                                           opts' (free-ops/with-bounded-frees opts bmap)
                                           bbnds' (mapv!= bbnds #(type-rec % opts'))
                                           body' (type-rec body opts')
                                           changed? (or (not (identical? bbnds bbnds'))
                                                        (not (identical? body body')))]
                                       (if changed?
                                         (PolyDots* names bbnds' body' {:named (:named ty)} opts)
                                         ty)))))

(add-default-fold-case Mu
                       (fn [ty]
                         (let [name (Mu-fresh-symbol* ty)
                               opts (free-ops/with-bounded-frees opts {(r/make-F name) r/no-bounds})
                               body (Mu-body* name ty opts)
                               body' (type-rec body opts)
                               changed? (not (identical? body body'))]
                           (if changed?
                             (Mu* name body' opts)
                             ty))))

(add-default-fold-case HSequential 
                       (fn [{:keys [types fs objects rest drest repeat kind] :as ty}]
                         (let [types' (mapv!= types type-rec)
                               fs' (mapv!= fs filter-rec)
                               objects' (mapv!= objects object-rec)
                               rest' (some-> rest type-rec)
                               drest' (some-> drest type-rec)
                               changed? (or (not (identical? types types'))
                                            (not (identical? fs fs'))
                                            (not (identical? objects objects'))
                                            (not (identical? rest rest'))
                                            (not (identical? drest drest')))]
                           (if changed?
                             (r/-hsequential types'
                                             {:filters fs'
                                              :objects objects'
                                              :rest rest'
                                              :drest drest'
                                              :repeat repeat
                                              :kind kind}
                                             opts)
                             ty))))

(add-default-fold-case HSet
                       (fn [{:keys [fixed] :as ty}]
                         (let [fixed' (into-identical #{} type-rec fixed)
                               changed? (not (identical? fixed fixed'))]
                           (if changed?
                             (r/-hset fixed')
                             ty))))

(defn visit-type-map
  ([m f] (visit-type-map m f false))
  ([m f bottom-preserving]
   (return-if-changed
     (fn [changed?]
       (reduce-kv (fn [m k v]
                    (let [k' (f k)
                          v' (f v)]
                      (if (AND bottom-preserving
                               (OR (r/Bottom? k')
                                   (r/Bottom? v')))
                        (do (vreset! changed? true)
                            (reduced nil))
                        (do (when-not (and (identical? k k')
                                           (identical? v v'))
                              (vreset! changed? true))
                            (assoc m k' v')))))
                  {} m))
     m)))

(defn- visit-type-paired-vector [v f]
  (mapv!= v #(mapv!= % f)))

(add-default-fold-case HeterogeneousMap
                       (fn [ty]
                         (let [mandatory (visit-type-map (:types ty) type-rec (not (::vs/no-simpl opts)))]
                           (if (nil? mandatory)
                             r/-nothing
                             (r/update-HeterogeneousMap ty 
                               [:types (fn [_] mandatory)]
                               ;;TODO move bottom keys to absent if not :complete?
                               [:optional visit-type-map type-rec])))))

(add-default-fold-case JSObj
                       (fn [ty]
                         (r/update-JSObj ty 
                           [:types #(let [vs (vals %)
                                          vs' (mapv!= vs type-rec)
                                          changed? (not (identical? vs vs'))]
                                      (if changed?
                                        (zipmap (keys %) vs')
                                        %))])))

(add-default-fold-case KwArgs
                       (fn [ty]
                         (r/update-KwArgs ty 
                           [:mandatory visit-type-map type-rec]
                           [:optional visit-type-map type-rec])))

(add-default-fold-case Bounds
                       (fn [ty]
                         (r/update-Bounds ty
                           [:upper-bound type-rec]
                           [:lower-bound type-rec])))

(add-default-fold-case KwArgsSeq
                       (fn [ty]
                         (r/update-KwArgsSeq ty 
                           [:kw-args-regex type-rec])))

(add-default-fold-case GetType
                       (fn [ty]
                         (r/update-GetType ty
                           [:target type-rec]
                           [:key type-rec]
                           [:not-found type-rec]
                           [:target-fs filter-rec]
                           [:target-object object-rec])))

(add-default-fold-case AssocType
                       (fn [{:keys [target entries dentries] :as ty}]
                         (let [target' (type-rec target)
                               entries' (visit-type-paired-vector entries type-rec)
                               changed? (or (not (identical? target target'))
                                            (not (identical? entries entries')))]
                           (or (when-not dentries
                                 (if changed?
                                   ((requiring-resolve 'typed.clj.checker.assoc-utils/assoc-pairs-noret)
                                    target' entries' opts)
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
                         (r/update-Result ty
                           [:t type-rec]
                           [:fl filter-rec]
                           [:o object-rec])))

(add-default-fold-case MatchType
                       (fn [ty]
                         (r/update-MatchType ty
                           [:target type-rec]
                           [:clauses mapv!= type-rec])))

(defmacro ^:private ret-first-many [& cls]
  `(do ~@(map #(list `add-default-fold-case % `ret-first) cls)))

; CLJS types

(ret-first-many JSNumber CLJSInteger JSObject JSString JSBoolean JSUndefined
                JSNull JSSymbol)

(add-default-fold-case ArrayCLJS
                       (fn [ty]
                         (r/update-ArrayCLJS ty
                           [:input-type type-rec]
                           [:output-type type-rec])))

;filters

(add-default-fold-case NoFilter ret-first)
(add-default-fold-case TopFilter ret-first)
(add-default-fold-case BotFilter ret-first)

(add-default-fold-case TypeFilter
                       (fn [ty]
                         (fr/update-TypeFilter ty
                           [:type type-rec]
                           [:path mapv!= pathelem-rec])))

(add-default-fold-case NotTypeFilter
                       (fn [ty]
                         (fr/update-NotTypeFilter ty
                           [:type type-rec #_{:variance :contravariant}]
                           [:path mapv!= pathelem-rec])))

(add-default-fold-case ImpFilter
                       (fn [ty]
                         (fr/update-ImpFilter ty
                           [:a filter-rec]
                           [:c filter-rec])))

(add-default-fold-case AndFilter
                       (fn [{:keys [fs] :as ty}]
                         (let [fs' (mapv!= (:fs ty) filter-rec)
                               changed? (not (identical? fs fs'))]
                           (if changed?
                             (ind/-and fs' opts)
                             ty))))

(add-default-fold-case OrFilter
                       (fn [{:keys [fs] :as ty}]
                         (let [fs' (mapv!= (:fs ty) filter-rec)
                               changed? (not (identical? fs fs'))]
                           (if changed?
                             (ind/-or fs' opts)
                             ty))))

(add-default-fold-case FilterSet
                       (fn [ty]
                         (fr/update-FilterSet ty
                           [:then filter-rec]
                           [:else filter-rec])))


;objects
(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty]
                         (or/update-Path ty
                           [:path mapv!= pathelem-rec])))
(add-default-fold-case NoObject ret-first)

;path-elems

(add-default-fold-case KeyPE ret-first)
(add-default-fold-case KeysPE ret-first)
(add-default-fold-case ValsPE ret-first)
(add-default-fold-case ClassPE ret-first)
(add-default-fold-case NthPE ret-first)
(add-default-fold-case CountPE ret-first)
(add-default-fold-case KeywordPE ret-first)
(add-default-fold-case SeqPE ret-first)

;TCResult

(add-default-fold-case TCResult
                       (fn [ty]
                         (r/update-TCResult ty
                           [:t type-rec]
                           [:fl filter-rec]
                           [:o object-rec])))

(add-default-fold-case MergeType
                       (fn [ty]
                         (r/update-MergeType ty
                           [:types mapv!= type-rec])))

(add-default-fold-case Regex
                       (fn [ty]
                         (r/update-Regex ty
                           [:types mapv!= type-rec])))

(add-default-fold-case Instance ret-first)
(add-default-fold-case Satisfies ret-first)
