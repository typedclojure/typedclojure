;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.type-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.clojure :as t]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.set :as set]
            [typed.cljc.checker.impl-protocols :as p]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            clojure.core.typed.contract-ann))

(t/defalias SeqNumber Long)

;;; Type rep predicates

(t/defalias Type
  "A normal type"
  (t/I p/TCType
       clojure.lang.IObj))

(t/defalias AnyType
  "A normal type or special type like Function."
  (t/U Type p/TCAnyType))

(t/defalias Kind p/TCKind)

(t/defalias MaybeScopedType
  "A type or a scope"
  (t/U Type p/IScope))

; not a real symmetric predicate, but we always extend Type with the
; interface for speed, so it's sufficient.
; Should just make this an interface to start with.
(t/ann ^:no-check Type? (t/Pred Type))
(defn Type? [a]
  (instance? typed.cljc.checker.impl_protocols.TCType a))

; similar for AnyType
(t/ann ^:no-check AnyType? (t/Pred AnyType))
(defn AnyType? [a]
  (or (Type? a)
      (instance? typed.cljc.checker.impl_protocols.TCAnyType a)))

(t/ann ^:no-check Kind? (t/Pred Kind))
(defn Kind? [a]
  (instance? typed.cljc.checker.impl_protocols.TCKind a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Types

(u/ann-record Top [])
(u/def-type Top []
  "The top type"
  []
  :methods
  [p/TCType])

(t/ann -any Type)
(def -any (Top-maker))

(u/ann-record Wildcard [])
(u/def-type Wildcard []
  ""
  []
  :methods
  [p/TCType])

(t/ann -wild Type)
(def -wild (Wildcard-maker))

(t/ann wild? (t/Pred Wildcard))
(defn wild? [t]
  (instance? Wildcard t))

(u/ann-record Unchecked [vsym :- (t/U nil t/Sym)])
(u/def-type Unchecked [vsym]
  "The unchecked type, like bottom and only introduced 
  per-namespace with the :unchecked-imports feature."
  []
  :methods
  [p/TCType])

(t/ann -unchecked [(t/U nil t/Sym) :-> Type])
(defn -unchecked [vsym]
  {:pre [((some-fn nil? symbol?) vsym)]}
  (Unchecked-maker vsym))

(u/ann-record TypeOf [vsym :- t/Sym])
(u/def-type TypeOf [vsym]
  "The type of a local or var."
  []
  :methods
  [p/TCType])

(t/ann -type-of [t/Sym :-> Type])
(defn -type-of [vsym]
  {:pre [(symbol? vsym)]}
  (TypeOf-maker vsym))

(u/ann-record Union [types :- (t/SortedSet Type)])
(u/def-type Union [types]
  "An flattened, sorted union of types"
  [(set? types)
   (sorted? types)
   (every? Type? types)
   (not-any? Union? types)]
  :methods
  [p/TCType])

(t/ann ^:no-check sorted-type-set [(t/Seqable Type) -> (t/SortedSet Type)])
(defn sorted-type-set [ts]
  (into (sorted-set-by u/type-comparator) ts))

;temporary union maker
(t/ann Un [Type :* -> Union])
(defn- Un [& types]
  (Union-maker (sorted-type-set types)))

(t/ann empty-union Type)
(def empty-union (Un))

(t/ann Bottom [:-> Type])
(defn Bottom []
  empty-union)

(t/ann -nothing Type)
(def -nothing (Bottom))

(t/ann Bottom? [t/Any -> t/Bool])
(defn Bottom? [a]
  (= empty-union a))

(u/ann-record TCError [])
(u/def-type TCError []
  "Use *only* when a type error occurs"
  []
  :methods
  [p/TCType])

(t/ann-many Type
            Err -error)
(def Err (TCError-maker))
(def -error Err)

(u/ann-record Intersection [types :- (t/I t/NonEmptyCount 
                                          (t/SortedSet Type))])
(u/def-type Intersection [types]
  "An unordered intersection of types."
  [(sorted? types)
   (set? types)
   (seq types)
   (every? Type? types)]
  :methods 
  [p/TCType])

(t/defalias Variance
  "Keywords that represent a certain variance"
  (t/U ':constant ':covariant ':contravariant ':invariant ':dotted))

(t/ann variances (t/Set Variance))
(def variances #{:constant :covariant :contravariant :invariant :dotted})

(t/ann ^:no-check variance? (t/Pred Variance))
(defn variance? [v]
  (contains? variances v))

(declare Scope? TypeFn?)

;FIXME these are crude kinds just to distinguish between Poly and PolyDots
(def kinds #{:Type :.. :TypeFn})

;; *(lower-bound, upper-bound) in Generics of a Higher Kind, Moors et al https://adriaanm.github.io/files/higher.pdf
(u/ann-record Bounds [upper-bound :- MaybeScopedType
                      lower-bound :- MaybeScopedType])
(u/def-type Bounds [upper-bound lower-bound]
  "The kind of a Type between upper-bound and lower-bound."
  [;; verbose for performance
   (Type? upper-bound)
   (Type? lower-bound)
   ;; TODO a TypeFn var probably should have a "bound" embedded in its kind
   ;; e.g., :kind (TFn [[x :variance :covariant]] (Bounds t/Any t/Nothing))
   (not (TypeFn? upper-bound))
   (not (TypeFn? lower-bound))
   ]
  :methods [p/TCKind
            p/TCAnyType])

(u/ann-record B [idx :- Number])
(u/def-type B [idx]
  "de Bruijn indices - should never appear outside of this file.
  Bound type variables"
  [(nat-int? idx)]
  :methods
  [p/TCType])

; Always naming frees as fresh is crucial in Typed Clojure.
; Typed Clojure has bounded-polymorphism, which means we need to be very careful
; when caching results of subtyping, intersections and similar. 
;
; We use bounds to our advantage to make subtyping between free variables more useful
;
; eg. 
; In 
;   (All [[x :< Long]] [-> x]) <: (All [[y :< Number]] [-> y])
; x <: y
;
; Because of the way we check function return values, we cache this result.

; Same with bounds.
(u/ann-record F [name :- t/Sym])
(u/def-type F [name]
  "A named free variable"
  [(symbol? name)]
  :methods
  [p/TCType])

(t/ann make-F [t/Sym -> F])
(defn make-F
  "Make a free variable "
  [name] (F-maker name))

(t/ann F-original-name [F -> t/Sym])
(defn F-original-name 
  "Get the printable name of a free variable.
  
  Used for pretty-printing errors or similar, only instantiate
  an instance of F with this name for explicit scoping."
  [f]
  {:pre [(F? f)]
   :post [(symbol? %)]}
  (or (-> f :name meta :original-name)
      (:name f)))

(u/ann-record Scope [body :- MaybeScopedType])
(u/def-type Scope [body]
  "A scope that contains one bound variable, can be nested. Not used directly"
  [(or (Scope? body)
       (Type? body)
       (Kind? body))]
  :methods
  [p/IScope])

(t/defalias ScopedType
  (t/U Type Scope))

(t/ann ^:no-check scoped-Type? (t/Pred (t/U Scope Type)))
(def scoped-Type? (some-fn Scope? Type?))

(t/ann scope-depth? [Scope Number [t/Any :-> t/Bool] :-> t/Bool])
(defn scope-depth?
  "True if scope is has depth number of scopes nested"
  [scope depth pred]
  {:pre [(Scope? scope)
         (nat-int? depth)]}
  (pred (last (take (inc depth) (iterate #(and (Scope? %)
                                               (:body %))
                                         scope)))))

(u/ann-record RClass [variances :- (t/U nil (t/NonEmptySeqable Variance))
                      poly? :- (t/U nil (t/NonEmptySeqable Type))
                      the-class :- t/Sym
                      replacements :- (t/Map t/Sym ScopedType)
                      unchecked-ancestors :- (t/SortedSet ScopedType)])
(u/def-type RClass [variances poly? the-class replacements unchecked-ancestors]
  "A restricted class, where ancestors are
  (replace replacements (ancestors the-class))"
  [(or (nil? variances)
       (and (seq variances)
            (sequential? variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (sequential? poly?)
            (every? Type? poly?)))
   (symbol? the-class)
   (map? replacements)
   ((con/hash-c? symbol? scoped-Type?) replacements)
   (sorted? unchecked-ancestors)
   ((con/set-c? scoped-Type?) unchecked-ancestors)]
  :intern
  [variances
   (map hash poly?)
   (keyword the-class)]
  :methods
  [p/TCType])

(t/ann RClass->Class [RClass -> Class])
(defn ^Class RClass->Class [rcls]
  {:pre [(RClass? rcls)]}
  (coerce/symbol->Class (:the-class rcls)))

(u/ann-record JSNominal [variances :- (t/U nil (t/NonEmptySeqable Variance))
                         kind :- (t/U ':interface ':class)
                         poly? :- (t/U nil (t/NonEmptySeqable Type))
                         name :- t/Sym
                         ctor :- (t/U nil MaybeScopedType)
                         instance-properties :- (t/Map t/Sym MaybeScopedType)
                         static-properties :- (t/Map t/Sym Type)])
(u/def-type JSNominal [variances kind poly? name ctor instance-properties static-properties]
  "A Javascript nominal type"
  [(or (nil? variances)
       (and (seq variances)
            (sequential? variances)
            (every? variance? variances)))
   (= (count variances) (count poly?))
   (or (nil? poly?)
       (and (seq poly?)
            (sequential? poly?)
            (every? Type? poly?)))
   ((some-fn nil? Type?) ctor)
   (#{:interface :class} kind)
   ((con/hash-c? symbol? (some-fn Scope? Type?)) instance-properties)
   ((con/hash-c? symbol? Type?) static-properties)
   (symbol? name)]
  :methods
  [p/TCType])

(u/ann-record DataType [the-class :- t/Sym,
                        variances :- (t/U nil (t/NonEmptySeqable Variance)),
                        poly? :- (t/U nil (t/NonEmptySeqable Type)),
                        fields :- (t/Map t/Sym MaybeScopedType)
                        record? :- t/Bool])
(u/def-type DataType [the-class variances poly? fields record?]
  "A Clojure datatype"
  [(or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? Type? poly?)))
   (= (count variances) (count poly?))
   (symbol? the-class)
   ((con/array-map-c? symbol? (some-fn Scope? Type?)) fields)
   (boolean? record?)]
  :methods
  [p/TCType])

(t/ann DataType->Class [DataType -> Class])
(defn ^Class DataType->Class [^DataType dt]
  (coerce/symbol->Class (.the-class dt)))

(t/ann Record? [t/Any -> t/Bool])
(defn Record? [^DataType a]
  (boolean
    (when (DataType? a)
      (.record? a))))

(u/ann-record Protocol [the-var :- t/Sym,
                        variances :- (t/U nil (t/NonEmptySeqable Variance)),
                        poly? :- (t/U nil (t/NonEmptySeqable Type)),
                        on-class :- t/Sym,
                        methods :- (t/Map t/Sym Type)
                        declared? :- t/Bool])
(u/def-type Protocol [the-var variances poly? on-class methods declared?]
  "A Clojure Protocol"
  [(symbol? the-var)
   (or (nil? variances)
       (and (seq variances)
            (every? variance? variances)))
   (or (nil? poly?)
       (and (seq poly?)
            (every? (some-fn Scope? Type?) poly?)))
   (= (count poly?) (count variances))
   (symbol? on-class)
   ((con/hash-c? (every-pred symbol? (complement namespace)) (some-fn Scope? Type?)) methods)
   (boolean? declared?)]
  :methods
  [p/TCType])

(u/ann-record TypeFn [nbound :- Number,
                      variances :- (t/U (t/I t/Fn [:-> (t/Seqable Variance)])
                                        (t/Seqable Variance))
                      bbnds :- (t/Seqable Kind),
                      scope :- p/IScope]
              :maker-name -TypeFn-maker)
(u/def-type TypeFn [nbound variances bbnds scope]
  "A type function containing n bound variables with variances.
  Also represents 'kind' of type functions."
  [(nat-int? nbound)
   (or (fn? variances)
       (every? variance? variances))
   (every? #(scope-depth? % nbound Kind?) bbnds)
   (apply = nbound (map count (cond-> [bbnds]
                                (not (fn? variances)) (conj variances))))
   (scope-depth? scope nbound (some-fn Type? Kind?))]
  :maker-name -TypeFn-maker
  :compute-valAt {:variances (fn [this]
                               `(let [v# (.-variances ~this)]
                                  (if (fn? v#) (v#) v#)))
                  :scope (fn [this]
                           `(do (let [v# (.-variances ~this)]
                                  ;; force checks
                                  (when (fn? v#) (v#)))
                                (.-scope ~this)))}
  :methods
  ;;FIXME not a type, of a different kind
  [p/TCType
   p/TCKind])

(t/defalias TFnVariancesMaybeFn
  #_ ;;TODO sugar
  (t/If t/Fn
        [:-> '{:cache t/Bool :variances (t/Seqable Variance)}]
        (t/Seqable Variance))
  (t/U (t/I t/Fn [:-> '{:cache t/Bool :variances (t/Seqable Variance)}])
       (t/I (t/Not t/Fn) (t/Seqable Variance))))

(t/ann ^:force-check TypeFn-maker
       [Number TFnVariancesMaybeFn (t/Seqable Kind) p/IScope (t/Option (t/Map t/Any t/Any)) :?
        :-> TypeFn])
(defn TypeFn-maker
  ([nbound variances bbnds scope]
   (TypeFn-maker nbound variances bbnds scope nil))
  ([nbound variances bbnds scope mta]
   (-TypeFn-maker nbound
                  (if (fn? variances)
                    (let [vol (volatile! (t/ann-form nil (t/Seqable Variance)))]
                      (fn []
                        (or (do @vol)
                            (let [{vs :variances :keys [cache]} (variances)]
                              (t/ann-form vs (t/Seqable Variance))
                              (assert (every? variance? vs) [nbound variances bbnds scope])
                              (assert (apply = nbound (map count [vs bbnds])))
                              (assert (boolean? cache))
                              (when cache (vreset! vol vs))
                              vs))))
                    ;; FIXME issues with (t/I (t/Not t/Fn) (t/Seqable Variance)) :< (t/Seqable Variance)
                    ^{::t/unsafe-cast (t/Seqable Variance)}
                    variances)
                  bbnds
                  scope
                  mta)))

(declare Regex?)

(u/ann-record Poly [nbound :- Number,
                    bbnds :- (t/Vec Kind),
                    scope :- p/IScope
                    named :- (t/Map t/Sym t/Int)
                    kind :- t/Kw]
              :maker-name -Poly-maker)
(u/def-type Poly [nbound bbnds scope named kind]
  "A polymorphic type containing n bound variables.
  `named` is a map of free variable names to de Bruijn indices (range nbound)"
  [(nat-int? nbound)
   (vector? bbnds)
   (every? #(scope-depth? % nbound Kind?) bbnds)
   (= nbound (count bbnds))
   (scope-depth? scope nbound Type?)
   (map? named)
   (every? symbol? (keys named))
   (every? #(<= 0 % (dec nbound)) (vals named))
   (or (empty? named)
       (apply distinct? (vals named)))
   (case kind
     :Poly (every? #(scope-depth? % nbound Bounds?) bbnds)
     :PolyDots (and (every? #(scope-depth? % nbound Bounds?) (pop bbnds))
                    (scope-depth? (peek bbnds)
                                  nbound
                                  Regex?)))]
  :pred-name -Poly?
  :maker-name -Poly-maker
  :ctor-meta {:private true}
  :methods
  [p/TCType])

(t/ann-many [t/Any :-> t/Bool :filters {:then (is Poly 0)}]
            Poly? PolyDots?)
(defn Poly? [p] (and (-Poly? p) (= :Poly (:kind p))))
(defn PolyDots? [p] (and (-Poly? p) (= :PolyDots (:kind p))))
(t/ann-many [Number (t/Seqable Kind) p/IScope (t/Map t/Sym t/Int) (t/? (t/Option (t/Map t/Any t/Any)))
             :-> Poly]
            ^:force-check Poly-maker
            ;;TODO assoc-in support
            ^:no-check PolyDots-maker)
(defn Poly-maker
  ([nbound bbnds scope named] (Poly-maker nbound bbnds scope named nil))
  ([nbound bbnds scope named meta]
   {:post [(Poly? %)]}
   (-Poly-maker nbound (vec bbnds) scope named :Poly meta)))
(defn PolyDots-maker
  ([nbound bbnds scope named] (PolyDots-maker nbound bbnds scope named nil))
  ([nbound bbnds scope named meta]
   {:pre [(pos? nbound)]
    :post [(PolyDots? %)]}
   (-Poly-maker nbound (vec bbnds) scope named :PolyDots meta)))

(t/ann unsafe-body [[t/Any :-> t/Any] Poly :-> Type])
(defn ^:private unsafe-body [pred p]
  {:pre [(pred p)]
   :post [((every-pred Type? (complement Scope?)) %)]}
  (let [sc (t/atom :- MaybeScopedType, (:scope p))
        _ (t/tc-ignore
            ;;TODO dotimes type rule
            (dotimes [n (:nbound p)]
              (let [s @sc
                    _ (assert (Scope? s))]
                (reset! sc (:body s)))))
        t @sc]
    (assert (not (p/IScope? t)))
    t))

(t/ann Poly-body-unsafe* [Poly :-> Type])
(defn Poly-body-unsafe* [p]
  (unsafe-body Poly? p))

(t/ann PolyDots-body-unsafe* [Poly :-> Type])
(defn PolyDots-body-unsafe* [p]
  {:pre [(PolyDots? p)]}
  (unsafe-body PolyDots? p))

(u/ann-record Name [id :- t/Sym])
(u/def-type Name [id]
  "A late bound name"
  [((every-pred symbol?
                (some-fn namespace #(some #{\.} (str %))))
     id)]
  :methods
  [p/TCType])

(u/ann-record TApp [rator :- Type,
                    rands :- (t/Seqable Type)])
(u/def-type TApp [rator rands]
  "An application of a type function to arguments."
  [(Type? rator)
   (every? Type? rands)]
  :methods
  [p/TCType])

(u/ann-record App [rator :- Type,
                   rands :- (t/Seqable Type)])
(u/def-type App [rator rands]
  "An application of a polymorphic type to type arguments"
  [(Type? rator)
   (every? Type? rands)]
  :methods
  [p/TCType])

(u/ann-record Mu [scope :- p/IScope])
(u/def-type Mu [scope]
  "A recursive type containing one bound variable, itself"
  [(Scope? scope)]
  :methods
  [p/TCType
   p/IMu
   (mu-scope [_] scope)])

(declare FnIntersection?)

(u/ann-record MatchType [target :- Type
                         clauses :- (t/Vec Type #_(U FnIntersection Poly))])
(u/def-type MatchType [target clauses]
  "A recursive type containing one bound variable, itself"
  [(Type? target)
   (every? (some-fn FnIntersection? -Poly?) clauses)]
  :methods
  [p/TCType])

(t/ann Mu-body-unsafe [Mu -> Type])
(defn Mu-body-unsafe [mu]
  {:pre [(Mu? mu)]
   :post [(Type? %)
          (not (p/IScope? %))]}
  (-> mu :scope :body))

(u/ann-record Value [val :- t/Any])
(u/def-type Value [val]
  "A Clojure value"
  []
  :methods
  [p/TCType])

(u/ann-record AnyValue [])
(u/def-type AnyValue []
  "Any Value"
  []
  :methods
  [p/TCType])

(t/ann -val [t/Any -> Type])
(def -val Value-maker)

(t/ann-many Type 
            -false -true -nil -falsy)
(def -false (-val false))
(def -true (-val true))
(def -nil (-val nil))
(def -falsy (Un -nil -false))

(t/ann-many [t/Any -> t/Bool]
            Nil? False? True?)
(defn Nil? [a] (= -nil a))
(defn False? [a] (= -false a))
(defn True? [a] (= -true a))


(declare Result?)

(u/ann-record HeterogeneousMap [types :- (t/Map Type Type),
                                optional :- (t/Map Type Type),
                                absent-keys :- (t/Set Type),
                                other-keys? :- t/Bool])
(u/def-type HeterogeneousMap [types optional absent-keys other-keys?]
  "A constant map, clojure.lang.IPersistentMap"
  [((con/hash-c? Value? (some-fn Type? Result?))
     types)
   ((con/hash-c? Value? (some-fn Type? Result?))
     optional)
   ((con/set-c? Value?) absent-keys)
   (empty? (set/intersection
             (set (keys types))
             (set (keys optional))
             absent-keys))
   (boolean? other-keys?)]
  :methods
  [p/TCType])

(u/ann-record JSObj [types :- (t/Map t/Kw Type)])
(u/def-type JSObj [types]
  "A JavaScript structural object"
  [((con/hash-c? keyword? Type?) types)]
  :methods
  [p/TCType])

(declare Regex?)

(u/ann-record Regex [types :- (t/Vec (t/U Type Kind Regex DottedPretype))
                     kind :- t/Kw])
(u/ann-record DottedPretype [pre-type :- (t/U Type Regex)
                             name :- (t/U t/Sym Number)])
(u/def-type DottedPretype [pre-type name]
  "A dotted pre-type. Not a type."
  [((some-fn Type? Regex?) pre-type)
   ((some-fn symbol? nat-int?) name)]
  :methods
  [p/TCAnyType])

(t/ann-many [Type (t/U t/Sym Number) -> DottedPretype]
            DottedPretype1-maker
            DottedPretype2-maker)

(defn DottedPretype1-maker [pre-type name]
  (DottedPretype-maker pre-type name))

(t/defalias HSequentialKind (t/U ':list ':seq ':vector ':sequential))

(u/ann-record HSequential [types :- (t/Seqable Type)
                           fs :- (t/Vec p/IFilterSet)
                           objects :- (t/Vec p/IRObject)
                           ;variable members to the right of fixed
                           rest :- (t/U nil Type)
                           drest :- (t/U nil DottedPretype)
                           repeat :- t/Bool
                           kind :- HSequentialKind])
(u/def-type HSequential [types fs objects rest drest repeat kind]
  "A constant Sequential, clojure.lang.Sequential"
  [(sequential? types)
   (every? (some-fn Type? Result?) types)
   (vector? fs)
   (every? p/IFilterSet? fs)
   (vector? objects)
   (every? p/IRObject? objects)
   (apply = (map count [types fs objects]))
   (#{0 1} (count (filter identity [rest drest repeat])))
   (or (not repeat) (not-empty types))
   ((some-fn nil? Type?) rest)
   ((some-fn nil? DottedPretype?) drest)
   (boolean? repeat)
   (#{:list :seq :vector :sequential} kind)]
  :methods
  [p/TCType])

(u/ann-record TopHSequential [])
(u/def-type TopHSequential []
  "Supertype of all HSequentials's."
  []
  :methods [p/TCType])

(t/ann -any-hsequential Type)
(def -any-hsequential (TopHSequential-maker))

(t/ann ^:no-check -hsequential
       [(t/Seqable Type) & :optional {:filters (t/Seqable p/IFilterSet) :objects (t/Seqable p/IRObject)
                                      :rest (t/U nil Type) :drest (t/U nil DottedPretype) :repeat t/Bool
                                      :kind HSequentialKind}
        -> Type])
(defn -hsequential
  [types & {:keys [filters objects rest drest kind] repeat? :repeat}]
  (if (and (not vs/*no-simpl*) (some Bottom? types))
    (Bottom)
    (HSequential-maker types
                       (vec (or filters
                                (repeat (count types) (ind/-FS (ind/-top-fn)
                                                               (ind/-top-fn)))))
                       (vec (or objects
                                (repeat (count types) (ind/-empty-fn))))
                       rest
                       drest
                       (boolean repeat?)
                       (or kind :sequential))))

(t/ann compatible-HSequential-kind? [HSequentialKind HSequentialKind :-> t/Bool])
(defn compatible-HSequential-kind?
  "True if kind s is a subtype of kind t."
  [s t]
  (or (= s t)
      (= :sequential t)
      (and (= s :list)
           (= t :seq))))

(t/ann HeterogeneousList? [t/Any :-> t/Bool :filters {:then (is HSequential 0)}])
(defn HeterogeneousList? [t]
  (and (HSequential? t)
       (= :list (:kind t))))

(t/ann HeterogeneousList-maker [(t/Seqable Type) :-> Type])
(defn HeterogeneousList-maker [types]
  (-hsequential types :kind :list))

(t/ann HeterogeneousSeq? [t/Any :-> t/Bool :filters {:then (is HSequential 0)}])
(defn HeterogeneousSeq? [t]
  (and (HSequential? t)
       (= :seq (:kind t))))

(t/ann ^:no-check -hseq
       [(t/Seqable Type) & :optional {:filters (t/Seqable p/IFilterSet) :objects (t/Seqable p/IRObject)
                                      :rest (t/Nilable Type) :drest (t/Nilable DottedPretype) :repeat t/Bool}
        -> Type])
(defn -hseq
  [types & opts]
  (apply -hsequential types (concat opts [:kind :seq])))

(t/ann HeterogeneousVector? [t/Any :-> t/Bool :filters {:then (is HSequential 0)}])
(defn HeterogeneousVector? [t]
  (and (HSequential? t)
       (= :vector (:kind t))))

(t/ann ^:no-check -hvec
       [(t/Vec Type) & :optional {:filters (t/Seqable p/IFilterSet) :objects (t/Seqable p/IRObject)
                                  :rest (t/Nilable Type) :drest (t/Nilable DottedPretype) :repeat t/Bool}
        -> Type])
(defn -hvec
  [types & opts]
  (apply -hsequential types (concat opts [:kind :vector])))

(u/ann-record HSet [fixed :- (t/Set Type)
                    complete? :- t/Bool])
(u/def-type HSet [fixed complete?]
  "A constant set"
  [(every? Type? fixed)
   (set? fixed)
   (boolean? complete?)]
  :methods
  [p/TCType])

(t/ann -hset [(t/Set Type) & :optional {:complete? t/Bool} -> HSet])
(defn -hset [fixed & {:keys [complete?] :or {complete? true}}]
  (HSet-maker fixed complete?))

(u/ann-record PrimitiveArray [jtype :- Class,
                              input-type :- Type
                              output-type :- Type])
(u/def-type PrimitiveArray [jtype input-type output-type]
  "A Java Primitive array"
  [(class? jtype)
   (Type? input-type)
   (Type? output-type)]
  :methods
  [p/TCType])

;; Heterogeneous ops

(u/ann-record AssocType [target :- Type,
                         entries :- (t/Coll '[Type Type])
                         dentries :- (t/U nil DottedPretype)])
(u/def-type AssocType [target entries dentries]
  "An assoc[iate] operation on the type level"
  [(Type? target)
   (or (DottedPretype? dentries)
       (nil? dentries))
   (and (every? (con/hvector-c? Type? Type?) entries)
        (sequential? entries))]
  :methods
  [p/TCType])

(u/ann-record MergeType [types :- (t/Coll Type)])
(u/def-type MergeType [types]
  "Merge at the type level."
  [(every? Type? types)]
  :methods
  [p/TCType])

(u/ann-record DissocType [target :- Type,
                          keys :- (t/Coll Type)
                          dkeys :- (t/U nil DottedPretype)])
(u/def-type DissocType [target keys dkeys]
  "A dissoc[iate] operation on the type level"
  [(Type? target)
   (or (DottedPretype? dkeys)
       (nil? dkeys))
   (and (every? Type? keys)
        (sequential? keys))
   (not (and keys dkeys))]
  :methods
  [p/TCType])

(u/ann-record GetType [target :- Type,
                       key :- Type
                       not-found :- Type
                       target-fs :- p/IFilterSet
                       target-object :- p/IRObject])
(u/def-type GetType [target key not-found target-fs target-object]
  "get on the type level"
  [(Type? target)
   (Type? key)
   (Type? not-found)
   (p/IFilterSet? target-fs)
   (p/IRObject? target-object)]
  :methods
  [p/TCType])

(t/ann -get
       [Type Type & :optional {:not-found (t/U nil Type)
                               :target-fs (t/U nil p/IFilterSet)
                               :target-object (t/U nil p/IRObject)}
        -> GetType])
(defn -get
  [target key & {:keys [not-found target-fs target-object]}]
  (GetType-maker target key (or not-found -nil)
                 (or target-fs (ind/-FS (ind/-top-fn)
                                        (ind/-top-fn)))
                 (or target-object (ind/-empty-fn))))

;not a type, see KwArgsSeq
;; TODO support clojure 1.11 kw args format
(u/ann-record KwArgs [mandatory :- (t/Map Type Type)
                      optional  :- (t/Map Type Type)
                      complete? :- t/Bool
                      ;; TODO make nilable but possibly-empty
                      maybe-trailing-nilable-non-empty-map? :- t/Bool])
(u/def-type KwArgs [mandatory
                    optional
                    complete?
                    maybe-trailing-nilable-non-empty-map?]
  "Represents a flattened map as a regex op like clojure.spec/keys*.
  A set of mandatory and optional keywords"
  [(every? (con/hash-c? Value? Type?) [mandatory optional])
   (empty? (set/intersection (set (keys mandatory)) 
                             (set (keys optional))))
   (boolean? complete?)
   (boolean? maybe-trailing-nilable-non-empty-map?)])

(u/ann-record KwArgsSeq [kw-args-regex :- KwArgs])
(u/def-type KwArgsSeq [kw-args-regex]
  "A sequential seq representing a flattened map."
  [(KwArgs? kw-args-regex)]
  :methods
  [p/TCType])

(u/ann-record TopKwArgsSeq [])
(u/def-type TopKwArgsSeq []
  "Supertype of all KwArgsSeq's."
  []
  :methods [p/TCType])

(t/ann -any-kw-args-seq Type)
(def -any-kw-args-seq (TopKwArgsSeq-maker))

(u/ann-record KwArgsArray [kw-args-regex :- KwArgs])
(u/def-type KwArgsArray [kw-args-regex]
  "A Java array representing a flattened map."
  [(KwArgs? kw-args-regex)]
  :methods
  [p/TCType])

;;FIXME stackoverflow in type checker
(t/ann ^:no-check -kw-args [& :optional {:mandatory (t/Map Type Type)
                                         :optional (t/Map Type Type)
                                         :complete? t/Bool
                                         :maybe-trailing-nilable-non-empty-map? t/Bool}
                            -> KwArgs])
(defn -kw-args [& {:keys [mandatory optional
                          complete? maybe-trailing-nilable-non-empty-map?]
                   :or {mandatory {} optional {}
                        complete? false maybe-trailing-nilable-non-empty-map? false}}]
  {:post [(KwArgs? %)]}
  (KwArgs-maker mandatory optional complete? maybe-trailing-nilable-non-empty-map?))

;;FIXME apply + KwArgs
(t/ann ^:no-check -kw-args-seq [& :optional {:mandatory (t/Map Type Type)
                                             :optional (t/Map Type Type)
                                             :complete? t/Bool
                                             :maybe-trailing-nilable-non-empty-map? t/Bool}
                                -> KwArgsSeq])
(defn -kw-args-seq [& opt]
  {:post [(KwArgsSeq? %)]}
  (KwArgsSeq-maker (apply -kw-args opt)))

;;FIXME apply + KwArgs
(t/ann ^:no-check -kw-args-array [& :optional {:mandatory (t/Map Type Type)
                                               :optional (t/Map Type Type)
                                               :complete? t/Bool
                                               :maybe-trailing-nilable-non-empty-map? t/Bool}
                                  -> KwArgsArray])
(defn -kw-args-array [& opt]
  {:post [(KwArgsArray? %)]}
  (KwArgsArray-maker (apply -kw-args opt)))

(u/def-type Regex [types kind]
  "Type representing regular expressions of sexpr's.
  Also used as the kind of dotted variable when given kinds."
  [(vector? types)
   (do (case kind
         (:* :+ :?) (do (assert (every? (some-fn Regex? Type? Kind?) types))
                        (assert (= 1 (count types))))
         :cat (assert (every? (some-fn Type? Regex? DottedPretype? Kind?) types))
         (:alt :or) (assert (every? (some-fn Regex? Type? Kind?) types)))
       true)]
  :methods
  [p/TCAnyType
   p/TCKind])

(t/ann regex [(t/Vec (t/U Regex Type Kind)) t/Kw -> Regex])
(defn regex [types kind]
  {:post [(Regex? %)]}
  (case kind
    :+ (do (assert (= 1 (count types)))
           (regex [(first types) (regex types :*)] :cat))
    :cat (Regex-maker (into [] (mapcat (fn [t]
                                         (if (and (Regex? t)
                                                  (= :cat (:kind t)))
                                           (:types t)
                                           [t])))
                            types)
                      :cat)
    (Regex-maker types kind)))

;must go before Function
(u/ann-record Result [t :- Type,
                      fl :- p/IFilterSet
                      o :- p/IRObject])

(u/ann-record Function [dom :- (t/Vec Type),
                        rng :- Result,
                        rest :- (t/Nilable Type)
                        drest :- (t/Nilable DottedPretype)
                        kws :- (t/Nilable KwArgs)
                        prest :- (t/Nilable HSequential)
                        pdot :- (t/Nilable DottedPretype)
                        regex :- (t/Nilable Regex)
                        kind :- t/Kw])
(u/def-type Function [dom rng rest drest kws prest pdot regex kind]
  "A function arity, must be part of an intersection"
  [(vector? dom)
   (Result? rng)
   ;at most one of rest drest kws prest or pdot can be provided
   (= (cond-> 1 (= :fixed kind) dec) (count (filter identity [rest drest kws prest pdot regex])))
   (case kind
     :fixed true
     :rest rest
     :drest drest
     :kws kws
     :pdot pdot
     :prest prest
     :regex regex
     (throw (Exception. (str "Bad Function :kind " (pr-str kind)))))
   (or (nil? rest)
       (Type? rest))
   (or (nil? drest)
       (DottedPretype? drest))
   (or (nil? kws)
       (KwArgs? kws))
   (or (nil? prest)
       (and (HSequential? prest)
            ; we could have prest without repeat, but why would you do that
            (:repeat prest)
            (:types prest)))
   (or (nil? pdot)
       (and (DottedPretype? pdot)
            ; we could have pdot without repeat, but why would you do that
            (-> pdot :pre-type :repeat)
            (-> pdot :pre-type :types)))
   (or (nil? regex)
       (and (Regex? regex)
            (empty? dom)))]
  :computed-fields
  [kind (cond
          rest :rest
          drest :drest
          prest :prest
          pdot :pdot
          kws :kws
          regex :regex
          :else :fixed)
   ;; expensive, cache result
   dom (if (some-> dom meta ::valid-Function-dom deref (identical? dom))
         dom
         (let [_ (assert (every? (every-pred Type? (complement Regex?))
                                dom))
               tie (volatile! nil)
               dom (-> dom
                       vec
                       (with-meta (assoc (meta dom) ::valid-Function-dom tie)))]
           (vreset! tie dom)
           dom))]
  :ctor-meta {:private true}
  :methods
  [p/TCAnyType])

(u/ann-record TopFunction [])
(u/def-type TopFunction []
  "Supertype to all functions"
  []
  :methods
  [p/TCType])

(u/ann-record FnIntersection [types :- (t/NonEmptyVec Function)])
(u/def-type FnIntersection [types]
  "An ordered intersection of Functions."
  [(seq types)
   (vector? types)
   (every? Function? types)]
  :methods
  [p/TCType])

(u/ann-record CountRange [lower :- Number,
                          upper :- (t/U nil Number)])
(u/def-type CountRange [lower upper]
  "A sequence of count between lower (inclusive) and upper (inclusive).
  If upper is nil, between lower and infinity."
  [(nat-int? lower)
   (or (nil? upper)
       (and (nat-int? upper)
            (<= lower upper)))]
  :methods
  [p/TCType])

(t/ann make-CountRange [Number (t/U nil Number) :? -> CountRange])
(defn make-CountRange
  ([lower] (make-CountRange lower nil))
  ([lower upper] (CountRange-maker lower upper)))

(t/ann make-ExactCountRange [Number -> CountRange])
(defn make-ExactCountRange [c]
  {:pre [(nat-int? c)]}
  (make-CountRange c c))

(t/ann make-FnIntersection [Function :+ -> FnIntersection])
(defn make-FnIntersection [& fns]
  {:pre [(every? Function? fns)]}
  (let [fns (vec fns)]
    (assert (seq fns))
    (FnIntersection-maker fns)))

(u/ann-record NotType [type :- Type])
(u/def-type NotType [type]
  "A type that does not include type"
  [(Type? type)]
  :methods
  [p/TCType])

(u/ann-record DifferenceType [type :- Type
                              without :- (t/SortedSet Type)])
(u/def-type DifferenceType [type without]
  "A type that does not include type"
  [(Type? type)
   (every? Type? without)
   (set? without)
   (sorted? without)]
  :methods
  [p/TCType])

(t/ann -difference [Type :+ -> DifferenceType])
(defn -difference [t & without]
  {:pre [without]}
  (DifferenceType-maker t (sorted-type-set without)))

(u/ann-record Extends [extends :- (t/I (t/SortedSet Type)
                                       t/NonEmptyCount)
                       without :- (t/SortedSet Type)])
(u/def-type Extends [extends without]
  "A set of ancestors that always and never occur."
  [(every? Type? extends)
   (set? extends)
   (sorted? extends)
   (seq extends)
   (every? Type? without)
   (set? without)
   (sorted? without)]
  :methods
  [p/TCType])

(u/def-type Result [t fl o]
  "A result type with filter f and object o. NOT a type."
  [(Type? t)
   (p/IFilterSet? fl)
   (p/IRObject? o)]
  :methods
  [p/TCAnyType])

(declare ret TCResult? make-Result)

(u/ann-record TCResult [t :- Type
                        fl :- p/IFilterSet
                        o :- p/IRObject
                        opts :- (t/Map t/Any t/Any)])

(t/ann Result->TCResult [Result -> TCResult])
(defn Result->TCResult [{:keys [t fl o] :as r}]
  {:pre [(Result? r)]
   :post [(TCResult? %)]}
  (ret t fl o))

(t/ann TCResult->Result [TCResult -> Result])
(defn TCResult->Result [{:keys [t fl o] :as r}]
  {:pre [(TCResult? r)]
   :post [(Result? %)]}
  (make-Result t fl o))

(t/ann Result-type* [Result -> Type])
(defn Result-type* [r]
  {:pre [(Result? r)]
   :post [(Type? %)]}
  (:t r))

(t/ann ^:no-check Result-filter* [Result -> p/IFilterSet])
(defn Result-filter* [r]
  {:pre [(Result? r)]
   :post [(p/IFilterSet? %)]}
  (:fl r))

(t/ann ^:no-check Result-object* [Result -> p/IRObject])
(defn Result-object* [r]
  {:pre [(Result? r)]
   :post [(p/IRObject? %)]}
  (:o r))

(t/ann -bounds [Type Type -> Bounds])
(defn -bounds [u l]
  {:pre [(Type? u)
         (Type? l)]}
  (Bounds-maker u l))

(t/ann no-bounds Bounds)
(def no-bounds (-bounds -any -nothing))

(t/ann dotted-no-bounds Regex)
(def dotted-no-bounds (regex [no-bounds] :*))

(u/def-type TCResult [t fl o opts]
  "This record represents the result of type-checking an expression"
  [(Type? t)
   (p/IFilterSet? fl)
   (p/IRObject? o)
   (map? opts)]
  ;:methods
  ;[p/TCAnyType]
  )

(t/ann ret
       (t/IFn [Type -> TCResult]
              [Type p/IFilterSet p/IRObject :? -> TCResult]))
(defn ret
  "Convenience function for returning the type of an expression"
  ([t] 
   (ret t (ind/-FS (ind/-top-fn) (ind/-top-fn)) (ind/-empty-fn)))
  ([t f] 
   (ret t f (ind/-empty-fn)))
  ([t f o]
   {:pre [(AnyType? t)
          (p/IFilterSet? f)
          (p/IRObject? o)]
    :post [(TCResult? %)]}
   (TCResult-maker t f o {})))

(t/ann ret-t [TCResult -> Type])
(defn ret-t [r]
  {:pre [(TCResult? r)]
   :post [(AnyType? %)]}
  (:t r))

(t/ann ^:no-check ret-f [TCResult -> p/IFilterSet])
(defn ret-f [r]
  {:pre [(TCResult? r)]
   :post [(p/IFilterSet? %)]}
  (:fl r))

(t/ann ^:no-check ret-o [TCResult -> p/IRObject])
(defn ret-o [r]
  {:pre [(TCResult? r)]
   :post [(p/IRObject? %)]}
  (:o r))

(t/ann make-Result
       (t/IFn [Type -> Result]
              [Type (t/Nilable p/IFilterSet) (t/Nilable p/IRObject) :? -> Result]))
(defn make-Result
  "Make a result. ie. the range of a Function"
  ([t] (make-Result t nil nil))
  ([t f] (make-Result t f nil))
  ([t f o]
   (Result-maker t 
                 (or f (ind/-FS (ind/-top-fn) (ind/-top-fn))) ;;TODO use (fo/-simple-filter)
                 (or o (ind/-empty-fn)))))

(t/ann ^:no-check make-Function
       [(t/Seqable Type)
        (t/U Type Result)
        & :optional
        {:rest (t/Nilable Type) :drest (t/Nilable Type) :prest (t/Nilable Type)
         :pdot (t/Nilable DottedPretype)
         :filter (t/Nilable p/IFilterSet) :object (t/Nilable p/IRObject)
         :mandatory-kws (t/Nilable (t/Map Type Type))
         :optional-kws (t/Nilable (t/Map Type Type))
         :kws (t/Nilable KwArgs)}
        -> Function])
(defn make-Function
  "Make a function, wrap range type in a Result.
  Accepts optional :filter and :object parameters that default to the most general filter
  and EmptyObject"
  [dom rng & {:keys [rest drest prest pdot filter object mandatory-kws optional-kws kws regex] :as opt}]
  {:pre [(every? keyword? (keys opt))
         (if (Result? rng)
           (not (or filter object))
           (Type? rng))]}
  (assert (not (:flow opt)) "removed this feature")
  (let [kws (if (or mandatory-kws optional-kws)
              (do (assert (not kws)
                          "Cannot combine :mandatory-kws or :optional-kws with :kws")
                  (-kw-args :mandatory (or mandatory-kws {})
                            :optional (or optional-kws {})))
              kws)]
    (Function-maker (vec dom)
                    (cond-> rng
                      (not (Result? rng)) (make-Result filter object))
                    rest
                    drest
                    kws
                    prest
                    pdot
                    regex
                    ;; kind (computed)
                    nil)))

;; Symbolic closures

(def ^:dynamic enable-symbolic-closures? true #_false)

(u/ann-record SymbolicClosure [bindings :- (t/Map t/Any t/Any)
                               fexpr :- (t/Map t/Any t/Any)
                               smallest-type :- Type])
(u/def-type SymbolicClosure [bindings fexpr smallest-type]
  "Symbolic closure"
  [(map? bindings)
   (map? fexpr)
   (Type? smallest-type)]
  :methods
  [p/TCType])

(t/ann symbolic-closure [(t/Map t/Any t/Any) Type :-> SymbolicClosure])
(defn symbolic-closure [fexpr smallest-type]
  ;(prn "creating symbolic-closure")
  (SymbolicClosure-maker (get-thread-bindings) fexpr smallest-type))

;;;;;;;;;;;;;;;;;
;; Clojurescript types

(u/ann-record JSUndefined [])
(u/def-type JSUndefined []
  "JavaScript undefined"
  []
  :methods
  [p/TCType])

(u/ann-record JSNull [])
(u/def-type JSNull []
  "JavaScript null"
  []
  :methods
  [p/TCType])

(u/ann-record JSBoolean [])
(u/def-type JSBoolean []
  "JavaScript primitive boolean"
  []
  :methods
  [p/TCType])

(u/ann-record JSObject [])
(u/def-type JSObject []
  "Any JavaScript object"
  []
  :methods
  [p/TCType])

(u/ann-record JSString [])
(u/def-type JSString []
  "JavaScript primitive string"
  []
  :methods
  [p/TCType])

(u/ann-record JSSymbol [])
(u/def-type JSSymbol []
  "JavaScript primitive symbol"
  []
  :methods
  [p/TCType])

(u/ann-record JSNumber [])
(u/def-type JSNumber []
  "JavaScript number"
  []
  :methods
  [p/TCType])

(u/ann-record CLJSInteger [])
(u/def-type CLJSInteger []
  "ClojureScript integer. Represents a primitive
  JavaScript number with no decimal places (values
  that pass `cljs.core/integer?`)."
  []
  :methods
  [p/TCType])

(t/ann -integer-cljs Type)
(def -integer-cljs (CLJSInteger-maker))

(u/ann-record ArrayCLJS [input-type :- Type
                         output-type :- Type])
(u/def-type ArrayCLJS [input-type output-type]
  "Primitive array in CLJS"
  [(Type? input-type)
   (Type? output-type)]
  :methods
  [p/TCType])

(u/def-type FunctionCLJS []
  "Primitive function in CLJS"
  []
  :methods
  [p/TCType])
