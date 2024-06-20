;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.frees
  (:require [typed.clojure :as t]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.fold-rep :as f]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.object-rep]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.name-env :as nmenv]
            [typed.cljc.checker.declared-kind-env :as kinds]
            [typed.cljc.runtime.env :as env])
  (:import (typed.cljc.checker.type_rep NotType Intersection Union FnIntersection Bounds
                                        Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly
                                        Mu HeterogeneousMap KwArgs
                                        CountRange Name Value Top Wildcard Unchecked TopFunction B F Result AnyValue
                                        Scope TCError AssocType GetType MergeType Regex HSequential HSet
                                        JSObj TypeOf MatchType Instance Satisfies)
           (typed.cljc.checker.filter_rep FilterSet TypeFilter NotTypeFilter ImpFilter
                                          AndFilter OrFilter TopFilter BotFilter)
           (typed.cljc.checker.object_rep Path EmptyObject NoObject)
           (typed.cljc.checker.path_rep NthPE NextPE ClassPE CountPE KeyPE KeysPE ValsPE KeywordPE SeqPE)))

(set! *warn-on-reflection* true)

;; private to this namespace, for performance
; frees : VarianceMap
; idxs : VarianceMap
(deftype FreesResult [frees idxs])

(def ^:private -empty-frees-result (FreesResult. {} {}))

(defn ^:private empty-frees-result? [^FreesResult fr]
  (and (empty? (.frees fr))
       (empty? (.idxs fr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(t/defalias VarianceEntry
  "A map entry of a VarianceMap."
  '[t/Sym r/Variance])

(t/defalias VarianceMap
  "A map of free names (symbols) to their variances"
  (t/Map t/Sym r/Variance))

(defprotocol ^:private IFrees
  (^:private ^FreesResult frees [t opts]))

(t/ann ^:no-check variance-map? (t/Pred VarianceMap))
(def variance-map? (con/hash-c? symbol? r/variance?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exposed interface

(defn flip-variance [vari]
  (case vari
    :covariant :contravariant
    :contravariant :covariant
    vari))

(defn flip-variance-map [vs]
  (update-vals vs flip-variance))

(t/ann fv-variances [r/AnyType t/Any -> VarianceMap])
(defn fv-variances
  "Map of frees to their variances"
  [t opts]
  {:post [(variance-map? %)]}
  (.frees (frees t opts)))

(t/ann idx-variances [r/AnyType -> VarianceMap])
(defn idx-variances 
  "Map of indexes to their variances"
  [t opts]
  {:post [(variance-map? %)]}
  (.idxs (frees t opts)))

(t/ann free-variances [r/AnyType t/Any -> '{:frees VarianceMap
                                            :idxs VarianceMap}])
(defn free-variances
  "Variances of all type and index variables in type"
  [t opts]
  {:post [((con/hmap-c? :frees variance-map? :idxs variance-map?) %)]}
  (let [f (frees t opts)]
    {:frees (.frees f) :idxs (.idxs f)}))

(t/ann fv+idx-variances [r/AnyType t/Any -> (t/Set t/Sym)])
(defn fv+idx-variances
  "Variances of all type and index variables in type"
  [t opts]
  {:post [(variance-map? %)]}
  (let [f (frees t opts)]
    (into (.frees f) (.idxs f))))

(t/ann fv [r/AnyType t/Any -> (t/Set t/Sym)])
(defn fv
  "All frees in type"
  [t opts]
  {:post [((con/set-c? symbol?) %)]}
  (set (keys (fv-variances t opts))))

(t/ann fi [r/AnyType t/Any -> (t/Set t/Sym)])
(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t opts]
  {:post [((con/set-c? symbol?) %)]}
  (set (keys (idx-variances t opts))))

(t/ann combine-frees [VarianceMap :* :-> VarianceMap])
(defn combine-frees [& frees]
  {:post [(map? %)]}
  (if frees
    (apply merge-with (fn [old-vari new-vari]
                        (cond 
                          (= old-vari new-vari) old-vari
                          (= old-vari :dotted) new-vari
                          (= new-vari :dotted) old-vari
                          (= old-vari :constant) new-vari
                          (= new-vari :constant) old-vari
                          :else :invariant))
           frees)
    {}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation 

(t/ann flip-variances [FreesResult -> FreesResult])
(defn ^FreesResult ^:private flip-variances [^FreesResult fr]
  {:pre [(instance? FreesResult fr)]
   :post [(instance? FreesResult %)]}
  (FreesResult. (flip-variance-map (.frees fr))
                (flip-variance-map (.idxs fr))))

(defn ^FreesResult ^:private invariant-variances [^FreesResult fr]
  {:pre [(instance? FreesResult fr)]
   :post [(instance? FreesResult %)]}
  (let [inv (fn [vs]
              (zipmap (keys vs) (repeat :invariant)))]
    (FreesResult. (inv (.frees fr))
                  (inv (.idxs fr)))))

(t/ann combine-freesresults [FreesResult :* :-> FreesResult])
(defn ^FreesResult ^:private combine-freesresults [& frees]
  {:post [(instance? FreesResult %)]}
  (reduce 
    (fn [^FreesResult r1 ^FreesResult r2]
      (FreesResult. (combine-frees (.frees r1) (.frees r2))
                    (combine-frees (.idxs r1) (.idxs r2))))
    -empty-frees-result
    frees))

;;TODO attempt to rewrite using the new second "info" argument to type-rec
(extend-protocol IFrees
  Result 
  (frees [t opts]
    (t/ann-form t Result)
    (let [{:keys [t fl o]} t]
      (combine-freesresults (frees t opts)
                            (frees fl opts)
                            (frees o opts))))
  ;; Filters
  FilterSet
  (frees [{:keys [then else]} opts]
    (combine-freesresults (frees then opts)
                          (frees else opts)))

  TypeFilter
  (frees [{:keys [type]} opts] (frees type opts))

  NotTypeFilter
  (frees [{:keys [type]} opts] (flip-variances (frees type opts)))

  ImpFilter
  (frees [{:keys [a c]} opts]
    (combine-freesresults (frees a opts)
                          (frees c opts)))
  AndFilter
  (frees [{:keys [fs]} opts]
    (apply combine-freesresults (map #(frees % opts) fs)))

  OrFilter
  (frees [{:keys [fs]} opts]
    (apply combine-freesresults (map #(frees % opts) fs)))

  TopFilter 
  (frees [t opts] -empty-frees-result)

  BotFilter 
  (frees [t opts] -empty-frees-result)

  ;; Objects

  Path
  (frees 
    [{:keys [path]} opts]
    (apply combine-freesresults (map #(frees % opts) path)))

  EmptyObject 
  (frees [t opts] -empty-frees-result)
  NoObject 
  (frees [t opts] -empty-frees-result)

  NthPE 
  (frees [t opts] -empty-frees-result)
  NextPE 
  (frees [t opts] -empty-frees-result)
  ClassPE 
  (frees [t opts] -empty-frees-result)
  CountPE 
  (frees [t opts] -empty-frees-result)
  KeyPE 
  (frees [t opts] -empty-frees-result)
  KeysPE 
  (frees [t opts] -empty-frees-result)
  ValsPE 
  (frees [t opts] -empty-frees-result)
  KeywordPE 
  (frees [t opts] -empty-frees-result)
  SeqPE 
  (frees [t opts] -empty-frees-result)

  F
  (frees
    [{:keys [name] :as t} opts]
    (FreesResult. {name :covariant} {}))

  TCError 
  (frees [t opts] -empty-frees-result)
  B
  (frees [t opts] -empty-frees-result)
  CountRange 
  (frees [t opts] -empty-frees-result)
  Value 
  (frees [t opts] -empty-frees-result)
  AnyValue 
  (frees [t opts] -empty-frees-result)
  Top 
  (frees [t opts] -empty-frees-result)
  Wildcard 
  (frees [t opts] -empty-frees-result)
  Unchecked 
  (frees [t opts] -empty-frees-result)
  Name 
  (frees [t opts] -empty-frees-result)
  TypeOf 
  (frees [t opts] -empty-frees-result)
  Instance 
  (frees [t opts] -empty-frees-result)
  Satisfies 
  (frees [t opts] -empty-frees-result)

  DataType
  (frees 
    [{varis :variances args :poly? :as t} opts]
    (assert (= (count args) (count varis)))
    (apply combine-freesresults (map (fn [arg va]
                                       (let [fr (frees arg opts)]
                                         (case va
                                           :covariant fr
                                           :contravariant (flip-variances fr)
                                           :invariant (invariant-variances fr))))
                                     args
                                     varis)))

  App
  (frees
    [{:keys [rator rands]} opts]
    (apply combine-freesresults (map #(frees % opts) (cons rator rands))))

  TApp
  (frees
    [{:keys [rator rands] :as tapp} opts]
    (apply combine-freesresults
           (frees rator opts)
           (let [checker (env/checker opts)
                 tfn (loop [rator rator]
                       (cond
                         (r/F? rator) (when-let [bnds (free-ops/free-with-name-bnds (:name rator))]
                                        ;assume upper/lower bound variance agree
                                        (c/fully-resolve-type (:upper-bound bnds) opts))
                         (r/Name? rator) (let [{:keys [id]} rator]
                                           (cond
                                             (nmenv/declared-name? id opts)
                                             (kinds/get-declared-kind checker id opts)

                                             ; alter class introduces temporary declared kinds for
                                             ; computing variance when referencing an RClass inside
                                             ; its own definition.
                                             (and (class? (resolve id))
                                                  (kinds/has-declared-kind? checker id opts))
                                             (kinds/get-declared-kind checker id opts)

                                             :else
                                             (recur (c/resolve-Name rator opts))))
                         (r/TypeFn? rator) rator
                         :else (err/int-error (str "Invalid operator to type application: "
                                                   (prs/unparse-type tapp opts))
                                              opts)))
                 _ (when-not (r/TypeFn? tfn)
                     (err/int-error (str "First argument to TApp must be TypeFn: "
                                         (prs/unparse-type tapp opts))
                                    opts))
                 vs (let [f (.-variances ^TypeFn tfn)]
                      (if (fn? f)
                        (f opts)
                        f))]
             (map (fn [v ^FreesResult fr]
                    (case v
                      :covariant fr
                      :contravariant (flip-variances fr)
                      :invariant (invariant-variances fr)))
                  vs
                  (map #(frees % opts) rands)))))

  PrimitiveArray
  (frees 
    [{:keys [input-type output-type]} opts]
    (combine-freesresults (flip-variances (frees input-type opts))
                          (frees output-type opts)))

  HeterogeneousMap
  (frees 
    [{:keys [types optional]} opts]
    (apply combine-freesresults
           (map #(frees % opts) (concat (keys types)
                                        (vals types)
                                        (keys optional)
                                        (vals optional)))))

  KwArgs
  (frees 
    [{:keys [mandatory optional]} opts]
    (apply combine-freesresults
           (map #(frees % opts) (concat (keys mandatory)
                                        (vals mandatory)
                                        (keys optional)
                                        (vals optional)))))

  JSObj
  (frees 
    [{:keys [types]} opts]
    (apply combine-freesresults (map #(frees % opts) (vals types))))

  HSequential
  (frees 
    [{:keys [types fs objects rest drest]} opts]
    (apply combine-freesresults (concat (mapv #(frees % opts) (concat types fs objects))
                                        (when rest [(frees rest opts)])
                                        (when drest
                                          [(let [fr (-> (:pre-type drest) (frees opts))]
                                             (FreesResult.
                                               (-> (.frees fr) (dissoc (:name drest)))
                                               (.idxs fr)))]))))

  HSet
  (frees 
    [{:keys [fixed]} opts]
    (apply combine-freesresults (map #(frees % opts) fixed)))

  MergeType
  (frees 
    [{:keys [types]} opts]
    (apply combine-freesresults (map #(frees % opts) types)))

  GetType
  (frees 
    [{:keys [target key not-found target-fs target-object]} opts]
    (combine-freesresults (frees target opts)
                          (frees key opts)
                          (frees not-found opts)
                          (frees target-fs opts)
                          (frees target-object opts)))

  Regex
  (frees 
    [{:keys [types]} opts]
    (apply combine-freesresults (map #(frees % opts) types)))

  AssocType
  (frees 
    [{:keys [target entries dentries]} opts]
    (apply combine-freesresults
           (frees target opts)
           (concat (map #(frees % opts) (apply concat entries))
                   (when-let [{:keys [name pre-type]} dentries]
                     (let [fr (frees pre-type opts)]
                       (assert (symbol? name))
                       [(FreesResult.
                          (-> (.frees fr) (dissoc name))
                          (-> (.idxs fr) (assoc name :covariant)))])))))

; are negative types covariant?
  NotType
  (frees 
    [{:keys [type]} opts]
    (frees type opts))

  Intersection
  (frees 
    [{:keys [types]} opts]
    (apply combine-freesresults (map #(frees % opts) types)))

  Union
  (frees 
    [{:keys [types]} opts]
    (apply combine-freesresults (map #(frees % opts) types)))

  FnIntersection
  (frees 
    [{:keys [types]} opts]
    (apply combine-freesresults (map #(frees % opts) types)))

  Function
  (frees 
    [{:keys [dom rng rest drest kws prest pdot kind]} opts]
    {:pre [(#{:fixed :rest :drest :kws :prest :pdot} kind)]}
    (apply combine-freesresults (concat (map (comp flip-variances #(frees % opts))
                                             (concat dom
                                                     (when rest
                                                       [rest])
                                                     (when kws
                                                       [kws])
                                                     (when prest
                                                       [prest])))
                                        [(frees rng opts)]
                                        (keep
                                          #(when-let [{:keys [name pre-type]} %]
                                             (assert (symbol? name))
                                             (let [fr (-> pre-type (frees opts) flip-variances)]
                                               (FreesResult.
                                                 (-> (.frees fr) (dissoc name))
                                                 (-> (.idxs fr) (assoc name :contravariant)))))
                                          [drest pdot]))))

  RClass
  (frees 
    [t opts]
    (let [varis (:variances t)
          args (:poly? t)]
      (when-not (= (count args) (count varis))
        (err/int-error (str "Wrong number of arguments passed to class "
                            (:the-class t) ": expected "
                            (count varis) ", given " (count args) ".")
                       opts))
      (apply combine-freesresults (map (fn [arg va]
                                         (let [fr (frees arg opts)]
                                           (case va
                                             :covariant fr
                                             :contravariant (flip-variances fr)
                                             :invariant (invariant-variances fr))))
                                       args
                                       varis))))

  Protocol
  (frees 
    [{varis :variances, args :poly?, :as t} opts]
    (assert (= (count args) (count varis)))
    (apply combine-freesresults (map (fn [arg va]
                                       (let [fr (frees arg opts)]
                                         (case va
                                           :covariant fr
                                           :contravariant (flip-variances fr)
                                           :invariant (invariant-variances fr))))
                                     args
                                     varis)))

  MatchType
  (frees
    [t opts]
    (invariant-variances
      (apply combine-freesresults
             (frees (:target t) opts)
             (map #(frees % opts) (:clauses t)))))

  Scope
  (frees 
    [{:keys [body]} opts]
    (frees body opts))

  Bounds
  (frees 
    [{:keys [upper-bound lower-bound]} opts]
    (combine-freesresults (frees upper-bound opts)
                          (frees lower-bound opts)))

;FIXME Type variable bounds should probably be checked for frees
  TypeFn
  (frees 
    [{:keys [scope bbnds]} opts]
    (let [_ (assert (every? empty-frees-result? (map #(frees % opts) bbnds))
                    "NYI Handle frees in bounds")]
      (frees scope opts)))

  Poly
  (frees 
    [{:keys [scope bbnds kind]} opts]
    (case kind
      (:Poly :PolyDots)
      (let [_ (when-not (every? empty-frees-result? (map #(frees % opts) bbnds))
                (err/nyi-error "NYI Handle frees in bounds" opts))]
        (frees scope opts))))

  Mu
  (frees 
    [{:keys [scope]} opts]
    (frees scope opts))

;;js types
  typed.cljc.checker.type_rep.JSBoolean 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.JSObject 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.JSString 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.JSSymbol 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.JSNumber 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.CLJSInteger 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.ArrayCLJS 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.FunctionCLJS 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.JSUndefined 
  (frees [t opts] -empty-frees-result)
  typed.cljc.checker.type_rep.JSNull 
  (frees [t opts] -empty-frees-result))
