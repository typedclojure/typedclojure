;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:skip-wiki clojure.core.typed.checker.frees
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.checker.type-ctors :as c]
            [clojure.core.typed.checker.object-rep]
            [clojure.core.typed.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.checker.filter-rep :as fr]
            [clojure.core.typed.checker.free-ops :as free-ops]
            [clojure.core.typed.checker.name-env :as nmenv]
            [clojure.core.typed.checker.declared-kind-env :as kinds])
  (:import (clojure.core.typed.checker.type_rep NotType DifferenceType Intersection Union FnIntersection Bounds
                                        Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Unchecked TopFunction B F Result AnyValue
                                        Scope TCError Extends AssocType HSequential HSet
                                        JSObj TypeOf)
           (clojure.core.typed.checker.filter_rep FilterSet TypeFilter NotTypeFilter ImpFilter
                                          AndFilter OrFilter TopFilter BotFilter)
           (clojure.core.typed.checker.object_rep Path EmptyObject NoObject)
           (clojure.core.typed.checker.path_rep NthPE NextPE ClassPE CountPE KeyPE KeysPE ValsPE KeywordPE)))

(set! *warn-on-reflection* true)

;TODO make this an argument
(t/ann *frees-mode* (t/U nil t/Kw))
(defonce ^:dynamic *frees-mode* nil)
(t/tc-ignore
(set-validator! #'*frees-mode* (some-fn #{::frees ::idxs} nil?))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Collecting frees

(t/defalias VarianceEntry
  "A map entry of a VarianceMap."
  '[t/Sym r/Variance])

(t/defalias VarianceMap
  "A map of free names (symbols) to their variances"
  (t/Map t/Sym r/Variance))

(t/ann ^:no-check variance-map? (t/Pred VarianceMap))
(def variance-map? (con/hash-c? symbol? r/variance?))

(declare frees-in)

(t/ann fv-variances [r/AnyType -> VarianceMap])
(defn fv-variances 
  "Map of frees to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::frees]
    (frees-in t)))

(t/ann idx-variances [r/AnyType -> VarianceMap])
(defn idx-variances 
  "Map of indexes to their variances"
  [t]
  {:post [(variance-map? %)]}
  (binding [*frees-mode* ::idxs]
    (frees-in t)))

(t/ann fv [r/AnyType -> (t/Set t/Sym)])
(defn fv 
  "All frees in type"
  [t]
  {:post [((con/set-c? symbol?) %)]}
  (set (keys (fv-variances t))))

(t/ann fi [r/AnyType -> (t/Set t/Sym)])
(defn fi
  "All index variables in type (dotted bounds, etc.)"
  [t]
  {:post [((con/set-c? symbol?) %)]}
  (set (keys (idx-variances t))))

;; private to this namespace, for performance
; frees : VarianceMap
; idxs : VarianceMap
(deftype FreesResult [frees idxs])

(def ^:private -empty-frees-result (FreesResult. {} {}))

(defn ^:private empty-frees-result? [^FreesResult fr]
  (and (empty? (.frees fr))
       (empty? (.idxs fr))))

(t/ann flip-variances [FreesResult -> FreesResult])
(defn ^FreesResult flip-variances [^FreesResult fr]
  {:pre [(instance? FreesResult fr)]
   :post [(instance? FreesResult %)]}
  (let [flp (fn [vs]
              (zipmap (keys vs) 
                      (map (t/fn [vari :- r/Variance]
                             (case vari
                               :covariant :contravariant
                               :contravariant :covariant
                               vari))
                           (vals vs))))]
    (FreesResult. (flp (.frees fr))
                  (flp (.idxs fr)))))

(defn ^FreesResult invariant-variances [^FreesResult fr]
  {:pre [(instance? FreesResult fr)]
   :post [(instance? FreesResult %)]}
  (let [inv (fn [vs]
              (zipmap (keys vs) (repeat :invariant)))]
    (FreesResult. (inv (.frees fr))
                  (inv (.idxs fr)))))

(t/ann combine-frees [VarianceMap * -> VarianceMap])
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

(t/ann combine-freesresults [FreesResult * -> FreesResult])
(defn ^FreesResult ^:private combine-freesresults [& frees]
  {:post [(instance? FreesResult %)]}
  (reduce 
    (fn [^FreesResult r1 ^FreesResult r2]
      (FreesResult. (combine-frees (.frees r1) (.frees r2))
                    (combine-frees (.idxs r1) (.idxs r2))))
    -empty-frees-result
    frees))

(derive ::frees ::any-var)
(derive ::idxs ::any-var)

(t/ann frees [t/Any -> VarianceMap])
(defmulti ^:private ^FreesResult frees (fn [t] [*frees-mode* (class t)]))

(t/ann frees-in [r/AnyType -> VarianceMap])
(defn frees-in [t]
  {:post [(variance-map? %)]}
  (let [fr (frees t)]
    (case *frees-mode*
      ::frees (.frees fr)
      ::idxs (.idxs fr))))

(defmethod frees [::any-var Result]
  [t]
  (t/ann-form t Result)
  (let [{:keys [t fl o]} t]
    (combine-freesresults (frees t)
                          (frees fl)
                          (frees o))))

;; Filters

(defmethod frees [::any-var FilterSet]
  [{:keys [then else]}]
  (combine-freesresults (frees then)
                        (frees else)))

(defmethod frees [::any-var TypeFilter]
  [{:keys [type]}]
  (frees type))

(defmethod frees [::any-var NotTypeFilter]
  [{:keys [type]}] 
  (flip-variances (frees type)))

(defmethod frees [::any-var ImpFilter]
  [{:keys [a c]}] 
  (combine-freesresults (frees a)
                        (frees c)))

(defmethod frees [::any-var AndFilter]
  [{:keys [fs]}] 
  (apply combine-freesresults (map frees fs)))

(defmethod frees [::any-var OrFilter]
  [{:keys [fs]}]
  (apply combine-freesresults (map frees fs)))

(defmethod frees [::any-var TopFilter] [t] -empty-frees-result)
(defmethod frees [::any-var BotFilter] [t] -empty-frees-result)

;; Objects

(defmethod frees [::any-var Path]
  [{:keys [path]}]
  (apply combine-freesresults (map frees path)))

(defmethod frees [::any-var EmptyObject] [t] -empty-frees-result)
(defmethod frees [::any-var NoObject] [t] -empty-frees-result)

(defmethod frees [::any-var NthPE] [t] -empty-frees-result)
(defmethod frees [::any-var NextPE] [t] -empty-frees-result)
(defmethod frees [::any-var ClassPE] [t] -empty-frees-result)
(defmethod frees [::any-var CountPE] [t] -empty-frees-result)
(defmethod frees [::any-var KeyPE] [t] -empty-frees-result)
(defmethod frees [::any-var KeysPE] [t] -empty-frees-result)
(defmethod frees [::any-var ValsPE] [t] -empty-frees-result)
(defmethod frees [::any-var KeywordPE] [t] -empty-frees-result)


(defmethod frees [::any-var F]
  [{:keys [name] :as t}]
  (FreesResult. {name :covariant} {}))

(defmethod frees [::any-var TCError] [t] -empty-frees-result)
(defmethod frees [::any-var B] [t] -empty-frees-result)
(defmethod frees [::any-var CountRange] [t] -empty-frees-result)
(defmethod frees [::any-var Value] [t] -empty-frees-result)
(defmethod frees [::any-var AnyValue] [t] -empty-frees-result)
(defmethod frees [::any-var Top] [t] -empty-frees-result)
(defmethod frees [::any-var Unchecked] [t] -empty-frees-result)
(defmethod frees [::any-var Name] [t] -empty-frees-result)
(defmethod frees [::any-var TypeOf] [t] -empty-frees-result)

(defmethod frees [::any-var DataType]
  [{varis :variances args :poly? :as t}]
  (assert (= (count args) (count varis)))
  (apply combine-freesresults (map (fn [arg va]
                              (let [fr (frees arg)]
                                (case va
                                  :covariant fr
                                  :contravariant (flip-variances fr)
                                  :invariant (invariant-variances fr))))
                            args
                            varis)))

(defmethod frees [::any-var App]
  [{:keys [rator rands]}]
  (apply combine-freesresults (map frees (cons rator rands))))

(def ^:private unparse-type (delay (impl/dynaload 'clojure.core.typed.checker.jvm.parse-unparse/unparse-type)))

(defmethod frees [::any-var TApp]
  [{:keys [rator rands] :as tapp}]
  (apply combine-freesresults
         (let [tfn (loop [rator rator]
                     (cond
                       (r/F? rator) (when-let [bnds (free-ops/free-with-name-bnds (:name rator))]
                                      ;assume upper/lower bound variance agree
                                      (c/fully-resolve-type (:upper-bound bnds)))
                       (r/Name? rator) (let [{:keys [id]} rator]
                                         (cond
                                           (nmenv/declared-name? id)
                                           (kinds/get-declared-kind id)

                                           ; alter class introduces temporary declared kinds for
                                           ; computing variance when referencing an RClass inside
                                           ; its own definition.
                                           (and (class? (resolve id))
                                                (kinds/has-declared-kind? id))
                                           (kinds/get-declared-kind id)

                                           :else
                                           (recur (c/resolve-Name rator))))
                       (r/TypeFn? rator) rator
                       :else (err/int-error (str "Invalid operator to type application: "
                                               (@unparse-type tapp)))))
               _ (when-not (r/TypeFn? tfn) 
                   (err/int-error (str "First argument to TApp must be TypeFn")))]
           (map (fn [v ^FreesResult fr]
                  (case v
                    :covariant fr
                    :contravariant (flip-variances fr)
                    :invariant (invariant-variances fr)))
                (:variances tfn)
                (map frees rands)))))

(defmethod frees [::any-var PrimitiveArray]
  [{:keys [input-type output-type]}] 
  (combine-freesresults (flip-variances (frees input-type))
                        (frees output-type)))

(defmethod frees [::any-var HeterogeneousMap]
  [{:keys [types optional]}]
  (apply combine-freesresults
         (map frees (concat (keys types)
                            (vals types)
                            (keys optional)
                            (vals optional)))))

(defmethod frees [::any-var JSObj]
  [{:keys [types]}]
  (apply combine-freesresults (map frees (vals types))))

(defmethod frees [::any-var HSequential]
  [{:keys [types fs objects rest drest]}]
  (apply combine-freesresults (concat (mapv frees (concat types fs objects))
                               (when rest [(frees rest)])
                               (when drest
                                 [(let [fr (-> (:pre-type drest) frees)]
                                    (FreesResult.
                                      (-> (.frees fr) (dissoc (:name drest)))
                                      (.idxs fr)))]))))

(defmethod frees [::any-var HSet]
  [{:keys [fixed]}]
  (apply combine-freesresults (map frees fixed)))

(defmethod frees [::any-var Extends]
  [{:keys [extends without]}] 
  (apply combine-freesresults (map frees (concat extends without))))

(defmethod frees [::any-var AssocType]
  [{:keys [target entries dentries]}]
  (apply combine-freesresults
         (frees target)
         (concat (map frees (apply concat entries))
                 (when-let [{:keys [name pre-type]} dentries]
                   (let [fr (frees pre-type)]
                     (assert (symbol? name))
                     [(FreesResult.
                        (-> (.frees fr) (dissoc name))
                        (-> (.idxs fr) (assoc name :covariant)))])))))

; are negative types covariant?
(defmethod frees [::any-var NotType]
  [{:keys [type]}] 
  (frees type))

(defmethod frees [::any-var Intersection]
  [{:keys [types]}] 
  (apply combine-freesresults (map frees types)))

; are negative types covariant?
(defmethod frees [::any-var DifferenceType]
  [{:keys [type without]}] 
  (apply combine-freesresults (frees type) (map frees without)))

(defmethod frees [::any-var Union]
  [{:keys [types]}]
  (apply combine-freesresults (map frees types)))

(defmethod frees [::any-var FnIntersection]
  [{:keys [types]}] 
  (apply combine-freesresults (map frees types)))

(defmethod frees [::any-var Function]
  [{:keys [dom rng rest drest kws prest pdot]}]
  (apply combine-freesresults (concat (map (comp flip-variances frees)
                                    (concat dom
                                            (when rest
                                              [rest])
                                            (when kws
                                              [(vals kws)])
                                            (when prest
                                              [prest])))
                               [(frees rng)]
                               (keep
                                 #(when-let [{:keys [name pre-type]} %]
                                    (assert (symbol? name))
                                    (let [fr (-> pre-type frees flip-variances)]
                                      (FreesResult.
                                        (-> (.frees fr) (dissoc name))
                                        (-> (.idxs fr) (assoc name :contravariant)))))
                                 [drest pdot]))))

(defmethod frees [::any-var RClass]
  [t]
  (let [varis (:variances t)
        args (:poly? t)]
    (assert (= (count args) (count varis)))
    (apply combine-freesresults (map (fn [arg va]
                                       (let [fr (frees arg)]
                                         (case va
                                           :covariant fr
                                           :contravariant (flip-variances fr)
                                           :invariant (invariant-variances fr))))
                                     args
                                     varis))))

(defmethod frees [::any-var Protocol]
  [{varis :variances, args :poly?, :as t}]
  (assert (= (count args) (count varis)))
  (apply combine-freesresults (map (fn [arg va]
                              (let [fr (frees arg)]
                                (case va
                                  :covariant fr
                                  :contravariant (flip-variances fr)
                                  :invariant (invariant-variances fr))))
                            args
                            varis)))

(defmethod frees [::any-var Scope]
  [{:keys [body]}]
  (frees body))

(defmethod frees [::any-var Bounds]
  [{:keys [upper-bound lower-bound]}]
  (combine-freesresults (frees upper-bound)
                        (frees lower-bound)))

;FIXME Type variable bounds should probably be checked for frees
(defmethod frees [::any-var TypeFn]
  [{:keys [scope bbnds]}]
  (let [_ (assert (every? empty-frees-result? (map frees bbnds))
                  "NYI Handle frees in bounds")]
    (frees scope)))

(defmethod frees [::any-var Poly]
  [{:keys [scope bbnds]}]
  (let [_ (when-not (every? empty-frees-result? (map frees bbnds))
            (err/nyi-error "NYI Handle frees in bounds"))]
    (frees scope)))

(defmethod frees [::any-var Mu]
  [{:keys [scope]}]
  (frees scope))

(defmethod frees [::any-var PolyDots]
  [{:keys [scope bbnds]}]
  (let [_ (when-not (every? empty-frees-result? (map frees bbnds))
            (err/nyi-error "NYI Handle frees in bounds"))]
    (frees scope)))

;;js types
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSBoolean] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSObject] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSString] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSSymbol] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSNumber] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.CLJSInteger] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.ArrayCLJS] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.FunctionCLJS] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSUndefined] [t] -empty-frees-result)
(defmethod frees [::any-var clojure.core.typed.checker.type_rep.JSNull] [t] -empty-frees-result)
