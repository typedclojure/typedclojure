;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.fold-default
  (:require [typed.cljc.checker.fold-rep :refer [add-default-fold-case]]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.filter-rep]
            [typed.cljc.checker.filter-ops :as fops]
            [typed.cljc.checker.object-rep]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.clj.checker.assoc-utils :as assoc-u]
            [typed.cljc.checker.path-rep])
  (:import (typed.cljc.checker.type_rep NotType DifferenceType Intersection Union FnIntersection Bounds
                                        DottedPretype Function RClass JSNominal App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Unchecked TopFunction B F Result
                                        TCResult TCError FlowSet Extends
                                        JSNumber CLJSInteger JSObject JSString ArrayCLJS
                                        JSBoolean AssocType GetType KwArgsSeq KwArgs HSequential HSet
                                        JSUndefined JSNull JSSymbol JSObj TypeOf SymbolicClosure)
           (typed.cljc.checker.filter_rep NoFilter TopFilter BotFilter TypeFilter NotTypeFilter
                                          ImpFilter AndFilter OrFilter FilterSet)
           (typed.cljc.checker.object_rep NoObject EmptyObject Path)
           (typed.cljc.checker.path_rep KeyPE KeysPE ValsPE ClassPE NthPE CountPE KeywordPE)))

(add-default-fold-case NotType
                       (fn [ty]
                         (-> ty
                           (update :type type-rec))))

(add-default-fold-case DifferenceType
                       (fn [ty]
                         (-> ty
                           (update :type type-rec)
                           (update :without #(mapv type-rec %)))))

(add-default-fold-case Intersection
                       (fn [ty]
                         ;(prn "fold-default Intersection" ty)
                         (apply c/In (map type-rec (:types ty)))))

(add-default-fold-case Union 
                       (fn [ty]
                         ;(prn "union default" (typed.clj.checker.parse-unparse/unparse-type ty))
                         (apply c/Un (map type-rec (:types ty)))))

(add-default-fold-case FnIntersection
                       (fn [ty]
                         (-> ty
                           (update :types #(mapv type-rec %)))))

(add-default-fold-case Bounds
                       (fn [ty]
                         (r/visit-bounds ty type-rec)))

(add-default-fold-case DottedPretype
                       (fn [ty]
                         (-> ty
                           (update :pre-type type-rec))))

(add-default-fold-case Function
                       (fn [ty]
                         ;(prn "fold Function" ty)
                         (-> ty
                           (update :dom #(mapv type-rec %))
                           (update :rng type-rec)
                           (update :rest #(when %
                                            (type-rec %)))
                           (update :drest #(when %
                                             (-> %
                                                 (update :pre-type type-rec))))
                           (update :prest #(when %
                                             (let [t (type-rec %)]
                                               ;; if we fully flatten out the prest, we're left
                                               ;; with no prest
                                               (if (= r/-nothing t)
                                                 nil
                                                 t)))))))

(add-default-fold-case JSNominal
                       (fn [ty]
                         (-> ty
                             (update :poly? #(when %
                                               (mapv type-rec %))))))

(add-default-fold-case RClass 
                       (fn [ty]
                         (-> ty
                             (update :poly? #(when %
                                               (mapv type-rec %)))
                             #_(update :replacements #(into {}
                                                            (map (fn [[k v]]
                                                                   [k (type-rec v)]))
                                                            %))
                             #_(update :unchecked-ancestors #(into #{}
                                                                   (map type-rec)
                                                                   %)))))

(add-default-fold-case App
                       (fn [ty]
                         (-> ty
                           (update :rator type-rec)
                           (update :rands #(mapv type-rec %)))))

(add-default-fold-case TApp
                       (fn [ty]
                         (-> ty
                           (update :rator type-rec)
                           (update :rands #(mapv type-rec %)))))

(add-default-fold-case PrimitiveArray
                       (fn [ty]
                         (-> ty
                           (update :input-type type-rec)
                           (update :output-type type-rec))))

(add-default-fold-case DataType
                       (fn [ty]
                         ;(prn "datatype default" (typed.clj.checker.parse-unparse/unparse-type ty))
                         (-> ty
                             (update :poly? #(when %
                                               (mapv type-rec %)))
                             (update :fields (fn [fs]
                                               (apply array-map
                                                      (apply concat
                                                             (for [[k v] fs]
                                                               [k (type-rec v)]))))))))

(add-default-fold-case Protocol
                       (fn [ty]
                         (-> ty
                             (update :poly? #(when %
                                               (mapv type-rec %)))
                             ;FIXME this should probably be left alone in fold
                             ; same in promote/demote
                             (update :methods (fn [ms]
                                                (into {}
                                                      (map (fn [[k v]]
                                                             [k (type-rec v)]))
                                                      ms))))))

(add-default-fold-case TypeFn
                       (fn [ty]
                         (let [names (c/TypeFn-fresh-symbols* ty)
                               body (c/TypeFn-body* names ty)
                               bbnds (c/TypeFn-bbnds* names ty)
                               bmap (zipmap (map r/make-F names) bbnds)]
                           (c/TypeFn* names 
                                      (:variances ty)
                                      (free-ops/with-bounded-frees bmap
                                        (mapv #(r/visit-bounds % type-rec) bbnds))
                                      (free-ops/with-bounded-frees bmap
                                        (type-rec body))))))


(add-default-fold-case Poly
                       (fn [ty]
                         (let [names (c/Poly-fresh-symbols* ty)
                               body (c/Poly-body* names ty)
                               bbnds (c/Poly-bbnds* names ty)
                               bmap (zipmap (map r/make-F names) bbnds)]
                           (c/Poly* names 
                                    (free-ops/with-bounded-frees bmap
                                      (mapv #(r/visit-bounds % type-rec) bbnds))
                                    (free-ops/with-bounded-frees bmap
                                      (type-rec body))
                                    :named (:named ty)))))

(add-default-fold-case PolyDots
                       (fn [ty]
                         (let [names (c/PolyDots-fresh-symbols* ty)
                               body (c/PolyDots-body* names ty)
                               bbnds (c/PolyDots-bbnds* names ty)
                               ; don't scope the dotted bound
                               bmap (zipmap (map r/make-F (rest names)) (rest bbnds))]
                           (c/PolyDots* names 
                                        (free-ops/with-bounded-frees bmap
                                          (mapv #(r/visit-bounds % type-rec) bbnds))
                                        (free-ops/with-bounded-frees bmap
                                          (type-rec body))
                                        :named (:named ty)))))

(add-default-fold-case Mu
                       (fn [ty]
                         (let [name (c/Mu-fresh-symbol* ty)
                               body (c/Mu-body* name ty)]
                           (c/Mu* name (type-rec body)))))

(add-default-fold-case HSequential 
                       (fn [{:keys [types rest drest repeat kind] :as ty}]
                         (r/-hsequential
                           (mapv type-rec (:types ty))
                           :filters (mapv filter-rec (:fs ty))
                           :objects (mapv object-rec (:objects ty))
                           :rest (when rest (type-rec rest))
                           :drest (when drest (update drest :pre-type type-rec))
                           :repeat repeat
                           :kind kind)))

(add-default-fold-case HSet
                       (fn [{:keys [fixed] :as ty}]
                         (r/-hset (set (map type-rec fixed)))))

(defn visit-type-map [m f]
  (into {}
        (map (fn [[k v]]
               [(f k) (f v)]))
        m))

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
                           (update :types #(zipmap (keys %) (map type-rec (vals %)))))))

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
                         (c/-extends
                           (doall (map type-rec extends))
                           :without (doall (mapv type-rec without)))))

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
                         (let [s-target (type-rec target)
                               s-entries (doall
                                           (for [[k v] entries]
                                             [(type-rec k) (type-rec v)]))
                               s-dentries (when dentries (type-rec dentries))
                               fallback-r (-> ty
                                              (assoc :target s-target)
                                              (assoc :entries s-entries)
                                              (assoc :dentries s-dentries))]
                           (if dentries
                             fallback-r
                             (if-let [assoced (apply assoc-u/assoc-pairs-noret s-target s-entries)]
                               assoced
                               fallback-r)))))

(def ret-first identity)

(add-default-fold-case CountRange ret-first)
(add-default-fold-case Name ret-first)
(add-default-fold-case Value ret-first)
(add-default-fold-case Top ret-first)
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
                             (update :o object-rec)
                             (update :flow filter-rec))))

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
                                    (update :o object-rec)
                                    (update :flow filter-rec)))))
    (nth 2)
    clojure.pprint/pprint)
)

(defmacro ret-first-many [& cls]
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
                             (update :path #(seq (doall (map pathelem-rec %)))))))

(add-default-fold-case NotTypeFilter
                       (fn [ty]
                         (-> ty
                             (update :type type-rec)
                             (update :path #(seq (doall (map pathelem-rec %)))))))

(add-default-fold-case ImpFilter
                       (fn [ty]
                         (-> ty
                             (update :a filter-rec)
                             (update :c filter-rec))))

(add-default-fold-case AndFilter
                       (fn [^AndFilter ty]
                         (apply fops/-and
                                (map filter-rec (.fs ty)))))

(add-default-fold-case OrFilter
                       (fn [^OrFilter ty]
                         (apply fops/-or
                                (map filter-rec (.fs ty)))))

(add-default-fold-case FilterSet
                       (fn [^FilterSet ty]
                         (fops/-FS
                           (filter-rec (.then ty))
                           (filter-rec (.else ty)))))

(add-default-fold-case FlowSet
                       (fn [^FlowSet ty]
                         (r/-flow (filter-rec (.normal ty)))))


;objects
(add-default-fold-case EmptyObject ret-first)
(add-default-fold-case Path
                       (fn [ty]
                         (-> ty
                             (update :path #(when %
                                              (mapv pathelem-rec %))))))
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
                             (update :o object-rec)
                             (update :flow filter-rec))))
