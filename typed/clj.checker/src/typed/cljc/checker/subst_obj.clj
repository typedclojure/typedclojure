;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.subst-obj
  (:require [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.fold-rep :as fold]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.free-in :as free-in]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.object-rep :as obj])
  (:import (typed.cljc.checker.type_rep Function)))

(declare subst-type)

;[Filter (U Number t/Sym) RObject Boolean -> Filter]
(defn subst-filter [f k o polarity]
  {:pre [(fl/Filter? f)
         (fl/name-ref? k)
         (obj/RObject? o)
         (boolean? polarity)]
   :post [(fl/Filter? %)]}
  (letfn [(ap [f] (subst-filter f k o polarity))
          (tf-matcher [t p i k o polarity maker]
            {:pre [(r/Type? t)
                   ((some-fn obj/EmptyObject? obj/NoObject? obj/Path?) o)]
             :post [(fl/Filter? %)]}
            (cond
              ((some-fn obj/EmptyObject? obj/NoObject?)
               o)
              (cond 
                (= i k) (if polarity fl/-top fl/-bot)
                ;; TODO delete this case - Ambrose
                (free-in/index-free-in? k t) (if polarity fl/-top fl/-bot)
                :else f)

              (obj/Path? o) (let [{p* :path i* :id} o]
                              (cond
                                (= i k) (maker 
                                          (subst-type t k o polarity)
                                          i*
                                          (concat p p*))
                                ;; TODO delete this case - Ambrose
                                (free-in/index-free-in? k t) (if polarity fl/-top fl/-bot)
                                :else f))
              :else (err/int-error (str "what is this? " o))))]
    (cond
      (fl/ImpFilter? f) (let [{ant :a consq :c} f]
                          (fo/-imp (subst-filter ant k o (not polarity)) (ap consq)))
      (fl/AndFilter? f) (let [fs (:fs f)] 
                          (apply fo/-and (map ap fs)))
      (fl/OrFilter? f) (let [fs (:fs f)]
                         (apply fo/-or (map ap fs)))
      (fl/BotFilter? f) f
      ;; preserve -infer-top
      (fl/TopFilter? f) f

      (fl/TypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity fo/-filter))

      (fl/NotTypeFilter? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity fo/-not-filter))
      (fl/NoFilter? f) f)))

(defn- add-extra-filter
  "If provided a type t, then add the filter (is t k).
  Helper function."
  [fl k t]
  {:pre [(fl/Filter? fl)
         (fl/name-ref? k)
         ((some-fn false? nil? r/Type?) t)]
   :post [(fl/Filter? %)]}
  (let [extra-filter (if t (fl/TypeFilter-maker t nil k) fl/-top)]
    (letfn [(add-extra-filter [f]
              {:pre [(fl/Filter? f)]
               :post [(fl/Filter? %)]}
              (let [f* (fo/-and extra-filter f)]
                (if (fl/BotFilter? f*)
                  f*
                  f)))]
      (add-extra-filter fl))))

;[FilterSet Number RObject Boolean (Option Type) -> FilterSet]
(defn subst-filter-set [fs k o polarity & [t]]
  {:pre [((some-fn fl/FilterSet? fl/NoFilter?) fs)
         (fl/name-ref? k)
         (obj/RObject? o)
         ((some-fn false? nil? r/Type?) t)]
   :post [(fl/FilterSet? %)]}
  ;  (prn "subst-filter-set")
  ;  (prn "fs" (prs/unparse-filter-set fs))
  ;  (prn "k" k) 
  ;  (prn "o" o)
  ;  (prn "polarity" polarity) 
  ;  (prn "t" (when t (prs/unparse-type t)))
  (cond
    (fl/FilterSet? fs) (fo/-FS (subst-filter (add-extra-filter (:then fs) k t) k o polarity)
                               (subst-filter (add-extra-filter (:else fs) k t) k o polarity))
    :else (fo/-FS fl/-top fl/-top)))

;[RObject NameRef RObject Boolean -> RObject]
(defn subst-object [t k o polarity]
  {:pre [(obj/RObject? t)
         (fl/name-ref? k)
         (obj/RObject? o)
         (boolean? polarity)]
   :post [(obj/RObject? %)]}
  (cond
    ((some-fn obj/NoObject? obj/EmptyObject?) t) t
    (obj/Path? t) (let [{p :path i :id} t]
                    (if (= i k)
                      (cond
                        (obj/EmptyObject? o) (obj/EmptyObject-maker)
                        ;; the result is not from an annotation, so it isn't a NoObject
                        (obj/NoObject? o) (obj/EmptyObject-maker)
                        (obj/Path? o) (let [{p* :path i* :id} o]
                                        ;; p* is applied first, then p
                                        (obj/-path (seq (concat p* p)) i*)))
                      t))))

(fold/def-derived-fold ISubstType subst-type* [st k o polarity])

(fold/add-fold-case
  ISubstType subst-type*
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot] :as ty} st k o polarity]
    {:pre [(#{:fixed :rest :drest :kws :prest :pdot} (:kind ty))]}
    ;; here we have to increment the count for the domain, where the new bindings are in scope
    (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (:mandatory kws)) (count (:optional kws)))
          st* (if (integer? k)
                (fn [t] 
                  {:pre [(r/AnyType? t)]}
                  (subst-type t (if (number? k) (+ arg-count k) k) o polarity))
                st)]
      (r/make-Function (mapv st dom)
                       (st* rng)
                       :rest (some-> rest st)
                       :drest (some-> drest (update :pre-type st))
                       :kws (some-> kws
                                    (update :mandatory #(reduce-kv (fn [m k v]
                                                                     (assoc m (st k) (st v)))
                                                                   {} %))
                                    (update :optional #(reduce-kv (fn [m k v]
                                                                     (assoc m (st k) (st v)))
                                                                   {} %)))
                       :prest (some-> prest st)
                       :pdot (some-> pdot (update :pre-type st))))))


;[Type (U t/Sym Number) RObject Boolean -> Type]
(defn subst-type [t k o polarity]
  {:pre [(r/AnyType? t)
         (fl/name-ref? k)
         (obj/RObject? o)
         (boolean? polarity)]
   :post [(r/AnyType? %)]}
  ;(prn "subst-type" (prs/unparse-type t))
  (letfn [(st
            ([t* _info] (st t*))
            ([t*]
             (subst-type t* k o polarity)))
          (sf
            ([fs _info] (sf fs))
            ([fs] 
             {:pre [(fl/FilterSet? fs)] 
              :post [(fl/FilterSet? %)]}
             (subst-filter-set fs k o polarity)))
          (object-rec 
            ([f _info] (object-rec f))
            ([f] (subst-object f k o polarity))) ]
    (call-subst-type*
      t
      {:type-rec st
       :filter-rec sf
       :object-rec object-rec
       :st st
       :k k
       :o o
       :polarity polarity})))
