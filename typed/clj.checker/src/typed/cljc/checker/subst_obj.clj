;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.subst-obj
  (:require [typed.cljc.checker.proposition-rep :as fl]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.fold-rep :as fold]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.free-in :as free-in]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.object-rep :as obj])
  (:import (typed.cljc.checker.type_rep Function)))

(declare subst-type)

;[Proposition (U Number t/Sym) RObject Boolean Opts -> Proposition]
(defn subst-proposition [f k o polarity opts]
  {:pre [(fl/Proposition? f)
         (fl/name-ref? k)
         (obj/RObject? o)
         (boolean? polarity)]
   :post [(fl/Proposition? %)]}
  (letfn [(ap [f] (subst-proposition f k o polarity opts))
          (tf-matcher [t p i k o polarity maker]
            {:pre [(r/Type? t)
                   ((some-fn obj/EmptyObject? obj/NoObject? obj/Path?) o)]
             :post [(fl/Proposition? %)]}
            (cond
              ((some-fn obj/EmptyObject? obj/NoObject?)
               o)
              (cond 
                (= i k) (if polarity fl/-top fl/-bot)
                ;; TODO delete this case - Ambrose
                (free-in/index-free-in? k t opts) (if polarity fl/-top fl/-bot)
                :else f)

              (obj/Path? o) (let [{p* :path i* :id} o]
                              (cond
                                (= i k) (maker 
                                          (subst-type t k o polarity opts)
                                          i*
                                          (concat p p*))
                                ;; TODO delete this case - Ambrose
                                (free-in/index-free-in? k t opts) (if polarity fl/-top fl/-bot)
                                :else f))
              :else (err/int-error (str "what is this? " o) opts)))]
    (cond
      (fl/ImpProposition? f) (let [{ant :a consq :c} f]
                          (fo/-imp (subst-proposition ant k o (not polarity) opts) (ap consq)))
      (fl/AndProposition? f) (let [fs (:fs f)] 
                          (fo/-and (map ap fs) opts))
      (fl/OrProposition? f) (let [fs (:fs f)]
                         (fo/-or (map ap fs) opts))
      (fl/BotProposition? f) f
      ;; preserve -infer-top
      (fl/TopProposition? f) f

      (fl/TypeProposition? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity fo/-proposition))

      (fl/NotTypeProposition? f) 
      (let [{t :type p :path i :id} f]
        (tf-matcher t p i k o polarity fo/-not-proposition))
      (fl/NoProposition? f) f)))

(defn- add-extra-proposition
  "If provided a type t, then add the proposition (is t k).
  Helper function."
  [fl k t opts]
  {:pre [(fl/Proposition? fl)
         (fl/name-ref? k)
         ((some-fn false? nil? r/Type?) t)]
   :post [(fl/Proposition? %)]}
  (let [extra-proposition (if t (fl/TypeProposition-maker t nil k) fl/-top)]
    (letfn [(add-extra-proposition [f]
              {:pre [(fl/Proposition? f)]
               :post [(fl/Proposition? %)]}
              (let [f* (fo/-and [extra-proposition f] opts)]
                (if (fl/BotProposition? f*)
                  f*
                  f)))]
      (add-extra-proposition fl))))

;[PropositionSet Number RObject Boolean (Option Type) -> PropositionSet]
(defn subst-proposition-set [fs k o polarity t opts]
  {:pre [((some-fn fl/PropositionSet? fl/NoProposition?) fs)
         (fl/name-ref? k)
         (obj/RObject? o)
         ((some-fn false? nil? r/Type?) t)]
   :post [(fl/PropositionSet? %)]}
  ;  (prn "subst-proposition-set")
  ;  (prn "fs" (prs/unparse-proposition-set fs opts))
  ;  (prn "k" k) 
  ;  (prn "o" o)
  ;  (prn "polarity" polarity) 
  ;  (prn "t" (when t (prs/unparse-type t opts)))
  (cond
    (fl/PropositionSet? fs) (fo/-FS (subst-proposition (add-extra-proposition (:then fs) k t opts) k o polarity opts)
                                    (subst-proposition (add-extra-proposition (:else fs) k t opts) k o polarity opts))
    :else (fo/-FS fl/-top fl/-top)))

;[RObject NameRef RObject Boolean -> RObject]
(defn subst-object [t k o polarity opts]
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
                  (subst-type t (if (number? k) (+ arg-count k) k) o polarity opts))
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


;[Type (U t/Sym Number) RObject Boolean Opts -> Type]
(defn subst-type [t k o polarity opts]
  {:pre [(r/AnyType? t)
         (fl/name-ref? k)
         (obj/RObject? o)
         (boolean? polarity)]
   :post [(r/AnyType? %)]}
  ;(prn "subst-type" (prs/unparse-type t opts))
  (letfn [(st
            ([t*] (st t* opts))
            ([t* opts]
             (subst-type t* k o polarity opts)))
          (sf
            ([fs] (sf fs opts))
            ([fs opts] 
             {:pre [(fl/PropositionSet? fs)] 
              :post [(fl/PropositionSet? %)]}
             (subst-proposition-set fs k o polarity nil opts)))
          (object-rec 
            ([f] (object-rec f opts))
            ([f opts] (subst-object f k o polarity opts)))]
    (call-subst-type*
      t opts
      {:type-rec st
       :proposition-rec sf
       :object-rec object-rec
       :st st
       :k k
       :o o
       :polarity polarity})))
