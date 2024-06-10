;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.free-in
  (:require [typed.cljc.checker.fold-rep :as fold]
            [typed.cljc.checker.type-rep :as r]
            typed.cljc.checker.object-rep
            typed.cljc.checker.filter-rep)
  (:import (typed.cljc.checker.object_rep Path)
           (typed.cljc.checker.filter_rep NotTypeFilter TypeFilter)
           (typed.cljc.checker.type_rep Function)))

(fold/def-derived-fold IFreeInForObject
  free-in-for-object
  [k free-in?])
(fold/def-derived-fold IFreeInForFilter
  free-in-for-filter
  [k free-in?])
(fold/def-derived-fold IFreeInForType
  free-in-for-type
  [k free-in? for-type])

(fold/add-fold-case
  IFreeInForObject free-in-for-object
  Path
  (fn [{i :id :as o} k free-in?]
    (when (= i k)
      (vreset! free-in? true))
    o))

(fold/add-fold-case
  IFreeInForFilter free-in-for-filter
  NotTypeFilter
  (fn [{i :id :as f} k free-in?]
    (when (= i k)
      (vreset! free-in? true))
    f))

(fold/add-fold-case
  IFreeInForFilter free-in-for-filter
  TypeFilter
  (fn [{i :id :as f} k free-in?]
    (when (= i k)
      (vreset! free-in? true))
    f))

(declare index-free-in?)

(fold/add-fold-case
  IFreeInForType free-in-for-type
  Function
  (fn [{:keys [dom rng rest drest prest pdot kws] :as ty} k free-in? for-type]
    {:pre [(#{:fixed :rest :drest :prest :pdot :kws} (:kind ty))]}
    ;; here we have to increment the count for the domain, where the new bindings are in scope
    (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (:mandatory kws)) (count (:optional kws)))
          st* (fn [t] (index-free-in? (if (number? k) (+ arg-count k) k) t))]
      (doseq [d dom]
        (for-type d))
      (st* rng)
      (some-> rest for-type)
      (some-> prest for-type)
      (some-> (:pre-type drest) for-type)
      (some-> (:pre-type pdot) for-type)
      (doseq [[_ v] (concat (:mandatory kws)
                            (:optional kws))]
        (for-type v))
      ty)))

;[AnyInteger Type -> Boolean]
(defn index-free-in? [k type opts]
  (let [free-in? (volatile! false)]
    (letfn [(for-object
              ([o] (for-object o opts))
              ([o opts]
               (call-free-in-for-object
                 o opts
                 {:type-rec for-type
                  :free-in? free-in?
                  :k k})))
            (for-filter
              ([f] (for-filter f opts))
              ([f opts]
               (call-free-in-for-filter
                 f opts
                 {:type-rec for-type
                  :filter-rec for-filter
                  :free-in? free-in?
                  :k k})))
            (for-type 
              ([t] (for-type t opts))
              ([t opts]
               (call-free-in-for-type
                 t opts
                 {:type-rec for-type
                  :filter-rec for-filter
                  :object-rec for-object
                  :free-in? free-in?
                  :k k
                  :for-type for-type})))]
      (for-type type opts)
      @free-in?)))
