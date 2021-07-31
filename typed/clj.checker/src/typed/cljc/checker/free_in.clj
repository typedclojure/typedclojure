;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
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
  (fn [{p :path i :id :as o} k free-in?]
    (when (= i k)
      (reset! free-in? true))
    o))

(fold/add-fold-case
  IFreeInForFilter free-in-for-filter
  NotTypeFilter
  (fn [{t :type p :path i :id :as f} k free-in?]
    (when (= i k)
      (reset! free-in? true))
    f))

(fold/add-fold-case
  IFreeInForFilter free-in-for-filter
  TypeFilter
  (fn [{t :type p :path i :id :as f} k free-in?]
    (when (= i k)
      (reset! free-in? true))
    f))

(declare index-free-in?)

(fold/add-fold-case
  IFreeInForType free-in-for-type
  Function
  (fn [{:keys [dom rng rest drest kws]} k free-in? for-type]
    ;; here we have to increment the count for the domain, where the new bindings are in scope
    (let [arg-count (+ (count dom) (if rest 1 0) (if drest 1 0) (count (concat (:mandatory kws)
                                                                               (:optional kws))))
          st* (fn [t] (index-free-in? (if (number? k) (+ arg-count k) k) t))]
      (doseq [d dom]
        (for-type d))
      (st* rng)
      (and rest (for-type rest))
      (and rest (for-type (:pre-type drest)))
      (doseq [[_ v] (concat (:mandatory kws)
                            (:optional kws))]
        (for-type v))
      ;dummy return value
      (r/make-Function [] r/-any))))

;[AnyInteger Type -> Boolean]
(defn index-free-in? [k type]
  (let [free-in? (atom false :validator boolean?)]
    (letfn [(for-object [o]
              (call-free-in-for-object
                o
                {:type-rec for-type
                 :free-in? free-in?
                 :k k}))
            (for-filter [o]
              (call-free-in-for-filter
                o
                {:type-rec for-type
                 :filter-rec for-filter
                 :free-in? free-in?
                 :k k}))
            (for-type [t]
              (call-free-in-for-type
                t
                {:type-rec for-type
                 :filter-rec for-filter
                 :object-rec for-object
                 :free-in? free-in?
                 :k k
                 :for-type for-type}))]
      (for-type type)
      @free-in?)))
