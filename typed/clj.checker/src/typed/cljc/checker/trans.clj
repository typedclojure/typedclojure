;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.trans
  (:require [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.fold-rep :as fold]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.object-rep :as or])
  (:import (typed.cljc.checker.type_rep HSequential Function AssocType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dotted pre-type expansion

(fold/def-derived-fold ITransDots trans-dots* [b bm])

;tdr from Practical Variable-Arity Polymorphism paper
; Expand out dotted pretypes to fixed domain, using types bm, if (:name bound) = b
(defn trans-dots [t b bm opts]
  (letfn [(tr
            ([ty] (tr ty opts))
            ([ty opts] (trans-dots ty b bm opts)))]
    (call-trans-dots*
      t opts
      {:type-rec tr
       :b b
       :bm bm})))

(fold/add-fold-case
  ITransDots trans-dots*
  HSequential
  (fn [{:keys [kind] :as t} b bm]
    (let [tfn #(trans-dots % b bm opts)]
      (cond
        (:drest t)
        (let [{:keys [pre-type name]} (:drest t)]
          (assert (symbol? name))
          (if (= b name) ;identical bounds
            (let [fixed (vec
                          (concat 
                            ;keep fixed entries
                            (doall (map tfn (:types t)))
                            ;expand dotted type to fixed entries
                            (doall (map (fn [bk]
                                          {:post [(r/Type? %)]}
                                          ;replace free occurences of bound with bk
                                          (-> (subst/substitute bk b pre-type opts)
                                              tfn))
                                        bm))))
                  extra-fixed (- (count fixed)
                                 (count (:types t)))]
              (r/-hsequential fixed
                              :filters (vec
                                         (concat (map tfn (:fs t))
                                                 (repeat extra-fixed
                                                         (fo/-simple-filter))))
                              :objects (vec
                                         (concat (map tfn (:objects t))
                                                 (repeat extra-fixed
                                                         or/-empty)))
                              ;drest is expanded into fixed
                              :kind kind))
            (r/-hsequential (mapv tfn (:types t))
                            :filters (mapv tfn (:fs t))
                            :objects (mapv tfn (:objects t))
                            :drest (some-> (:drest t)
                                           (update :pre-type tfn)) ;translate pre-type
                            :kind kind)))
        :else
        (r/-hsequential (mapv tfn (:types t))
                        :filters (mapv tfn (:fs t))
                        :objects (mapv tfn (:objects t))
                        :rest (some-> (:rest t) tfn)
                        :repeat (:repeat t)
                        :kind kind)))))

(fold/add-fold-case
  ITransDots trans-dots*
  AssocType
  (fn [{:keys [target entries dentries]} b bm]
    (let [tfn #(trans-dots % b bm opts)
          t-target (tfn target)
          t-entries (map (fn [ent]
                           [(tfn (first ent)) (tfn (second ent))])
                         entries)]
      (if (and dentries
               (= b (:name dentries)))
        (r/AssocType-maker t-target
                           (concat t-entries
                                   (->> bm
                                     (map (fn [bk]
                                            {:post [(r/Type? %)]}
                                            (-> (subst/substitute bk b (:pre-type dentries) opts)
                                              tfn)))
                                     (partition 2)
                                     (map vec)))
                           nil)
        (r/AssocType-maker t-target
                           t-entries
                           (some-> dentries
                                   (update :pre-type tfn)))))))

(fold/add-fold-case
  ITransDots trans-dots*
  Function
  (fn [t b bm]
    {:pre [(#{:fixed :rest :drest :prest} (:kind t))]}
    (let [tfn #(trans-dots % b bm opts)]
      (cond
        (:drest t)
        (let [{:keys [pre-type name]} (:drest t)]
          (assert (symbol? name))
          (if (= b name) ;identical bounds
            (let [dom (into 
                        ;keep fixed domain
                        (mapv tfn (:dom t))
                        ;expand dotted type to fixed domain
                        (map (fn [bk]
                               {:post [(r/Type? %)]}
                               ;replace free occurences of bound with bk
                               (-> (subst/substitute bk b pre-type opts)
                                   tfn)))
                        bm)]
              ;dotted pretype now expanded to fixed domain
              (r/make-Function dom (tfn (:rng t))))
            (-> t
                (update :dom #(doall (map tfn %)))
                (update :rng tfn)
                (update :drest (fn [drest]
                                 (some-> drest
                                         (update :pre-type tfn))))))) ;translate pre-type
        :else
        (-> t
            (update :dom #(doall (map tfn %)))
            (update :rng tfn)
            (update :rest #(some-> % tfn))
            (update :prest #(some-> % tfn)))))))
