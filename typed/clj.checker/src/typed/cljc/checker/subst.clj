;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.subst
  (:require [typed.clojure :as t]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.fold-rep :as f]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.object-rep :as orep]
            [typed.clj.checker.assoc-utils :as assoc-u])
  (:import (typed.cljc.checker.type_rep F Function HSequential AssocType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variable substitution

(f/def-derived-fold ISubstitute substitute* [images names name->pos recur?])

(declare substitute-many)

(f/add-fold-case
  ISubstitute substitute*
  F
  (fn [f images names name->pos recur?]
    (if-some [i (name->pos (:name f))]
      (cond-> (nth images i)
        recur? (substitute-many (subvec images i) (subvec names i) opts))
      f)))

(t/ann ^:no-check substitute-many [r/Type (t/Seqable r/Type) (t/Seqable t/Sym) t/Any -> r/Type])
(defn substitute-many [target images names opts]
  (let [images (vec images)
        names (vec names)]
    (assert (= (count images) (count names)) [names images])
    (cond-> target
      (seq images) (call-substitute* opts {:recur? true
                                           :images images
                                           :names names
                                           :name->pos (zipmap names (range))}))))

(t/ann ^:no-check substitute [r/Type t/Sym r/Type t/Any -> r/Type])
(defn substitute [image name target opts]
  {:pre [(r/AnyType? image)
         (symbol? name)
         (r/AnyType? target)]
   :post [(r/AnyType? %)]}
  (substitute-many target [image] [name] opts))

(declare substitute-dots substitute-dotted)

(t/ann ^:no-check subst-all [crep/SubstMap r/Type t/Any -> r/Type])
(defn subst-all [s t opts]
  {:pre [(crep/substitution-c? s)
         (r/AnyType? t)]
   :post [(r/AnyType? %)]}
  (reduce-kv (fn [t v r]
               (cond
                 (crep/t-subst? r) (substitute (:type r) v t opts)
                 (crep/i-subst? r) (substitute-dots (:types r) nil v t opts)
                 (crep/i-subst-starred? r) (substitute-dots (:types r) (:starred r) v t opts)
                 (and (crep/i-subst-dotted? r)
                      (empty? (:types r))) (substitute-dotted (:dty r) (:name (:dbound r)) v t opts)
                 (crep/i-subst-dotted? r) (err/nyi-error "i-subst-dotted nyi" opts)
                 :else (err/nyi-error (str "Other substitutions NYI") opts)))
             t s))

;; Substitute dots

(f/def-derived-fold ISubstituteDots substitute-dots* [name sb images rimage])

(f/add-fold-case
  ISubstituteDots substitute-dots*
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot] :as ftype} name sb images rimage]
    {:pre [(symbol? name)
           (#{:fixed :rest :drest :kws :prest :pdot} (:kind ftype))]}
    (if (= name (:name (or drest pdot)))
      (r/make-Function (let [sb-dom (mapv sb dom)]
                         (if drest
                           ;; We need to recur first, just to expand out any dotted usages of this.
                           (let [expanded (sb (:pre-type drest))]
                             ;(prn "expanded" (unparse-type expanded opts))
                             (into sb-dom (map (fn [img] (substitute img name expanded opts)))
                                   images))
                           (let [expandeds (mapv sb (-> pdot :pre-type :types))
                                 _ (assert (zero? (rem (count images) (count expandeds))))]
                             (into sb-dom (comp (partition-all (count expandeds))
                                                (mapcat (fn [images]
                                                          (map (fn [expanded img]
                                                                 (substitute img name expanded opts))
                                                               expandeds
                                                               images))))
                                   images))))
                       (sb rng)
                       :rest rimage)
      (r/make-Function (mapv sb dom)
                       (sb rng)
                       :rest (some-> rest sb)
                       :drest (some-> drest (update :pre-type sb))
                       :kws (when kws (err/nyi-error "substitute keyword args" opts))
                       :prest (some-> prest sb)
                       :pdot (some-> pdot (update :pre-type sb))))))

(f/add-fold-case
  ISubstituteDots substitute-dots*
  AssocType
  (fn [{:keys [target entries dentries] :as atype} name sb images rimage]
    (let [sb-target (sb target)
          sb-entries (mapv (fn [ent]
                             [(sb (first ent)) (sb (second ent))])
                           entries)]
      (if (and dentries
               (= name (:name dentries)))
        (let [entries (into sb-entries
                            (let [expanded (sb (:pre-type dentries))]
                              (comp (map (fn [img] (substitute img name expanded opts)))
                                    (partition-all 2)
                                    (map vec)))
                            images)]
          ; try not to use AssocType, because subtype and cs-gen support for it
          ; is not that mature
          (or (assoc-u/assoc-pairs-noret sb-target entries opts)
              (r/AssocType-maker sb-target entries nil)))
        (r/AssocType-maker sb-target
                           sb-entries
                           (some-> dentries (update :pre-type sb)))))))

(f/add-fold-case
  ISubstituteDots substitute-dots*
  HSequential
  (fn [{:keys [types fs objects rest drest kind] :as ftype} name sb images rimage]
    (if (and drest
             (= name (:name drest)))
      (r/-hsequential
        ;; We need to recur first, just to expand out any dotted usages of this.
        (let [expanded (sb (:pre-type drest))]
          (into (mapv sb types)
                (map (fn [img] (substitute img name expanded opts)))
                images))
        :filters (into (mapv sb fs) (repeat (count images) (fo/-FS fl/-top fl/-top)))
        :objects (into (mapv sb objects) (repeat (count images) orep/-empty))
        :kind kind)
      (r/-hsequential
        (mapv sb types)
        :filters (mapv sb fs)
        :objects (mapv sb objects)
        :rest (some-> rest sb)
        :drest (some-> drest (update :pre-type drest))
        :repeat (:repeat ftype)
        :kind kind))))

;; implements angle bracket substitution from the formalism
;; substitute-dots : Listof[Type] Option[type] Name Type -> Type
(t/ann ^:no-check substitute-dots [(t/Seqable r/Type) (t/U nil r/Type) t/Sym r/Type t/Any -> r/Type])
(defn substitute-dots [images rimage name target opts]
  {:pre [(every? r/AnyType? images)
         ((some-fn nil? r/AnyType?) rimage)
         (symbol? name)
         (satisfies? ISubstituteDots target)]}
  ;(prn "substitute-dots" (unparse-type target opts) name "->" (map #(unparse-type % opts) images))
  (letfn [(sb
            ([t] (sb t opts))
            ([t opts] (substitute-dots images rimage name t opts)))]
    (cond-> target
      (or ((frees/fi target opts) name)
          ((frees/fv target opts) name))
      (call-substitute-dots* opts
        {:type-rec sb
         :filter-rec (f/sub-f sb `call-substitute-dots* opts)
         :name name
         :sb sb
         :images images
         :rimage rimage}))))


(f/def-derived-fold ISubstituteDotted substitute-dotted* [sb name image])
(f/add-fold-case
  ISubstituteDotted substitute-dotted*
  F
  (fn [{name* :name :as t} sb name image]
   (if (= name* name)
     image
     t)))

(f/add-fold-case
  ISubstituteDotted substitute-dotted*
  Function
  (fn [{:keys [dom rng rest drest kws prest pdot kind]} sb name image]
    {:pre [(#{:fixed :rest :drest :kws :prest :pdot} kind)]}
    (r/make-Function (mapv sb dom)
                     (sb rng)
                     :rest (some-> rest sb)
                     :drest (when drest
                              (r/DottedPretype1-maker (substitute image (:name drest) (sb (:pretype drest)) opts)
                                                      (if (= name (:name drest))
                                                        name
                                                        (:name drest))))
                     :kws (when kws (err/nyi-error "substitute-dotted with kw arguments" opts))
                     :prest (some-> prest sb)
                     :pdot (when pdot (err/nyi-error "NYI pdot of substitute-dotted for Function" opts)))))

(f/add-fold-case
  ISubstituteDotted substitute-dotted*
  AssocType
  (fn [{:keys [target entries dentries]} sb name image]
   (r/AssocType-maker (sb target)
                      (reduce-kv (fn [entries k v]
                                   (assoc entries (sb k) (sb v)))
                                 {} entries)
                      (when dentries
                        (r/DottedPretype1-maker (substitute image (:name dentries) (sb (:pretype dentries)) opts)
                                                (if (= name (:name dentries))
                                                  name
                                                  (:name dentries)))))))

(f/add-fold-case
  ISubstituteDotted substitute-dotted*
  HSequential
  (fn [{:keys [types fs objects rest drest kind] :as ftype} sb name image]
    (r/-hsequential
      (mapv sb types)
      :filters (mapv sb fs))
      :objects (mapv sb objects)
      :rest (some-> rest sb)
      :drest (when drest
               (r/DottedPretype1-maker (substitute image (:name drest) (sb (:pretype drest)) opts)
                                       (if (= name (:name drest))
                                         name
                                         (:name drest))))
      :repeat (:repeat ftype)
      :kind kind))

;; implements curly brace substitution from the formalism
;; substitute-dotted : Type Name Name Type -> Type
(t/ann ^:no-check substitute-dotted [r/Type t/Sym t/Sym r/Type t/Any -> r/Type])
(defn substitute-dotted [image image-bound name target opts]
  {:pre [(r/AnyType? image)
         (symbol? image-bound)
         (symbol? name)
         (r/AnyType? target)]
   :post [(r/AnyType? %)]}
  (letfn [(sb
            ([t] (sb t opts))
            ([t opts] (substitute-dotted image image-bound name t opts)))]
    (cond-> target
      ((frees/fi target opts) name)
      (call-substitute-dotted* opts
        {:type-rec sb 
         :filter-rec (f/sub-f sb `call-substitute-dotted* opts)
         :name name
         :sb sb
         :image image}))))
