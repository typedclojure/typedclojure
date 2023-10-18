;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc 
  typed.clj.checker.rclass-ancestor-env
  (:require [typed.clojure :as t]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.type-ctors :as c])
  (:import [typed.cljc.checker.type_rep RClass]))

(t/ann ^:no-check rclass-ancestors [RClass -> (t/SortedSet r/Type)])
(defn rclass-ancestors [{:keys [unchecked-ancestors] poly :poly? :as rcls}]
  {:pre [(r/RClass? rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  (let [names (repeatedly (count poly) #(gensym "unchecked-ancestor"))
        fs (map r/make-F names)]
    (r/sorted-type-set
      (for [u unchecked-ancestors]
        (let [t (c/instantiate-many names u)
              subst (c/make-simple-substitution names poly)]
          (subst/subst-all subst t))))))

(t/ann ^:no-check rclass-replacements [RClass -> (t/Seqable t/Symbol r/Type)])
(defn rclass-replacements [{:keys [replacements] poly :poly? :as rcls}]
  {:pre [(r/RClass? rcls)]
   :post [((con/hash-c? symbol? r/Type?) %)]}
  (update-vals replacements #(c/inst-and-subst % poly)))

(t/ann ^:no-check abstract-rclass-ancestors [RClass (t/Seqable r/Type) -> nil])
(defn abstract-rclass-ancestors [rsym names as]
  {:pre [(symbol? rsym)]}
  (r/sorted-type-set
    (for [u as]
      (c/abstract-many names u))))

(t/ann ^:no-check abstract-rclass-replacements [RClass (t/Map t/Symbol r/Type) -> nil])
(defn abstract-rclass-replacements [rsym names as]
  {:pre [(symbol? rsym)]}
  (update-vals as #(c/abstract-many names %)))
