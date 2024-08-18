;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc 
  typed.clj.checker.rclass-ancestor-env
  (:refer-clojure :exclude [repeatedly])
  (:require [typed.clojure :as t]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.runtime.perf-utils :refer [vrepeatedly]]
            [typed.cljc.checker.free-ops :as free-ops])
  (:import [typed.cljc.checker.type_rep RClass]))

(t/ann ^:no-check rclass-ancestors [RClass t/Any -> (t/SortedSet r/Type)])
(defn rclass-ancestors [{:keys [unchecked-ancestors] poly :poly? :as rcls} opts]
  {:pre [(r/RClass? rcls)]
   :post [((con/sorted-set-c? r/Type?) %)]}
  (let [n (count poly)
        names (vrepeatedly n #(gensym "unchecked-ancestor"))
        ;; assumes unchecked-ancestors are Type's
        opts (free-ops/with-bounded-frees opts names (repeat n r/no-bounds))]
    (r/sorted-type-set
      (for [u unchecked-ancestors]
        (let [t (c/instantiate-many names u opts)
              subst (c/make-simple-substitution names poly)]
          (subst/subst-all subst t opts))))))

(t/ann ^:no-check rclass-replacements [RClass t/Any -> (t/Seqable t/Symbol r/Type)])
(defn rclass-replacements [{:keys [replacements] poly :poly? :as rcls} opts]
  {:pre [(r/RClass? rcls)]
   :post [((con/hash-c? symbol? r/Type?) %)]}
  (update-vals replacements #(c/inst-and-subst % poly opts)))

(t/ann ^:no-check abstract-rclass-ancestors [RClass (t/Seqable t/Symbol) (t/Seqable r/Kind) (t/Seqable r/Type) t/Any -> nil])
(defn abstract-rclass-ancestors [rsym names bnds as opts]
  {:pre [(symbol? rsym)]}
  (let [opts (free-ops/with-bounded-frees opts names bnds)]
    (r/sorted-type-set
      (for [u as]
        (c/abstract-many names u opts)))))

(t/ann ^:no-check abstract-rclass-replacements [RClass (t/Seqable t/Symbol) (t/Seqable r/Kind) (t/Map t/Symbol r/Type) t/Any -> nil])
(defn abstract-rclass-replacements [rsym names bnds as opts]
  {:pre [(symbol? rsym)]}
  (let [opts (free-ops/with-bounded-frees opts names bnds)]
    (update-vals as #(c/abstract-many names % opts))))
