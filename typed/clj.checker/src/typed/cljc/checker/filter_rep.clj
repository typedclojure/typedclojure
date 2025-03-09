;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.filter-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.clojure :as t]
            [typed.cljc.checker.impl-protocols :as p]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  ;; FIXME should this import be a var, since IPathElem is a protocol?
  (:import (typed.cljc.checker.path_rep IPathElem)))

(t/defalias Filter
  "A filter"
  p/IFilter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filters

(t/defalias NameRef
  "A name for a type variable, either a symbol or a number."
  (t/U t/Sym Number))

(t/ann ^:no-check name-ref? (t/Pred NameRef))
(def name-ref? (some-fn symbol? (every-pred integer?
                                            (complement neg?))))

(t/ann ^:no-check Filter? (t/Pred Filter))
(defn Filter? [a]
  (p/IFilter? a))

(u/def-filter BotFilter []
  "Always false proposition"
  []
  :methods
  [p/IFilter])

(u/def-filter TopFilter []
  "Trivially true proposition"
  []
  :methods
  [p/IFilter])

(t/ann -top Filter)
(t/ann -bot Filter)
(def -top (TopFilter-maker))
(def -bot (BotFilter-maker))

(def -infer-top (with-meta -top {:clojure.core.typed/infer true}))

(defn infer-top? [t]
  (and (= -infer-top t)
       (-> t meta :clojure.core.typed/infer boolean)))

(t/ann -top-fn [:-> Filter])
(defn -top-fn []
  -top)

(u/def-filter NoFilter []
  "Represents no info about filters, used for parsing types"
  []
  :methods
  [p/IFilter])

(t/ann -no-filter Filter)
(def -no-filter (NoFilter-maker))

(u/def-filter TypeFilter [type :- r/Type,
                          path :- (t/Seqable IPathElem)
                          id :- NameRef]
  "A filter claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (pr/path-elems? path)
   (name-ref? id)]
  :methods
  [p/IFilter])

(u/def-filter NotTypeFilter [type :- r/Type,
                             path :- (t/Seqable IPathElem)
                             id :- NameRef]
  "A filter claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (pr/path-elems? path)
   (name-ref? id)]
  :methods
  [p/IFilter])

; id and path should be merged
(defn equal-paths? [f1 f2]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) f1 f2)]
   :post [(boolean? %)]}
  (and (= (:id f1) (:id f2))
       (= (:path f1) (:path f2))))

(defn filter-path [f]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) f)]
   :post [(pr/path-elems? %)]}
  (:path f))

(defn filter-id [f]
  {:pre [((some-fn TypeFilter? NotTypeFilter?) f)]
   :post [(name-ref? %)]}
  (:id f))

(u/def-filter AndFilter [fs :- (t/Set Filter)]
  "Logical conjunction of filters"
  [(set? fs)
   (seq fs)
   (every? Filter? fs)]
  :methods
  [p/IFilter])

(t/ann ^:no-check make-AndFilter [Filter :* :-> AndFilter])
(defn make-AndFilter [& fs]
  {:pre [(every? Filter? fs)]
   :post [(AndFilter? %)]}
  (AndFilter-maker (set fs)))

(u/def-filter OrFilter [fs :- (t/Set Filter)]
  "Logical disjunction of filters"
  [(seq fs)
   (set? fs)
   (every? Filter? fs)]
  :methods
  [p/IFilter])

(t/ann make-OrFilter [Filter :* :-> OrFilter])
(defn make-OrFilter [& fs]
  {:pre [(every? Filter? fs)
         (seq fs)]
   :post [(Filter? %)]}
  (OrFilter-maker (set fs)))

(u/def-filter ImpFilter [a :- Filter
                         c :- Filter]
  "Antecedent (filter a) implies consequent (filter c)"
  [(Filter? a)
   (Filter? c)]
  :methods
  [p/IFilter])

(u/def-filter FilterSet [then :- Filter
                         else :- Filter]
  "A set of filters: those true when the expression is a true value, and 
  those when it is a false value."
  [(and (or (BotFilter? then)
            (and (BotFilter? else)
               (TopFilter? then))
            (Filter? then))
        (or (BotFilter? else)
            (and (BotFilter? then)
                 (TopFilter? else))
            (Filter? else)))]
  :methods
  [p/IFilter
   p/IFilterSet
   (then-filter [_] then)
   (else-filter [_] else)])

(t/defalias FilterSet
  (t/I Filter FilterSet))

(def -infer-FS (FilterSet-maker -infer-top -infer-top))
