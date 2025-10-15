;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.proposition-rep
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.clojure :as t]
            [typed.cljc.checker.impl-protocols :as p]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.path-rep :as pr]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  ;; FIXME should this import be a var, since IPathElem is a protocol?
  (:import (typed.cljc.checker.path_rep IPathElem)))

(t/defalias Proposition
  "A proposition"
  p/IProposition)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Propositions

(t/defalias NameRef
  "A name for a type variable, either a symbol or a number."
  (t/U t/Sym Number))

(t/ann ^:no-check name-ref? (t/Pred NameRef))
(def name-ref? (some-fn symbol? (every-pred integer?
                                            (complement neg?))))

(t/ann ^:no-check Proposition? (t/Pred Proposition))
(defn Proposition? [a]
  (p/IProposition? a))

(u/def-proposition BotProposition []
  "Always false proposition"
  []
  :methods
  [p/IProposition])

(u/def-proposition TopProposition []
  "Trivially true proposition"
  []
  :methods
  [p/IProposition])

(t/ann -top Proposition)
(t/ann -bot Proposition)
(def -top (TopProposition-maker))
(def -bot (BotProposition-maker))

(def -infer-top (with-meta -top {:clojure.core.typed/infer true}))

(defn infer-top? [t]
  (and (= -infer-top t)
       (-> t meta :clojure.core.typed/infer boolean)))

(t/ann -top-fn [:-> Proposition])
(defn -top-fn []
  -top)

(u/def-proposition NoProposition []
  "Represents no info about propositions, used for parsing types"
  []
  :methods
  [p/IProposition])

(t/ann -no-proposition Proposition)
(def -no-proposition (NoProposition-maker))

(u/def-proposition TypeProposition [type :- r/Type,
                          path :- (t/Seqable IPathElem)
                          id :- NameRef]
  "A proposition claiming looking up id, down the given path, is of given type"
  [(r/Type? type)
   (pr/path-elems? path)
   (name-ref? id)]
  :methods
  [p/IProposition])

(u/def-proposition NotTypeProposition [type :- r/Type,
                             path :- (t/Seqable IPathElem)
                             id :- NameRef]
  "A proposition claiming looking up id, down the given path, is NOT of given type"
  [(r/Type? type)
   (pr/path-elems? path)
   (name-ref? id)]
  :methods
  [p/IProposition])

; id and path should be merged
(defn equal-paths? [f1 f2]
  {:pre [((some-fn TypeProposition? NotTypeProposition?) f1 f2)]
   :post [(boolean? %)]}
  (and (= (:id f1) (:id f2))
       (= (:path f1) (:path f2))))

(defn proposition-path [f]
  {:pre [((some-fn TypeProposition? NotTypeProposition?) f)]
   :post [(pr/path-elems? %)]}
  (:path f))

(defn proposition-id [f]
  {:pre [((some-fn TypeProposition? NotTypeProposition?) f)]
   :post [(name-ref? %)]}
  (:id f))

(u/def-proposition AndProposition [fs :- (t/Set Proposition)]
  "Logical conjunction of propositions"
  [(set? fs)
   (seq fs)
   (every? Proposition? fs)]
  :methods
  [p/IProposition])

(t/ann ^:no-check make-AndProposition [Proposition :* :-> AndProposition])
(defn make-AndProposition [& fs]
  {:pre [(every? Proposition? fs)]
   :post [(AndProposition? %)]}
  (AndProposition-maker (set fs)))

(u/def-proposition OrProposition [fs :- (t/Set Proposition)]
  "Logical disjunction of propositions"
  [(seq fs)
   (set? fs)
   (every? Proposition? fs)]
  :methods
  [p/IProposition])

(t/ann make-OrProposition [Proposition :* :-> OrProposition])
(defn make-OrProposition [& fs]
  {:pre [(every? Proposition? fs)
         (seq fs)]
   :post [(Proposition? %)]}
  (OrProposition-maker (set fs)))

(u/def-proposition ImpProposition [a :- Proposition
                         c :- Proposition]
  "Antecedent (proposition a) implies consequent (proposition c)"
  [(Proposition? a)
   (Proposition? c)]
  :methods
  [p/IProposition])

(u/def-proposition PropositionSet [then :- Proposition
                              else :- Proposition]
  "A set of propositions: those true when the expression is a true value, and 
  those when it is a false value."
  [(and (or (BotProposition? then)
            (and (BotProposition? else)
               (TopProposition? then))
            (Proposition? then))
        (or (BotProposition? else)
            (and (BotProposition? then)
                 (TopProposition? else))
            (Proposition? else)))]
  :methods
  [p/IProposition
   p/IPropositionSet
   (then-proposition [_] then)
   (else-proposition [_] else)])

(t/defalias PropositionSet
  (t/I Proposition PropositionSet))

(def -infer-FS (PropositionSet-maker -infer-top -infer-top))
