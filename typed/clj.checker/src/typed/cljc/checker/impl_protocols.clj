;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc 
  typed.cljc.checker.impl-protocols
  (:refer-clojure :exclude [defrecord defprotocol])
  (:require [typed.cljc.checker.def-utils :as u]
            [typed.clojure :as t]))

;; Implementation Protocols and protocol predicates go here.
;;

;; Type protocols

(t/ann-protocol TCType)
(u/defprotocol TCType)

(t/ann-protocol TCAnyType)
(u/defprotocol TCAnyType)

(t/ann-protocol TCKind)
(u/defprotocol TCKind)

(t/ann-protocol IScope)
(u/defprotocol IScope)

(t/ann ^:no-check IScope? (t/Pred IScope))
(defn IScope? [a]
  (instance? typed.cljc.checker.impl_protocols.IScope a))

(t/ann-protocol IMu
                mu-scope
                [IMu -> IScope])
(u/defprotocol IMu
  (mu-scope [this]))

;; Proposition protocols

(t/ann-protocol IProposition)
(u/defprotocol IProposition)

(t/ann ^:no-check IProposition? (t/Pred IProposition))
(defn IProposition? [a]
  (instance? typed.cljc.checker.impl_protocols.IProposition a))

(t/ann-protocol IPropositionSet
                then-proposition
                [IPropositionSet -> IProposition]
                else-proposition
                [IPropositionSet -> IProposition])
(u/defprotocol IPropositionSet
  (then-proposition [this])
  (else-proposition [this]))

(t/ann ^:no-check IPropositionSet? (t/Pred IPropositionSet))
(defn IPropositionSet? [a]
  (instance? typed.cljc.checker.impl_protocols.IPropositionSet a))

;; Object protocols

(t/ann-protocol IRObject)
(u/defprotocol IRObject)

(t/ann ^:no-check IRObject? (t/Pred IRObject))
(defn IRObject? [a]
  (instance? typed.cljc.checker.impl_protocols.IRObject a))

(t/ann-protocol IResolve
                -resolve
                [TCType t/Any -> TCType])
(u/defprotocol IResolve
  (-resolve [ty opts]))

(t/ann-protocol IRequiresResolving
                -requires-resolving?
                [TCType t/Any -> TCType])
(u/defprotocol IRequiresResolving
  (-requires-resolving? [ty opts]))
