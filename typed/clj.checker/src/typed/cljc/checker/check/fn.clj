;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.fn
  (:require [clojure.core.typed :as t]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.check.fn-methods :as fn-methods]
            [typed.cljc.checker.check.utils :as cu]))

;[FnExpr (Option Type) -> Expr]
(defn check-fn 
  "Check a fn to be under expected and annotate the inferred type"
  [{:keys [methods] :as fexpr} expected]
  {:pre [(r/TCResult? expected)
         (= :fn (:op fexpr))]
   :post [(-> % u/expr-type r/TCResult?)
          (vector? (::t/cmethods %))]}
  ;(prn "check-fn" methods)
  (let [{:keys [ifn methods cmethods]}
        (fn-methods/check-fn-methods 
          methods
          (r/ret-t expected)
          :self-name (cu/fn-self-name fexpr))]
    (assoc fexpr
           :methods methods
           ::t/cmethods cmethods
           u/expr-type (r/ret ifn (fo/-true-filter)))))
