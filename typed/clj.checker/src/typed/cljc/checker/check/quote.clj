;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.quote
  (:require [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check.const :as const]))

(defn check-quote [{:keys [expr] :as quote-expr} expected]
  (let [cexpr (const/check-const expr expected true)]
    (assoc quote-expr
           :expr cexpr
           u/expr-type (u/expr-type cexpr))))
