;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.vector
  (:require [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.utils :as u]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fo]))

(defn check-vector [{:keys [items] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:items %))]}
  (let [cargs (mapv check-expr items)
        res-type (r/-hvec (mapv (comp r/ret-t u/expr-type) cargs)
                          :filters (mapv (comp r/ret-f u/expr-type) cargs)
                          :objects (mapv (comp r/ret-o u/expr-type) cargs))]
    (assoc expr
           :items cargs
           u/expr-type (below/maybe-check-below
                         (r/ret res-type (fo/-true-filter))
                         expected
                         opts))))
