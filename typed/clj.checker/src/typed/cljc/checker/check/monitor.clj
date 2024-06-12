;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.monitor
  (:require [typed.cljc.checker.utils :as u]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.check-below :as below]))

(defn check-monitor
  "monitor-enter and monitor-exit both take any object and return nil"
  [{:keys [target] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:pre [((some-fn nil? r/TCResult?) expected)]}
  (assoc expr
         :target (check-expr target (r/ret (c/RClass-of Object opts)) opts)
         u/expr-type (below/maybe-check-below (r/ret r/-nil (fo/-false-filter))
                                              expected
                                              opts)))
