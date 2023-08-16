;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.binding
  (:require [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.utils :as u]))

(defn check-binding
  [check {:keys [init] :as expr} expected]
  (let [cinit (binding [vs/*current-expr* init]
                (check init expected))]
    (assoc expr
           :init cinit
           u/expr-type (u/expr-type cinit))))
