;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.ext.clojure.core.typed__fn
  "Type rule for clojure.core.typed/fn"
  (:require [clojure.core.typed.internal :as internal]
            [typed.cljc.checker.check.special.fn :as special-fn]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defn -unanalyzed-special__fn
  [{:keys [env form] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [{fn-anns :ann expand1 :fn :keys [poly]} (internal/parse-fn* form)]
    (special-fn/check-special-fn*
      (assoc expr :form expand1)
      fn-anns
      poly
      expected)))