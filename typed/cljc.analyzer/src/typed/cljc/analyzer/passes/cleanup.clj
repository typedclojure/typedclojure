;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.cleanup
(ns typed.cljc.analyzer.passes.cleanup)

(defn cleanup
  {:pass-info {:walk :any :depends #{}}}
  [ast]
  (cond-> (-> ast
              (update :env dissoc :loop-locals-casts)
              (update-in [:env :locals] #(reduce-kv (fn [m k l] (assoc m k (dissoc l :env :init))) {} %)))
    (:atom ast) (assoc (:atom ast) nil)))
