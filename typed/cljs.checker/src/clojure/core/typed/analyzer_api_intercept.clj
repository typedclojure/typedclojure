;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure clojure.core.typed.analyzer-api-intercept
  (:require [cljs.analyzer.api :as api]
            [clojure.core.typed.ast-utils :as ast-u]))

(def ops-found (atom #{}))

(defn reset-found []
  (reset! ops-found #{}))

(defn walk-collect-ops [{:keys [op children] :as node}]
  (when-not (some (partial = op) @ops-found)
    (swap! ops-found #(conj % op)))
  (when children
    (doseq [c children]
      (walk-collect-ops (c node)))))

(defn analyze
  ([env form] (analyze env form nil {}))
  ([env form x opts]
   (let [result (api/analyze env form x opts)]
     (walk-collect-ops result)
     ;(println ops-found)
     result)))

(defn empty-env []
  (api/empty-env))
