(ns typed.clj.runtime.clj-kondo-hooks
  (:require [clj-kondo.hooks-api :as kondo]))

(defn _ann-form
  [{:keys [node]}]
  (let [[body _type] (rest (:children node))]
    {:node body}))
