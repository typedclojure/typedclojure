;; see cache.dep
(ns cache.dep1
  (:require [typed.clojure :as t]
            [clojure.core.typed]))

(t/ann a [t/Int :-> t/Bool])
(defn a [] nil)
