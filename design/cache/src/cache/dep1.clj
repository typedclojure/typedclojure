;; see cache.dep
(ns cache.dep1
  (:require [typed.clojure :as t]
            [clojure.core.typed]))

(t/ann a [:-> t/Int #_t/Bool]) ;; changing this should recheck cache.dep/foo
(defn a [] 1)
