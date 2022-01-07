(ns typed.dev)

(defn watch []
  ((requiring-resolve 'kaocha.runner/-main)))
