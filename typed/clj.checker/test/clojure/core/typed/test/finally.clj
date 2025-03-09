(ns ^:typed.clojure clojure.core.typed.test.finally
  (:require [typed.clojure :as t]))

(t/ann f [-> t/Str])
(defn f []
  (try
    "a"
    (finally 1)))
