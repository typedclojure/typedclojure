(ns ^:typed.clojure cljs.core.typed.test.identity
  (:require [cljs.core.typed :as t]))

(t/ann my-identity (t/All [x] [x -> (t/U x t/Num)]))
(defn my-identity [x]
  (if (number? x)
    (inc x)
    x))
