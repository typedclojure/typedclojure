;CTYP-79
(ns ^:typed.clojure clojure.core.typed.test.hmap-resolve-assoc
  (:require [typed.clojure :as t]))

(t/defalias TA (t/HMap :optional {:d t/Any}))

(t/ann a [TA -> TA])
(defn a [m] (assoc m :d "foo"))
