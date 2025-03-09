(ns ^:typed.clojure clojure.core.typed.test.pred-static
  (:require [clojure.core.typed :as t]))

(let [a (t/ann-form {:a 1} t/Any)
      _ (assert ((t/pred '{:a t/Num}) a))]
  (+ 1 (:a a)))
