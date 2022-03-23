(ns clojure.core.typed.test.promise
  (:require [typed.clojure :as t]))

(t/ann p (t/Promise t/Num))
(def p (promise))

(fn []
  (p 1)

  (inc @p))
