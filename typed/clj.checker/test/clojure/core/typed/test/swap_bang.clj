(ns clojure.core.typed.test.swap-bang
  (:require [typed.clojure :as t]))

(t/ann foo (t/Atom (t/Map ':a t/Num)))
(def foo (atom {:a 1}))

(fn []
  (swap! foo (t/inst assoc (t/Map ':a t/Num) ':a t/Num) :a 3))

;
;(swap! foo (fn [a] a))
;(swap! foo (fn [a] a))
;(swap! foo (fn [a b] a) 2)
;(swap! foo (fn [a b] a) 2)
