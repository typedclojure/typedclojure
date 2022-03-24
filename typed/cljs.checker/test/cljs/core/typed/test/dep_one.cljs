(ns cljs.core.typed.test.dep-one
  (:require [typed.clojure :as t]))

(t/ann a t/Int)
(def a 1)
