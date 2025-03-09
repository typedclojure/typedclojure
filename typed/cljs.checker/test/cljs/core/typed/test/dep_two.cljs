(ns ^:typed.clojure cljs.core.typed.test.dep-two
  (:require [cljs.core.typed :as t]
            [cljs.core.typed.test.dep-one :as one]))

(def b (inc one/a))
