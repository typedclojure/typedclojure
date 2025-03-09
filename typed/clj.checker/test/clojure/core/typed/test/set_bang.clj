(ns ^:typed.clojure clojure.core.typed.test.set-bang
  (:require [typed.clojure :as t]))

(t/ann foo t/Keyword)
(def ^:dynamic foo :foo)

(t/ann bar [-> t/Keyword])
(defn bar []
  (set! foo :bar))
