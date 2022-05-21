(ns typed-example.zero-deps.typedclojure-ann
  (:require [typed.clojure :as t]
            [typed-example.zero-deps :as-alias zd]))

(t/defalias MyStr t/Str)
(t/ann zd/foo [(t/Vec MyStr) :-> (t/Vec MyStr)])
