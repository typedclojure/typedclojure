(ns typed-example.zero-deps.typedclojure-ann
  (:require [typed.clojure :as t]
            #_ ;; this will work for CLJ but not CLJS yet
            [typed-example.zero-deps :as-alias zd]))

(t/defalias MyStr t/Str)
(t/ann typed-example.zero-deps/foo [(t/Vec MyStr) :-> (t/Vec MyStr)])
