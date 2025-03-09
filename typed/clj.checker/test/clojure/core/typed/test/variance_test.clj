(ns ^:typed.clojure clojure.core.typed.test.variance-test
  (:require [typed.clojure :as t]))


(t/ann-datatype [[w :variance :contravariant]
                 [r :variance :covariant]]
                FooT
                [])

(deftype FooT [])
