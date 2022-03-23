(ns clojure.core.typed.test.nested-tfn-operator
  (:require [typed.clojure :as t]))

(t/defalias Indirect0
  (t/TFn [[x :variance :covariant]]
     x))

(t/defalias Indirect1
  Indirect0)

(t/ann foo [(Indirect1 Number) -> Number])
(defn foo [x]
  x)

(foo 1)
