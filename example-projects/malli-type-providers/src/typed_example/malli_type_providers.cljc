(ns typed-example.malli-type-providers
  (:require [typed.clojure :as-alias t]
            [malli.core :as m]))

;; this is a normal malli annotation
(m/=> foo [:=> [:cat :int] :int])
;; Typed Clojure will check foo as [Int :-> Int]
(defn foo [t] (inc t))

;; Typed Clojure will infer foo as [Int :-> Int]
(foo 1)

(comment
  (t/check-ns-clj)
  )
