(ns typed-example.spec1-type-providers
  (:require [typed.clojure :as t]
            [clojure.spec.alpha :as s]))

;; this is a normal spec1 annotation
(s/fdef foo :args (s/cat :t int?) :ret int?)
;; Typed Clojure will check foo as [Int :-> Int]
(defn foo [t] (inc t))

;; Typed Clojure will infer foo as [Int :-> Int]
(foo 1)

(comment
  (t/check-ns-clj)
  )
