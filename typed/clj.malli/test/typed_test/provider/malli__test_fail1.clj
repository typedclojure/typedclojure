(ns typed-test.provider.malli__test-fail1
  (:require [typed.provider.malli :as mp]
            [malli.core :as m]
            [typed.clojure :as t]))

(m/=> foo [:=> [:cat :nil] :int])
(defn foo [t] (inc t))

(comment
  (t/check-ns-clj)
  (m/function-schemas)
  )
