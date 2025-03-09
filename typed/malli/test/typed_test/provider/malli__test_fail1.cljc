(ns ^:typed.clojure typed-test.provider.malli__test-fail1
  (:require [malli.core :as m]
            [typed.clojure :as t]))

(m/=> foo [:=> [:cat :nil] :int])
(defn foo [t] (inc t))

(comment
  (t/check-ns-clj)
  (m/function-schemas)
  )
