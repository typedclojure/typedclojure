(ns typed-test.provider.malli__meta-test1
  (:require [malli.core :as m]
            [typed.clojure.malli :as-alias tm]
            [typed.clojure :as t]))

(m/=> foo1 [:=> [:cat [:vector :int]] [:vector :int]])
(defn foo1 [c]
  (mapv (fn [^{::tm/- :int} i] i)
        c))

(comment
  (t/check-ns-clj)
  (t/check-ns-cljs)
  (m/function-schemas)
  )
