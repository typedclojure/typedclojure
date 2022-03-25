(ns typed-test.provider.malli__test-fail-just-in-cljs1
  (:require [malli.core :as m]
            [typed.clojure :as t]))

(m/=> foo [:=> [:cat #?(:clj :int :cljs :nil)] :int])
(defn foo [t] (inc t))

(comment
  (t/check-ns-clj)
  (t/check-ns-cljs)
  (m/function-schemas)
  )
