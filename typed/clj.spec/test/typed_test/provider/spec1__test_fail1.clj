(ns typed-test.provider.spec1__test-fail1
  (:require [clojure.spec.alpha :as s]
            [typed.clojure :as t]))

(s/fdef foo :args (s/cat :t nil?) :ret int?)
(defn foo [t] (inc t))

(comment
  (t/check-ns-clj)
  )
