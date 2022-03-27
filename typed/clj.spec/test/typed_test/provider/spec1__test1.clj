(ns typed-test.provider.spec1__test1
  (:require [clojure.spec.alpha :as s]
            [typed.clojure :as t]))

(s/fdef foo :args (s/cat :t int?) :ret int?)
(defn foo [t] (inc t))

(s/def ::external int?)
(s/fdef external :args (s/cat :t ::external) :ret int?)
(defn external [t] (inc t))

;; t/ann overrides m/=>
(s/fdef choose-tc :args (s/cat :t nil?) :ret int?)
(t/ann choose-tc [t/AnyInteger :-> t/AnyInteger])
(defn choose-tc [t] (inc t))

(comment
  (t/check-ns-clj)
  (t/check-ns-cljs)
  (s/registry)
  )
