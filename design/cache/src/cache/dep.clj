(ns cache.dep
  (:require [cache.dep1 :as dep1]
            [clojure.core.typed :as t]
            [typed.clojure :as tc]))

(t/ann foo [:-> t/Any])
(defn foo [] (inc (dep1/a)))

(comment
  (t/check-ns *ns*)
  (tc/check-ns-clj *ns* :check-config {:check-ns-dep :recheck})
  )
