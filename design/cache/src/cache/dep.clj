(ns cache.dep
  (:require [cache.dep1 :as dep1]
            [clojure.core.typed :as t]
            [typed.clojure :as tc]))

(comment
  (t/check-ns *ns*)
  (tc/check-ns-clj *ns* :check-config {:check-ns-dep :recheck})
  )
