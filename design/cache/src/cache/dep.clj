(ns cache.dep
  (:require [cache.dep1 :as dep1]
            [typed.clojure :as t]))

(t/ann foo [:-> t/Any])
(defn foo [] (inc (dep1/a)))

(comment
  (t/check-ns-clj *ns* :check-config {:check-ns-dep :recheck})
  ;Not checking typed.clojure (tagged with :typed.clojure/ignore metadata)
  ;Not checking clojure.core.typed (tagged with :typed.clojure/ignore metadata)
  ;Start checking cache.dep1
  ;Checked cache.dep1 in 49.832031 msecs
  ;Start checking cache.dep
  ;Checked cache.dep in 56.33667 msecs
  :ok
  )
