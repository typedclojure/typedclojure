(ns clojure.core.typed.test.recur-rest-arg
  (:require [typed.clojure :as t]))

(t/ann recur-args [t/Num :* :-> t/Any])
(defn recur-args 
  [& args]
  (recur args))
