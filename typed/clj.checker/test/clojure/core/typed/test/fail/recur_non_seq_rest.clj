(ns clojure.core.typed.test.fail.recur-non-seq-rest
  (:require [typed.clojure :as t]))

(t/ann recur-args-fail [t/Num :* :-> t/Any])
(defn recur-args-fail [& args]
  (recur [1 2 3]))
