(ns clojure.core.typed.test.fail.recur-empty-seq
  (:require [typed.clojure :as t]))

(t/ann recur-args-fail [t/Num * -> t/Any])
(defn recur-args-fail [& args]
  (recur ()))
