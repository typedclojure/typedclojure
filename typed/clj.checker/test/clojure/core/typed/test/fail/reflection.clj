(ns clojure.core.typed.test.fail.reflection
  (:require [typed.clojure :as t]))

(t/fn [a :- java.io.File]
  (.setReadOnly a 1))

(t/fn [a :- java.io.File]
  (.getName a))

(fn [a]
  (java.io.File. a))

(t/ann write-lines [java.io.Writer (t/Option (t/Coll t/Str)) -> nil])
(defn write-lines [writer lines]
  (doseq [l lines]
    (.write writer "testing")
    (println l)))
