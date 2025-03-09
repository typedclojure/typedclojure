(ns ^:typed.clojure clojure.core.typed.test.fail.record-as-datatype
  (:require [typed.clojure :as t]))

;Should throw a top level type error
(t/ann-datatype IncorrectRec [])
(defrecord IncorrectRec [])
