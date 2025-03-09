(ns ^:typed.clojure clojure.core.typed.test.fail.datatype-as-record
  (:require [typed.clojure :as t]))

;Should throw a top level type error
(t/ann-record IncorrectDt [])
(deftype IncorrectDt [])
