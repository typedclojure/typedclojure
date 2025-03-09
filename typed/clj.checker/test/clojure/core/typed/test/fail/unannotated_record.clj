(ns ^:typed.clojure clojure.core.typed.test.fail.unannotated-record
  (:require [typed.clojure :as t]))

;Should throw a top level type error
(defrecord Unannotated [])
