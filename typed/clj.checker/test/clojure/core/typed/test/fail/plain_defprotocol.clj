(ns clojure.core.typed.test.fail.plain-defprotocol
  (:require [typed.clojure :as t]))

; should throw top level type error
(defprotocol Foo (bar [this]))
