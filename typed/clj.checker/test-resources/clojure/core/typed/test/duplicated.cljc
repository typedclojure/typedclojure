(ns clojure.core.typed.test.duplicated
  (:require [typed.clojure :as t]))

; this is intentional type error for test purposes
(inc 'a)
