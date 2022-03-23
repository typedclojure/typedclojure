(ns clojure.core.typed.test.CTYP-124-case-update-default
  (:require [typed.clojure :as t]))

(t/ann case-default [(t/U ':a ':b) -> ':b])
(defn case-default [e]
  (case e
    :a :b
    e))
