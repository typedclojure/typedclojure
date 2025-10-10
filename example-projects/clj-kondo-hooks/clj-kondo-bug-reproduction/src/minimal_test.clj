(ns minimal-test
  (:require [clojure.core.typed :as t]))

;; This should lint without errors when using the macro hooks
;; The t/def macro should expand to strip the type annotation
(t/def x :- Number 1)

;; This should produce an error (keyword where number expected)
(t/defn foo [] (inc :expected-error))
