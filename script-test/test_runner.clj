#!/usr/bin/env bb

(require '[clojure.test :as t])

(def test-namespaces
  '[check-docs-test])

(apply require test-namespaces)                  

(def test-results
  (apply t/run-tests test-namespaces))

(def failures-and-errors
  (let [{:keys [fail error]} test-results]
    (+ fail error)))

(System/exit (min 1 failures-and-errors))
