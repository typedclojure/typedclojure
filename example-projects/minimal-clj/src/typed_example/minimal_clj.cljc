(ns typed-example.minimal-clj
  "This file can be type checked as Clojure via t/check-ns-clj."
  (:require [typed.clojure :as t]))

(t/ann hello-world-error [t/Int :-> t/Str])
(defn hello-world-error [a]
  ;; Note: `inc` is inlinable in Clojure, but Typed Clojure type checks
  ;; it as a regular function call before it is expanded for better error messages.
  (inc a))
