(ns ^:typed.clojure typed-example.minimal
  "This file can be type checked as either Clojure or ClojureScript via t/check-ns-clj{s}."
  (:require [typed.clojure :as t]))

(t/ann hello-world-error [t/Int :-> t/Str])
(defn hello-world-error [a]
  ;; Note: `inc` is actually a macro in CLJS and inlinable in Clojure, but Typed Clojure type checks
  ;; it as a regular function call before it is expanded for better error messages.
  (inc (do
         ;; look for this form in the type error message.
         #?(:clj :checking-clj :cljs :checking-cljs)
         a)))
