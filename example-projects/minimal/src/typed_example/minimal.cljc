(ns typed-example.minimal
  (:require [typed.clojure :as t]))

(t/ann hello-world-error [t/Int :-> t/Str])
(defn hello-world-error [a]
  (inc (do #?(:clj :checking-clj :cljs :checking-cljs)
           a)))
