(ns clojure.core.typed.test.collatz
  (:require [typed.clojure :as t]))

(t/ann collatz [t/Num -> t/Num])
(defn collatz [n]
  (cond
    (= 1 n) 
     1
    (and (integer? n) 
         (even? n)) 
     (collatz (/ n 2))
    :else 
     (collatz (inc (* 3 n)))))

(collatz 10)
