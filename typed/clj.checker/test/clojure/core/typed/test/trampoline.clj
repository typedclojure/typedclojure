(ns clojure.core.typed.test.trampoline
  (:require [typed.clojure :as t]))

(declare funb)

(t/ann-many [t/Num -> (t/Rec [f]
                         (t/U t/Num [-> (t/U t/Num f)]))]
            funa funb)
(defn funa [n]
  (if (= n 0)
    0
    #(funb (dec n))))

(defn funb [n]
  (if (= n 0)
    0
    #(funa (dec n))))

(trampoline funa 100)
