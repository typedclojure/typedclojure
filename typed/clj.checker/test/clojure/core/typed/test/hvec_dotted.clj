(ns clojure.core.typed.test.hvec-dotted
  (:require [typed.clojure :as t]))

(declare vector*)

(t/ann ^:no-check vector* (t/All [x :..]
                            [x :.. x -> '[x :.. x]]))

(t/ann foo [-> '[t/Num t/Num t/Num]])
(defn foo []
  (vector* 1 2 3))
