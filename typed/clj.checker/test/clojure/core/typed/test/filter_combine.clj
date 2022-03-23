(ns clojure.core.typed.test.filter-combine
  (:require [typed.clojure :as t :refer [ann-form]]))

; macroexpansion of `or` is understood
(fn [a]
  (when (or (string? a)
            (symbol? a))
    (ann-form a (t/U t/Sym String))))

;exceptional control flow
(fn [a]
  {:pre [(or (string? a)
             (symbol? a))]}
  (ann-form a (t/U t/Sym String)))

;TODO
(comment
(t/fn [a :- (U nil '{:d Number})]
  {:pre [(:d a)]}
  (ann-form (:d a) Number))
  )
