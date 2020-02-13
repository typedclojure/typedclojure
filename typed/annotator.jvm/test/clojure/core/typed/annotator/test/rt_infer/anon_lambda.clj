(ns clojure.core.typed.annotator.test.rt-infer.anon-lambda
  {:lang :core.typed
   :core.typed {:features #{:runtime-infer}}}
  (:require [clojure.core.typed :as t]))

;; Start: Generated by clojure.core.typed - DO NOT EDIT
(declare)
(t/ann b [(t/Vec Long) :-> (t/Coll Long)])
(t/ann c [Long :-> Long])
(t/ann d [Long :-> Long])
;; End: Generated by clojure.core.typed - DO NOT EDIT
(defmacro hidden-f [& args]
  `(fn ~@args))

(defn b [coll]
  (->> coll
       (map 
         (t/ann-form
           (fn [n] (inc n))
           [Long :-> Long])
         )  ;; hello
       (filter
         (t/ann-form
           (fn [n] (odd? n))
           [Long :-> (t/U Boolean false)])
         )))

(b [1 2 3 4 5])

;(defn c [n]
;  ((hidden-f [a] (inc a))
;   n))
;
;(c 1)

(defn d [n]
  (#(inc ^{::t/ann Long} %) n))

(d 1)