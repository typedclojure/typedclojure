(ns ^:typed.clojure clojure.core.typed.test.atom
  (:require [typed.clojure :as t :refer [ann ann-form defalias]]
            [clojure.repl :refer [pst]])
  (:import (clojure.lang IPersistentMap Symbol)))

(ann my-atom (t/Atom Number))
(def my-atom (atom 2))

(reset! my-atom 1)
(swap! my-atom (t/fn [x :- Number] (+ x 2 3)))

(defalias InnerEntry '{:c '{:d String}})
(defalias Entry '{:a '{:b (IPersistentMap Symbol (t/Atom InnerEntry))}})

(ann complicated (t/Atom Entry))
(def complicated (atom {:a {:b {}}}))

;(swap! complicated update-in [:a :b 'a] #(swap! (or % (atom {})) assoc-in [:c :d] "b"))

(swap! complicated (ann-form
                     (fn [c] 
                       (-> c
                        (update-in [:a :b 'a]
                                   (fn [a] (swap! (or a (atom {}))
                                                  (fn [i]
                                                    (-> i (assoc-in [:c :d] "b"))))))))
                     [Entry -> Entry]))
