(ns clojure.core.typed.test.unsound-record
  (:require [typed.clojure :as t]))

(t/ann-record Foo [a :- t/Num])
(defrecord Foo [a])

(t/ann unsound [(t/Map t/Any t/Any) -> t/Num])
(defn unsound [r]
  (let [r (assoc r :a nil)]
    (assert (instance? Foo r))
    (inc (:a r))))

(fn []
  (unsound (->Foo 1)))
