(ns clojure.core.typed.test.record-optional-key
  (:require [typed.clojure :as t]))

(t/ann-record Foo [a :- (t/U nil t/Num)])
(defrecord Foo [a])

(map->Foo {})

(t/ann-record FooP [a :- (t/U nil t/Num)])
(defrecord FooP [a])

(map->FooP {})
