(ns clojure.core.typed.test.fail.record-no-nil
  (:require [typed.clojure :as t]))

(t/ann-record Foo [a :- t/Num])
(defrecord Foo [a])

(map->Foo {})
