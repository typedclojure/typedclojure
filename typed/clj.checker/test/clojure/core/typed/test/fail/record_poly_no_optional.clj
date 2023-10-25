(ns clojure.core.typed.test.fail.record-poly-no-optional
  (:require [typed.clojure :as t]))

; all map->* keys are mandatory in polymorphic records

(t/ann-record [[foo :variance :invariant]] Foo [b :- (t/U nil t/Num)])
(t/tc-ignore
(defrecord Foo [b])
  )

(map->Foo {})
