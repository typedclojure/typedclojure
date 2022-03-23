(ns clojure.core.typed.test.record-poly-map-ctor
  (:require [typed.clojure :as t]))

; can omit fields with ground types

(t/ann-record [[foo :variance :invariant]] Foo [b :- (t/U nil t/Num)
                                                c :- foo])
; FIXME need defrecord>
(t/tc-ignore
(defrecord Foo [b c])
  )

(map->Foo {:c 1 :b 2})
