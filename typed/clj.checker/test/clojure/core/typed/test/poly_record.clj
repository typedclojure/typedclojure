(ns clojure.core.typed.test.poly-record
  (:require [typed.clojure :as t]))

(t/ann-record [[foo :variance :invariant]] Foo [b :- (t/U nil t/Num)])
(t/tc-ignore
(defrecord Foo [b]))

; Haskell equivalent
; data Either a b = Either { unwrap :: forall x. (a -> x) -> (b -> x) -> x }
(t/ann-record [[a :variance :covariant]
               [b :variance :covariant]]
              Either
              [unwrap :- (t/All [x] [[a :-> x] [b :-> x] :-> x])])
(defrecord Either [unwrap])

;(t/ann-form
;  ((:unwrap (->Either (t/fn :forall [x]
;                        [f :- [t/Int :-> x]
;                         g :- [t/Bool :-> x]]
;                        (g true))))
;   (t/fn [i :- t/Int] i)
;   (t/fn [b :- t/Bool] b))
;  t/Bool)
