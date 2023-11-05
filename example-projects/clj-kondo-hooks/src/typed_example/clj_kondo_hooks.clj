;; use (inc :expected-failure) for an expected failure
;; use (inc "unexpected-failure") for an unexpected failure
(ns typed-example.clj-kondo-hooks
  (:require [clojure.core.typed :as ct]
            [typed.clojure :as t]))

;; intentionally blank









;; expected failures from here (add new ones before the unexpected failures)

(t/ann-form (inc :expected-failure)
            (inc "unexpected-failure"))
(ct/ann-form (inc :expected-failure)
             (inc "unexpected-failure"))

(t/atom (inc :expected-failure))
(ct/atom (inc :expected-failure))

(t/atom :- (inc "unexpected-failure")
        (inc :expected-failure))
(ct/atom :- (inc "unexpected-failure")
         (inc :expected-failure))

(t/def tdef1 (inc :expected-failure))
tdef1
(ct/def ctdef1 (inc :expected-failure))
ctdef1

(t/defn tdefn1 [] (inc :expected-failure))
tdefn1
(ct/defn ctdefn1 [] (inc :expected-failure))
ctdefn1

(t/fn tfn1 []
  tfn1
  (inc :expected-failure))
(ct/fn ctfn1 []
  ctfn1
  (inc :expected-failure))

(t/loop [a (inc :expected-failure)]
  (recur (+ a :expected-failure)))
(ct/loop [a (inc :expected-failure)]
  (recur (+ a :expected-failure)))

(t/ref (inc :expected-failure))
(ct/ref (inc :expected-failure))

(t/ref :- (inc "unexpected-failure")
       (inc :expected-failure))
(ct/ref :- (inc "unexpected-failure")
        (inc :expected-failure))

(t/tc-ignore (inc :expected-failure))
(ct/tc-ignore (inc :expected-failure))

;; unexpected failures from here

(t/ann (inc "unexpected-failure")
       (inc "unexpected-failure"))
(ct/ann (inc "unexpected-failure")
        (inc "unexpected-failure"))

(t/ann-datatype (inc "unexpected-failure")
                (inc "unexpected-failure"))
(ct/ann-datatype (inc "unexpected-failure")
                 (inc "unexpected-failure"))

(t/ann-many (inc "unexpected-failure")
            (inc "unexpected-failure"))
(ct/ann-many (inc "unexpected-failure")
             (inc "unexpected-failure"))

(t/ann-protocol (inc "unexpected-failure")
                (inc "unexpected-failure"))
(ct/ann-protocol (inc "unexpected-failure")
                 (inc "unexpected-failure"))

(t/ann-record (inc "unexpected-failure")
                (inc "unexpected-failure"))
(ct/ann-record (inc "unexpected-failure")
                 (inc "unexpected-failure"))

(t/declare-alias-kind (inc "unexpected-failure")
                      (inc "unexpected-failure"))
(ct/declare-alias-kind (inc "unexpected-failure")
                       (inc "unexpected-failure"))

(t/declare-datatypes (inc "unexpected-failure")
                     (inc "unexpected-failure"))
(ct/declare-datatypes (inc "unexpected-failure")
                      (inc "unexpected-failure"))

(t/declare-names (inc "unexpected-failure")
                 (inc "unexpected-failure"))
(ct/declare-names (inc "unexpected-failure")
                  (inc "unexpected-failure"))

(t/declare-protocols (inc "unexpected-failure")
                     (inc "unexpected-failure"))
(ct/declare-protocols (inc "unexpected-failure")
                      (inc "unexpected-failure"))

(t/defalias (inc "unexpected-failure")
  (inc "unexpected-failure"))
(ct/defalias (inc "unexpected-failure")
  (inc "unexpected-failure"))

(t/defn :forall [x]
  tdefn-unexpected1
  [a :- (inc "unexpected-failure")
   b :- (inc "unexpected-failure")]
  :- (inc "unexpected-failure")
  tdefn-unexpected1
  (+ a b))
tdefn-unexpected1
(ct/defn :forall [x]
  ctdefn-unexpected1
  [a :- (inc "unexpected-failure")
   b :- (inc "unexpected-failure")]
  :- (inc "unexpected-failure")
  ctdefn-unexpected1
  (+ a b))
ctdefn-unexpected1

(t/defprotocol [[x :variance :covariant]] TFoo)
(extend-protocol TFoo nil)
(ct/defprotocol [[x :variance :covariant]] CTFoo)
(extend-protocol CTFoo nil)

(t/fn tfn-unexpected1
  [a :- (inc "unexpected-failure")
   b :- (inc "unexpected-failure")]
  :- (inc "unexpected-failure")
  tfn-unexpected1
  (+ a b))

(t/fn :forall [x]
  tfn-unexpected1
  [a :- (inc "unexpected-failure")
   b :- (inc "unexpected-failure")]
  :- (inc "unexpected-failure")
  tfn-unexpected1
  (+ a b))
(ct/fn :forall [x]
  ctfn-unexpected1
  [a :- (inc "unexpected-failure")
   b :- (inc "unexpected-failure")]
  :- (inc "unexpected-failure")
  ctfn-unexpected1
  (+ a b))

(t/loop [a :- (inc "unexpected-failure"), 0]
  (recur (inc a)))
(ct/loop [a :- (inc "unexpected-failure"), 1]
  (recur (inc a)))
