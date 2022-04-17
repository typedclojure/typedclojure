;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed-test.clj.checker.check.deftype
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.typed :as t]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.test-utils :refer :all]))

;;;; Checking deftype implementation of protocol methods

(deftest new-instance-method-return-test
  (is (t/check-ns 'clojure.core.typed.test.protocol))
  (is (err/top-level-error-thrown?
        (t/check-ns 'clojure.core.typed.test.protocol-fail))))

(deftest deftype-test
  (is-tc-e (do (ann-datatype T [a :- t/Int])
               (deftype T [a])))
  (is-tc-e (do (defprotocol P
                 (-m [this arg :- t/Int] :- t/Int))
               (ann-datatype T [a :- t/Int])
               (deftype T [a]
                 P
                 (-m [this arg] (+ a arg)))))
  ;; missing method definition is ok
  (is-tc-e (do (defprotocol P
                 (-m [this arg :- t/Int] :- t/Int))
               (ann-datatype T [a :- t/Int])
               (deftype T [a] P)))
  ;; protocol arg has correct type
  (is-tc-err (do (defprotocol P
                   (-m [this arg :- nil] :- t/Int))
                 (ann-datatype T [a :- t/Int])
                 (deftype T [a]
                   P
                   (-m [this arg] (+ a arg)))))
  ;; protocol return checks correctly
  (is-tc-err (do (defprotocol P
                   (-m [this arg :- t/Int] :- nil))
                 (ann-datatype T [a :- t/Int])
                 (deftype T [a]
                   P
                   (-m [this arg] (+ a arg)))))
  (is-tc-e (do (defprotocol P
                 (-m [this arg :- t/Int] :- T))
               (ann-datatype T [a :- t/Int])
               (deftype T [a]
                 P
                 (-m [this arg] this))))
  ;; implements polymorphic protocol
  (is-tc-e (do (t/defprotocol [[x :variance :invariant]]
                 P
                 (-m [this arg :- x] :- x))
               (ann-datatype T [])
               (deftype T []
                 P
                 (-m [this arg] arg))))
  (is-tc-e (do (t/defprotocol [[x :variance :invariant]]
                 P
                 (-m [this arg :- x] :- (P x)))
               (ann-datatype T [])
               (deftype T []
                 P
                 (-m [this arg] this))))
  ;;FIXME
  #_
  (is-tc-err (do (t/defprotocol [[x :variance :invariant]]
                   P
                   (-m [this arg :- x] :- (P x)))
                 (ann-datatype T [])
                 (deftype T []
                   P
                   (-m [this arg] this))
                 (ann-form (->T) (P t/Int)))))

(deftest deftype-poly-ancestor-test
  (is (t/check-ns 'clojure.core.typed.test.protocol-scoping)))

(deftest rewrite-in-deftype-test
  (is
    (should-not-reflect
      (tc-e (do
              (defprotocol B
                (f [m]))
              (ann-datatype F [])
              (deftype F []
                B
                (f [m]
                  (fn [a :- t/Str]
                    (java.io.File. a)))))))))

(deftest defrecord-test
  (is-tc-e (do (ann-record Foo [a :- t/Int])
               (defrecord Foo [a])
               (defn foo [] :- Foo (->Foo 3)))))

(deftest deftype-set!-test
  (is-tc-e (do (defprotocol Setter
                 (setter [_]))
               (ann-datatype Foo [a :- t/Bool])
               (deftype Foo [^:unsynchronized-mutable ^boolean a]
                 Setter
                 (setter [_] (set! a (boolean true))))))
  (is-tc-e (do (defprotocol Setter
                 (setter [_]))
               (ann-datatype Foo [a :- t/Bool])
               (deftype Foo [^:unsynchronized-mutable a]
                 Setter
                 (setter [_] (set! a true))))))
