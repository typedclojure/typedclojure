(ns ^:typed.clojure clojure.core.typed.test.namespaced-specials
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.clojure :as t]))

;backwards compatibility tests for type syntax

(deftest value-test
  (is-tc-e (ann-form :a ':a))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form :a (clojure.core.typed/Value :a)))
  (is-tc-e (ann-form :a (t/Value :a)))
  ; runtime parsing
  (is-tc-e (defalias Foo (t/Value :a)))
  ; old syntax
  (is-cf (clojure.core.typed/defalias Foo (clojure.core.typed/Value :a))))

(deftest quote-test
  ; bare quote should always resolve to clojure.core/quote
  (is-tc-e (do (defalias quote t/Any)
               (defalias TAlias (quote :a))
               (ann-form :a TAlias)))
  (is-tc-e (do (defalias quote t/Any)
               (defalias TAlias (quote {:a t/Num}))
               (ann-form {:a 1} TAlias))))

(deftest hmap-test
  (is-tc-e (ann-form {:a 1} (HMap :mandatory {:a t/Num})))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form {:a 1} (HMap :mandatory {:a clojure.core.typed/Num})))
  ;new syntax
  (is-tc-e (ann-form {:a 1} (t/HMap :mandatory {:a t/Num}))))

(deftest hvec-test
  (is-tc-e (ann-form [1] (t/HVec [t/Num])))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form 
           [1]
           (clojure.core.typed/HVec [clojure.core.typed/Num])))
  ;new syntax
  (is-tc-e (ann-form 
             [1]
             (t/HVec [t/Num]))))

(deftest Any-test
  (is-tc-e (ann-form {:a 1} t/Any))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form {:a 1} clojure.core.typed/Any))
  ; new syntax
  (is-tc-e (ann-form {:a 1} t/Any)))

(deftest Nothing-test
  (is-tc-e (ann-form (fn [] (throw (Exception.))) 
                     [-> t/Nothing]))
  ;old syntax
  (is-cf (clojure.core.typed/ann-form
           (fn [] (throw (Exception.))) 
           [-> clojure.core.typed/Nothing]))
  ; new syntax
  (is-tc-e (ann-form 
             (fn [] (throw (Exception.))) 
             [-> t/Nothing])))
