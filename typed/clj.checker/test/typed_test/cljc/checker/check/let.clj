(ns ^:typed.clojure typed-test.cljc.checker.check.let
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err]]))

(deftest core-loop-unannotated-test
  (is-tc-e #(cc/loop [a 1] (t/ann-form a Long)))
  (is-tc-err #(cc/loop [a 1] (recur (t/ann-form 1 t/Int))))
  ;; An unannotated loop binding's init type is generalized to the class of the
  ;; literal (here Long), so the loop invariant accepts every iteration: (recur 2)
  ;; now checks, and the binding is no longer the singleton (Val 1).
  (do (is-tc-e #(cc/loop [a 1] (recur 2)))
      (is-tc-err #(cc/loop [a 1] (t/ann-form a '1)))
      (is-tc-e #(cc/loop [a 1] (t/ann-form a Long)))))
