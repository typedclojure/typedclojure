(ns ^:typed.clojure clojure.core.typed.test.cast
  (:refer-clojure :exclude [cast])
  (:require 
    [typed.clj.checker.test-utils :refer :all]
    [clojure.test :refer :all]
    [clojure.core.typed :as t]))

(deftest typed-cast-test
  (is-tc-e (cast t/Int 1) t/Int)
  (is-tc-err #(cast t/Int (inc 'a)))
  (is-tc-e (cast [t/Int -> t/Int] identity) [t/Int -> t/Int])
  (is-tc-e (cast '{:a t/Int} {:a 1}) '{:a t/Int})
  (is-tc-e #(cast t/Int nil) [:-> t/Int])
  ;; runtime errors
  (is (thrown? Exception (t/check-form-info `(t/cast t/Int nil))))
  (is (thrown? Exception
               (tc-e (cast '{:a t/Int} {:a nil}))))
  (is (thrown? Exception
               (tc-e
                 ((:a (cast '{:a [t/Int :-> t/Int]} {:a str}))
                  1))))
  )
