(ns clojure.core.typed.test.mapv-test
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest mapv-test
  (testing "when we know the input sequence has values"
    (is-tc-e
     (fn [coll :- (t/NonEmptySeqable String)]
       (let [result (mapv str coll)]
         (ann-form result (t/CountRange 1)))))))
