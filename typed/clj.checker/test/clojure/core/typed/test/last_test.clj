(ns ^:typed.clojure clojure.core.typed.test.last-test
  (:refer-clojure :exclude [fn])
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.clojure :as t :refer [ann-form fn]]))

(deftest last-test
  (testing "when we know there is at least one value"
    (is-tc-e
     (fn [coll :- (t/NonEmptySeqable Long)]
       (let [value (last coll)]
         (ann-form value Long))))))
