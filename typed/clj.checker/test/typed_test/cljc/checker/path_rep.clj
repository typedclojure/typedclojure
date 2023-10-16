(ns typed-test.cljc.checker.path-rep
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :refer [is-tc-e]]))

(deftest SeqPE-test
  (is-tc-e (let [a nil]
             (if (seq a)
               (inc :unreachable)
               (inc 1))))
  (is-tc-e nil (t/SeqOn nil))
  (is-tc-e (seq nil) (t/SeqOn nil))
  (is-tc-e (fn [seq :- (t/All [[x :< (t/Seqable t/Any)]]
                              [x :-> (t/SeqOn x) :object {:id 0 :path [Seq]}])]
             (let [a nil
                   test (seq a)]
               (clojure.core.typed/print-env "test")
               (if test
                 (inc :unreachable)
                 (inc 1)))))
  (is-tc-e (fn [seq :- (t/All [[x :< (t/Seqable t/Any)]]
                              [x :-> (t/SeqOn x) :object {:id 0 :path [Seq]}])]
             (let [a [1 2 3]
                   test (seq a)]
               (clojure.core.typed/print-env "test")
               (if test
                 (inc 1)
                 (inc :unreachable))))))
