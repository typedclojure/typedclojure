(ns clojure.core.typed.test.assoc
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest assoc-test
  (is-tc-e (assoc {:a 1} :b 2)
           '{:a Number :b Number})

  (is-tc-err #(let [a 1]
                (assoc a :b 2)))
  (is-tc-err
    (do (t/ann-record FooRec [a :- t/Num
                              b :- t/Symbol])
        (defrecord FooRec [a b])
        (assoc (->FooRec 1 'a) :a 'b)))

  (is-tc-e
    (do (t/ann-record FooRec [a :- t/Num
                              b :- t/Symbol])
        (defrecord FooRec [a b])
        (assoc (->FooRec 1 'a) :a 4)))

  ;intersections
  (is-tc-e (t/fn [m :- (t/I (t/HMap :mandatory {:foo t/Num})
                            (t/HMap :mandatory {:bar t/Num}))]
             :- '{:foo t/Num, :bar t/Num, :baz t/Num}
             (assoc m :baz 2))))
