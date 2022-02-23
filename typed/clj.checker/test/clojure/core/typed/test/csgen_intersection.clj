(ns clojure.core.typed.test.csgen-intersection
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.cljc.checker.cs-gen :as cgen]
            ))

(deftest csgen-intersect
  (is-tc-e (do 
             (defprotocol 
               [[x :variance :covariant]]
               ICollection')
             (defprotocol 
               [[x :variance :covariant]]
               ISeq')
             (defprotocol 
               [[x :variance :covariant]]
               ISeqable')
             (defprotocol 
               [[x :variance :covariant]]
               IList')
             (defalias
               NEColl'
               (t/TFn [[x :variance :covariant]]
                    (ICollection' x)))
             (defalias
               NEASeq'
               (t/TFn [[x :variance :covariant]]
                    (t/I
                      (ICollection' x)
                      (ISeqable' x)
                      (ISeq' x)
                      (IList' x)
                      #_(CountRange 1))))
             (fn [seq' :- (t/All [x] [(NEColl' x) -> (NEASeq' x)])
                  a :- (NEColl' t/Int)] 
               :- (NEASeq' Number)
               (seq' a)))))
