(ns typed-test.cljc.checker.check-below
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.check-below :as sut]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :refer [parse-type]]
            [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]))

(deftest check-below-test
  (is-tc-e 1) ;;load type system
  (is-tc-err (-> 1
                 (t/ann-form t/?)
                 (t/ann-form t/Int)))
  (is-tc-e (-> 1
               (t/ann-form t/Int)
               (t/ann-form t/?)))
  (is-clj (thrown? Error (sut/check-below r/-wild (parse-type `t/Int))))
  (is-clj (both-subtype?
            (sut/check-below (parse-type `t/Int) r/-wild)
            (parse-type `t/Int)))
  (is-clj (both-subtype?
            (sut/check-below (parse-type `t/Int) r/-any)
            (parse-type `t/Any)))
  (is-clj (both-subtype? (sut/check-below (parse-type `[:-> t/Int]) (parse-type `[:-> t/?]))
                         (parse-type `[:-> t/Int])))
  (is-clj (both-subtype? (sut/check-below (parse-type `[[t/Int :-> t/Int] :-> t/Int])
                                          (parse-type `[[t/? :-> (t/Val 1)] :-> t/?]))
                         (parse-type `[[t/Int :-> (t/Val 1)] :-> t/Int]))))
