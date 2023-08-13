(ns typed-test.cljc.checker.check-below
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.check-below :as sut]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :refer [parse-type]]
            [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]))

(deftest check-below-test
  (is-tc-e 1) ;;load type system
  (is-clj (thrown? Exception (sut/check-below r/-infer-any (parse-type `t/Int))))
  (is-clj (both-subtype?
            (sut/check-below (parse-type `t/Int) r/-infer-any)
            (parse-type `t/Int)))
  (is-clj (both-subtype?
            (sut/check-below (parse-type `t/Int) r/-any)
            (parse-type `t/Any)))
  (is-clj (both-subtype? (sut/check-below (parse-type `[:-> t/Int]) (parse-type `[:-> ^:clojure.core.typed/infer t/Any]))
                         (parse-type `[:-> t/Int])))
  (is-clj (both-subtype? (sut/check-below (parse-type `[[t/Int :-> t/Int] :-> t/Int])
                                          (parse-type `[[^:clojure.core.typed/infer t/Any :-> (t/Val 1)] :-> ^:clojure.core.typed/infer t/Any]))
                         (parse-type `[[t/Int :-> (t/Val 1)] :-> t/Int])))
  )
