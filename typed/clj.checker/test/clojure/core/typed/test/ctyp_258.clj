(ns ^:typed.clojure clojure.core.typed.test.ctyp-258
  (:require [clojure.test :refer :all]
            [typed.cljc.checker.type-rep :refer :all]
            [typed.cljc.checker.filter-rep :refer :all]
            [typed.cljc.checker.filter-ops :refer :all]
            [typed.clj.checker.test-utils :refer :all]))

(deftest ctyp-258-test
  (testing "(is Any ..) = tt"
    (is (= (-filter -any 'a)
           -top)))
  (testing "(is Nothing ..) = ff"
    (is (= (-filter -nothing 'a)
           -bot)))
  (testing "(! Any ..) = ff"
    (is (= (-not-filter -any 'a)
           -bot)))
  (testing "(! Nothing ..) = tt"
    (is (= (-not-filter -nothing 'a)
           -top))))
