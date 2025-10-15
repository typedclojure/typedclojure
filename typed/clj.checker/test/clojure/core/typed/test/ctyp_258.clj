(ns ^:typed.clojure clojure.core.typed.test.ctyp-258
  (:require [clojure.test :refer :all]
            [typed.cljc.checker.type-rep :refer :all]
            [typed.cljc.checker.proposition-rep :refer :all]
            [typed.cljc.checker.proposition-ops :refer :all]
            [typed.clj.checker.test-utils :refer :all]))

(deftest ctyp-258-test
  (testing "(is Any ..) = tt"
    (is (= (-proposition -any 'a)
           -top)))
  (testing "(is Nothing ..) = ff"
    (is (= (-proposition -nothing 'a)
           -bot)))
  (testing "(! Any ..) = ff"
    (is (= (-not-proposition -any 'a)
           -bot)))
  (testing "(! Nothing ..) = tt"
    (is (= (-not-proposition -nothing 'a)
           -top))))
