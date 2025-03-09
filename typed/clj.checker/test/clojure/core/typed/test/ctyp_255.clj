(ns ^:typed.clojure clojure.core.typed.test.ctyp-255
  (:require [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :refer :all]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]))

(deftest ctyp-255-test
  (testing "unknown implementation of unparse uses clojure.core.typed for special types"
    (is (= (unparse-type r/-any (clj-opts))
           'typed.clojure/Any)))
  (testing "can print a tc-e result with :unknown"
    (is (do (prn (tc-e 1))
            true))))
