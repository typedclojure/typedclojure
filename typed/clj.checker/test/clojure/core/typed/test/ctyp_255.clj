(ns clojure.core.typed.test.ctyp-255
  (:require [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :refer :all]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [clojure.core.typed :as t]
            [typed.cljc.checker.init :as init]
            [typed.clj.checker.test-utils :refer :all]))

(deftest ctyp-255-test
  (testing "unknown implementation of unparse uses clojure.core.typed
           for special types"
    (is (= (unparse-type r/-any)
           'clojure.core.typed/Any)))
  (testing "can print a tc-e result with :unknown"
    (is (do (prn (tc-e 1))
            true))))
