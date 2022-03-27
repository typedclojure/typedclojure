(ns typed-test.provider.spec1
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as uv]
            [clojure.core.typed.current-impl :as impl]))

;; these tests implicitly load typed.clj{s}.provider.spec1 
(deftest var-provider-test
  (is (t/check-ns-clj 'typed-test.provider.spec1__test1))
  (is (err/top-level-error-thrown? (t/check-ns-clj 'typed-test.provider.spec1__test-fail1))))
