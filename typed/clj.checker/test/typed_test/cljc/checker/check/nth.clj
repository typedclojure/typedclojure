(ns ^:typed.clojure ^:no-doc typed-test.cljc.checker.check.nth
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clj.checker.test-utils :refer :all]))

(deftest despecialize-on-invalid-first-arg-test
  (is (= :typed.clojure/app-type-error
         (-> (is-tc-err-messages #(nth #{} 0))
             :type-errors
             first
             :type-error))))
