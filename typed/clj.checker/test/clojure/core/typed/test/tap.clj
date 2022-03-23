(ns clojure.core.typed.test.tap
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest print-tap-diagnostic-test
  (is-tc-e #(print-tap-diagnostic "abc") [-> t/Any]
           :requires [[clojure.test.tap :refer [print-tap-diagnostic]]])
  (is-tc-err #(print-tap-diagnostic 1) [-> t/Any]
             :requires [[clojure.test.tap :refer [print-tap-diagnostic]]]))

(deftest print-tap-plan-test
  (is-tc-e #(print-tap-plan 1) [-> t/Any]
           :requires [[clojure.test.tap :refer [print-tap-plan]]]))

(deftest print-tap-pass-test
  (is-tc-e #(print-tap-pass 1) [-> t/Any]
           :requires [[clojure.test.tap :refer [print-tap-pass]]]))

(deftest print-tap-fail-test
  (is-tc-e #(print-tap-fail 1) [-> t/Any]
           :requires [[clojure.test.tap :refer [print-tap-fail]]]))
