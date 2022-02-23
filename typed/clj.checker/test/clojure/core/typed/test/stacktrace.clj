(ns clojure.core.typed.test.stacktrace  
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest e-test
  (is-tc-e #(e) [-> t/Any]
           :requires [[clojure.stacktrace :refer [e]]]))

(deftest print-cause-trace-test
  (is-tc-e #(print-cause-trace (Exception. "a")) [-> t/Any]
           :requires [[clojure.stacktrace :refer [print-cause-trace]]])
  (is-tc-err #(print-cause-trace "A") [-> t/Any]
             :requires [[clojure.stacktrace :refer [print-cause-trace]]]))

(deftest print-stack-trace-test
  (is-tc-e #(print-stack-trace (Exception. "a")) [-> t/Any]
           :requires [[clojure.stacktrace :refer [print-stack-trace]]])
  (is-tc-err #(print-stack-trace "AC") [-> t/Any]
             :requires [[clojure.stacktrace :refer [print-stack-trace]]]))

(deftest print-throwable-test
  (is-tc-e #(print-throwable (Exception. "a")) [-> t/Any]
           :requires [[clojure.stacktrace :refer [print-throwable]]])
  (is-tc-err #(print-throwable "A")  [-> t/Any]
             :requires [[clojure.stacktrace :refer [print-throwable]]]))

(deftest root-cause-test
  (is-tc-e #(root-cause (Exception. "a")) [-> Throwable]
           :requires [[clojure.stacktrace :refer [root-cause]]])
  (is-tc-err #(root-cause (Exception. "a")) [-> Exception]
             :requires [[clojure.stacktrace :refer [root-cause]]]))
