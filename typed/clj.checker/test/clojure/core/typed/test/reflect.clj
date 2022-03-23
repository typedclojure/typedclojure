(ns clojure.core.typed.test.reflect
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest type-reflect-test
  (is-tc-e #(reflect 1) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.reflect :refer [reflect]]])
  (is-tc-err #(reflect 1) [-> t/Str]
             :requires [[clojure.reflect :refer [reflect]]]))
