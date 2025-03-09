(ns ^:typed.clojure clojure.core.typed.test.edn
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest read-string-test
  (is-tc-e #(edn/read-string "abc")
           :requires [[clojure.edn :as edn]])
  (is-tc-err #(edn/read-string "abc") [-> String]
             :requires [[clojure.edn :as edn]]))
