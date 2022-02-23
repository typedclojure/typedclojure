(ns clojure.core.typed.test.shell
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest sh-test
  (is-tc-e #(sh) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.java.shell :refer [sh]]]))
