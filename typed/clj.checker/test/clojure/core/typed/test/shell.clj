(ns clojure.core.typed.test.shell
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest sh-test
  (is-tc-e #(sh) [-> (Map Any Any)]
           :requires [[clojure.java.shell :refer [sh]]]))
