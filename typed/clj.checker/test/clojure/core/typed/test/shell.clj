(ns clojure.core.typed.test.shell
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest sh-test
  (is-tc-e #(sh) [-> (t/Map t/Any t/Any)]
           :requires [[clojure.java.shell :refer [sh]]]))
