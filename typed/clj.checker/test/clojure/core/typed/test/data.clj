(ns ^:typed.clojure clojure.core.typed.test.data
  (:require [clojure.core.typed :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest diff-test
  (is-tc-e (diff '[1 2 3] '[1 2]) '[t/Any t/Any t/Any]
           :requires [[clojure.data :refer [diff]]]) 
  (is-tc-err (diff '[1 2 3] '[1 2]) (t/List t/Any )
             :requires [[clojure.data :refer [diff]]]))
