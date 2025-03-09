(ns ^:typed.clojure clojure.core.typed.test.io
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest delete-file-test
  (is-tc-e #(delete-file "abc") [-> t/Any]
           :requires [[clojure.java.io :refer [delete-file]]])
  (is-tc-e #(delete-file "abc" true) [-> t/Any]
           :requires [[clojure.java.io :refer [delete-file]]])
  (is-tc-err #(delete-file "abc") String
             :requires [[clojure.java.io :refer [delete-file]]]))
