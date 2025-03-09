(ns ^:typed.clojure clojure.core.typed.test.javadoc
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest javadoc-test
  (is-tc-e #(javadoc 1) [-> t/Any]
           :requires [[clojure.java.javadoc :refer [javadoc]]]))

(deftest add-local-javadoc-test
  (is-tc-e #(add-local-javadoc 1) [-> (t/List t/Any)]
           :requires [[clojure.java.javadoc :refer [add-local-javadoc]]])
  (is-tc-err #(add-local-javadoc 1) [-> String]
             :requires [[clojure.java.javadoc :refer [add-local-javadoc]]]))

(deftest add-remote-javadoc-test
  (is-tc-e #(add-remote-javadoc 
              "org.apache.commons.csv." 
              "http://commons.apache.org/proper/commons-csv/apidocs/index.html") 
           [-> (t/Map t/Any t/Any)]
           :requires [[clojure.java.javadoc :refer [add-remote-javadoc]]])
  (is-tc-err #(add-remote-javadoc 
                "org.apache.commons.csv."
                "http://commons.apache.org/proper/commons-csv/apidocs/index.html")
             [-> String]
             :requires [[clojure.java.javadoc :refer [add-remote-javadoc]]])
  (is-tc-err #(add-remote-javadoc 
                1 
                "http://commons.apache.org/proper/commons-csv/apidocs/index.html") 
             [-> (t/Map t/Any t/Any)]
             :requires [[clojure.java.javadoc :refer [add-remote-javadoc]]]))
