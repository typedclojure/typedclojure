(ns ^:typed.clojure clojure.core.typed.test.instant
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest read-instant-date-test
  (is-tc-e (read-instant-date "2014-04-23T10:13Z") java.util.Date
           :requires [[clojure.instant :refer [read-instant-date]]])
  (is-tc-err (read-instant-date "2014-04-23T10:13Z") t/Str
             :requires [[clojure.instant :refer [read-instant-date]]])
  (is-tc-err (read-instant-date 201404231013) java.util.Date
             :requires [[clojure.instant :refer [read-instant-date]]]))
           
(deftest read-instant-calendar-test
  (is-tc-e (read-instant-calendar "2014-04-23T10:13Z") java.util.GregorianCalendar             
           :requires [[clojure.instant :refer [read-instant-calendar]]])
  (is-tc-err (read-instant-calendar "2014-04-23T10:13Z") t/Str
             :requires [[clojure.instant :refer [read-instant-calendar]]])
  (is-tc-err (read-instant-calendar 201404231013) java.util.GregorianCalendar
             :requires [[clojure.instant :refer [read-instant-calendar]]]))

(deftest read-instant-timestamp-test
  (is-tc-e (read-instant-timestamp "2014-04-23T10:13Z") java.sql.Timestamp             
           :requires [[clojure.instant :refer [read-instant-timestamp]]])
  (is-tc-err (read-instant-timestamp "2014-04-23T10:13Z") t/Str
             :requires [[clojure.instant :refer [read-instant-timestamp]]])
  (is-tc-err (read-instant-timestamp 201404231013) java.sql.Timestamp            
             :requires [[clojure.instant :refer [read-instant-timestamp]]])) 
