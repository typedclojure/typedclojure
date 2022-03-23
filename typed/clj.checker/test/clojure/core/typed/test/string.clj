(ns clojure.core.typed.test.string
  (:require [typed.clojure :as t] 
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest escape-test
  (is-tc-e (escape "I want 1 < 2 as HTML, & other good things."
                   {\< "&", \> "&", \& "&"}) t/Str            
           :requires [[clojure.string :refer [escape]]])
  (is-tc-e (escape "I want 1 < 2 as HTML, & other good things."
                   (fn [a]
                     (case a
                       \< "&"
                       \> "&" 
                       \& "&"
                       nil)))
           t/Str            
           :requires [[clojure.string :refer [escape]]])
  (is-tc-err (escape "I want 1 < 2 as HTML, & other good things."
                     {\< "&", \> "&", \& "&"}) t/Bool            
             :requires [[clojure.string :refer [escape]]])
  (is-tc-err (escape 1 {\< "&", \> "&", \& "&"}) t/Str
             :requires [[clojure.string :refer [escape]]]))

(deftest split-lines-test
  (is-tc-e (split-lines "abc\n abc") (t/Vec t/Str)            
           :requires [[clojure.string :refer [split-lines]]])
  (is-tc-err (split-lines "abc\n abc") t/Bool             
             :requires [[clojure.string :refer [split-lines]]])
  (is-tc-err (split-lines 1) (t/Vec t/Str)
             :requires [[clojure.string :refer [split-lines]]]))
