(ns clojure.core.typed.test.browse
  (:require [typed.clojure :as t]
            [clojure.test :refer :all]                
            [typed.clj.checker.test-utils :refer :all]))

(deftest browse-url-test
  (is-tc-e #(browse-url "www.typedclojure.org") [-> t/Any]
           :requires [[clojure.java.browse :refer [browse-url]]])
  (is-tc-err #(browse-url "www.typedclojure.org") [-> Boolean]
             :requires [[clojure.java.browse :refer [browse-url]]]))
