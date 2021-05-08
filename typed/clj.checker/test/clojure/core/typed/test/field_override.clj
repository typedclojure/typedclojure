(ns clojure.core.typed.test.field-override
  (:require 
    [clojure.core.typed.test.test-utils :refer :all]
    [clojure.test :refer :all]
    [typed.cljc.checker.type-ctors :refer :all]
    [typed.cljc.checker.type-rep :refer :all]))

(deftest persistent-array-map-overrides-test
  (is-clj (= (tc-t clojure.lang.PersistentArrayMap/EMPTY)
             (ret (-complete-hmap {})))))
