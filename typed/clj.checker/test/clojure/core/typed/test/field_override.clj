(ns ^:typed.clojure clojure.core.typed.test.field-override
  (:require 
    [typed.clj.checker.test-utils :refer :all]
    [clojure.test :refer :all]
    [typed.cljc.checker.type-ctors :refer :all]
    [typed.cljc.checker.type-rep :refer :all]))

(deftest persistent-array-map-overrides-test
  (is-clj (= (tc-t clojure.lang.PersistentArrayMap/EMPTY)
             (ret (-complete-hmap {} {})))))
