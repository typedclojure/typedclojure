(ns ^:no-doc typed-test.ext.clojure.spec.alpha__def
  (:require [clojure.test :refer [deftest is testing]]
            typed.ext.clojure.spec.alpha
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :refer :all]))

(deftest sdef-test
  (is-tc-e (s/def :foo/bar int?)
           :requires [[clojure.spec.alpha :as s]])
  (is-tc-err (ann-form (s/def :foo/bar int?) nil)
             :requires [[clojure.spec.alpha :as s]]))
