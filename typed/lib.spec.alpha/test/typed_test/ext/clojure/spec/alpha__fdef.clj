(ns ^:no-doc typed-test.ext.clojure.spec.alpha__fdef
  (:require [clojure.test :refer [deftest is testing]]
            typed.ext.clojure.spec.alpha
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :refer :all]))

(deftest fdef-test
  (is-tc-e (s/fdef bar :args (s/cat))
           :requires [[clojure.spec.alpha :as s]])
  (is-tc-err (ann-form (s/fdef bar :args (s/cat)) nil)
             :requires [[clojure.spec.alpha :as s]]))
