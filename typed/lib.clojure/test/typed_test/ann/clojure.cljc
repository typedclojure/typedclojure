(ns typed-test.ann.clojure
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :as clj]
            [typed.clj.checker.test-utils :as cljs]))

(deftest seqable?-test
  (clj/is-tc-e (seqable? 1)))
