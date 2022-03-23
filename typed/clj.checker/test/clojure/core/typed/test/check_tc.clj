(ns clojure.core.typed.test.check-tc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t :refer [check-ns]]))

#_(deftest check-tc
  ;fails in hudson
  (is (check-ns '[typed.cljc.checker.utils
                  typed.cljc.checker.type-rep
                  typed.cljc.checker.cs-rep
                  typed.cljc.checker.name-env
                  typed.cljc.checker.type-ctors])))
