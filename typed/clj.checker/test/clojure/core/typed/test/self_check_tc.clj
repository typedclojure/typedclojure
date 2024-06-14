(ns clojure.core.typed.test.self-check-tc
  (:require [clojure.test :refer :all]
            [typed.clj.checker :refer [check-ns4]]
            [clojure.core.typed :as t :refer [check-ns]]))

(deftest check-tc
  (binding [*assert* false]
    (is (check-ns '[clojure.core.typed.coerce-utils
                    typed.cljc.checker.utils
                    typed.cljc.checker.type-rep
                    typed.cljc.checker.cs-rep
                    typed.cljc.checker.name-env
                    clojure.core.typed.util-vars
                    ;typed.cljc.checker.type-ctors
                    typed.cljc.dir]))))

#_ ;;WIP
(deftest analyzer-test
  (is (check-ns4 'typed.cljc.analyzer)))

#_ ;;WIP
(deftest api-test
  (is (check-ns4 'typed.clojure)))

(deftest self-test-annotations
  (is (t/envs)))
