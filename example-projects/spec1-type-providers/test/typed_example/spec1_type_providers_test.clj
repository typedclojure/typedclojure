(ns typed-example.spec1-type-providers-test
  (:require [typed.clojure :as t]
            [clojure.test :refer [deftest is]]))

(deftest check-ns-tests
  (is (t/check-ns-clj 'typed-example.spec1-type-providers))
  (is (t/check-ns-clj 'typed-example.spec1-extensible)))
