(ns typed-example.malli-type-providers-test
  (:require [typed.clojure :as t]
            [clojure.test :refer [deftest is]]))

(deftest check-ns-tests
  (is (t/check-ns-clj 'typed-example.malli-type-providers))
  (is (t/check-ns-clj 'typed-example.malli-extensible))
  (is (t/check-ns-clj 'typed-example.malli-global-registry)))
