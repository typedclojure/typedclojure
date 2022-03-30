(ns typed-example.minimal-clj-test
  (:require [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [clojure.test :refer [deftest is]]))

(deftest minimal-clj-test
  (is (err/top-level-type-error-thrown? (eval `(t/cns 'typed-example.minimal-clj))))
  (is (err/top-level-type-error-thrown? (t/check-ns-clj 'typed-example.minimal-clj))))
