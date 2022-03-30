(ns typed-example.minimal-test
  (:require [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [clojure.test :refer [deftest is]]))

(deftest minimal-test
  (is (err/top-level-type-error-thrown? (t/check-ns 'typed-example.minimal)))
  (is (err/top-level-type-error-thrown? (t/check-ns-clj 'typed-example.minimal)))
  (is (err/top-level-type-error-thrown? (t/check-ns-cljs 'typed-example.minimal)))
  ;;TODO add t/check-ns test from CLJS REPL
  )
