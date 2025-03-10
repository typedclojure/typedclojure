(ns typed-example.minimal-test
  (:require [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [clojure.test :refer [deftest is]]))

(deftest minimal-test
  ;;TODO add t/cns test from CLJS REPL
  (is (err/top-level-error-thrown? (eval `(t/cns 'typed-example.minimal))))
  (is (err/top-level-error-thrown? (t/check-ns-clj 'typed-example.minimal)))
  (is (err/top-level-error-thrown? (t/check-ns-cljs 'typed-example.minimal))))
