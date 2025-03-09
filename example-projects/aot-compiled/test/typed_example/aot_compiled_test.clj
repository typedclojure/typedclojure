(ns typed-example.aot-compiled-test
  (:require [clojure.test :refer [deftest is]]))

(deftest aot-compiled-test
  (compile 'typed-example.aot-compiled))
