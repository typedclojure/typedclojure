(ns ^:no-doc typed-test.ext.clojure.core.typed.unsafe
  (:require [clojure.test :refer [deftest is]]
            typed.clj.ext.clojure.core.typed.unsafe ;; load
            [clojure.core.typed :as t]
            [clojure.core.typed.test.test-utils :refer :all]))

(deftest ignore-unsafe-cast-test
  (is-tc-e (unsafe/ignore-with-unchecked-cast
             (fn [] (+ 'a 1))
             String)
           String)
  (is-tc-err (ann-form
               (unsafe/ignore-with-unchecked-cast
                 (fn [] (+ 'a 1))
                 String)
               t/Int)))
