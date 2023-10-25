(ns cycle-detection.main-test
  (:require [clojure.test :refer [deftest is]]))

(deftest no-cycle
  (is (nil? (require 'cycle-detection.main)))
  ;; https://github.com/typedclojure/typedclojure/issues/121
  (is ((requiring-resolve 'clojure.core.typed/envs))))
