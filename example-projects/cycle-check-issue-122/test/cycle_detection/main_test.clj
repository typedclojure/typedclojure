(ns cycle-detection.main-test
  (:require [clojure.test :refer [deftest is]]))

(deftest no-cycle
  (is (nil? (require 'cycle-detection.main))))
