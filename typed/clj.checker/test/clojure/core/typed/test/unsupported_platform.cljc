(ns ^:typed.clojure clojure.core.typed.test.unsupported-platform
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]
            [typed.clj.checker.test-utils :refer [is-tc-err]]))

;; Test that UnsupportedPlatform throws clear error messages
;; when the type is actually parsed (via pred)

(deftest test-unsupported-platform-error
  (testing "UnsupportedPlatform throws error when used with pred"
    ;; Using pred to test the type causes it to be parsed and should error
    (is-tc-err
      (t/pred t/UnsupportedPlatform))))

