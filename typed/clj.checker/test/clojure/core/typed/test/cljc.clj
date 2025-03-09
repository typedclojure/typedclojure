(ns ^:typed.clojure clojure.core.typed.test.cljc
  (:require [clojure.test :refer :all]
            [clojure.core.typed :as t]))

(deftest check-ns-should-load-cljc-files
  (remove-ns 'clojure.core.typed.test.dummy-cljc)
  (is (t/check-ns 'clojure.core.typed.test.dummy-cljc)))

(deftest check-ns-should-load-clj-files-before-cljc
  (remove-ns 'clojure.core.typed.test.duplicated)
  (is (t/check-ns 'clojure.core.typed.test.duplicated)))

(deftest allow-reader-conditional-in-ns-form
  (remove-ns 'clojure.core.typed.test.reader-cond)
  (is (t/check-ns 'clojure.core.typed.test.reader-cond)))
