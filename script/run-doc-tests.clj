#!/usr/bin/env bb
;; Test runner for documentation test files
;;
;; Runs type checking on all documentation test files and updates them
;; with the latest check-ns-info results.
;;
;; Usage:
;;   bb script/run-doc-tests.clj

(require '[babashka.process :as p])

(let [result (p/shell {:continue true}
                      "clojure" "-M:test" "-e"
                      "(System/setProperty \"typed.clojure.doc-test.sync\" \"true\") (require '[clojure.test]) (require '[typed-test.doc-test]) (clojure.test/run-tests 'typed-test.doc-test)")]
  (System/exit (:exit result)))
