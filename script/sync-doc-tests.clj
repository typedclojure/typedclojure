#!/usr/bin/env bb
;; Bidirectional documentation sync for Typed Clojure
;;
;; This script maintains synchronization between markdown documentation
;; code blocks and their corresponding test files. Each code block is
;; assigned a permanent UUID and version number, enabling bidirectional
;; editing optimized for technical writers.
;;
;; Features:
;; - UUID-based linking between code blocks and test files
;; - Version tracking to manage sync conflicts
;; - Automatic test file generation on first sync
;; - Type metadata field to indicate expected type checking result
;; - Metadata preservation in both directions
;;
;; Usage:
;;   bb script/sync-doc-tests.clj website/docs/topic.md
;;   bb script/sync-doc-tests.clj --all

(require '[babashka.process :as p])

(defn -main [& args]
  (cond
    (empty? args)
    (do
      (println "Usage:")
      (println "  bb script/sync-doc-tests.clj <markdown-file>")
      (println "  bb script/sync-doc-tests.clj --all")
      (System/exit 1))
    
    :else
    (let [sync-code (if (= "--all" (first args))
                      "(test-utils/sync-all-docs)"
                      (str "(doseq [doc-file " (pr-str (vec args)) "]"
                           "  (test-utils/sync-doc-file doc-file))"))
          result (p/shell {:continue true}
                         "clojure" "-M:test" "-e"
                         (str "(require '[typed.clj.checker.test-utils :as test-utils]) "
                              sync-code))]
      (System/exit (:exit result)))))

(apply -main *command-line-args*)
