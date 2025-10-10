#!/usr/bin/env bb
;; Test runner for documentation test files
;;
;; Iterates over test files and verifies they type-check with expected results
;; based on the 'type' metadata field.
;;
;; Usage:
;;   bb script/run-doc-tests.clj

(ns run-doc-tests
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.process :as p]))

(def test-dir "typed/clj.checker/test/typed_test/doc")

(defn parse-metadata
  "Parse metadata from test file comments"
  [content]
  (let [pattern #";;\s*doc-test:\s*(.+)"
        match (re-find pattern content)]
    (when match
      (let [pairs (re-seq #"(\w+)=([^\s]+)" (second match))]
        (into {} (map (fn [[_ k v]] [(keyword k) v]) pairs))))))

(defn run-type-check
  "Run type checking on a namespace file.
   Returns {:success boolean :output string}"
  [file-path]
  (try
    (let [result (p/shell {:out :string
                          :err :string
                          :continue true}
                         "clojure -M:test -e"
                         (str "(require 'typed.clojure)"
                              "(typed.clojure/check-ns-info '"
                              ;; Extract namespace from file
                              (let [content (slurp file-path)
                                    ns-match (re-find #"\(ns\s+([^\s)]+)" content)]
                                (when ns-match (second ns-match)))
                              ")"))]
      {:success (zero? (:exit result))
       :output (str (:out result) "\n" (:err result))})
    (catch Exception e
      {:success false
       :output (.getMessage e)})))

(defn test-file
  "Test a single documentation test file"
  [file-path]
  (println "\n----------------------------------------")
  (println "Testing:" file-path)
  
  (let [content (slurp file-path)
        metadata (parse-metadata content)
        expected-type (keyword (get metadata :type "success"))
        expected-result expected-type
        result (run-type-check file-path)
        success? (:success result)
        passed? (= success? (= expected-result :success))]
    
    (println "Expected:" expected-result)
    (println "Result:" (if success? "Type check passed" "Type check failed"))
    (println "Test:" (if passed? "✓ PASS" "✗ FAIL"))
    
    (when-not passed?
      (println "\nOutput:")
      (println (:output result)))
    
    {:file file-path
     :expected expected-result
     :actual (if success? :success :fail)
     :passed passed?
     :metadata metadata}))

(defn run-tests
  "Run all documentation tests"
  []
  (println "========================================")
  (println "Running Documentation Tests")
  (println "========================================")
  
  (let [results (atom [])
        dir (io/file test-dir)]
    (when (.exists dir)
      (println "\nScanning test directory:" test-dir)
      (doseq [f (filter #(str/ends-with? (.getName %) ".clj")
                       (file-seq dir))
              :when (.isFile f)]
        (let [result (test-file (.getPath f))]
          (swap! results conj result))))
    
    (println "\n========================================")
    (println "Test Summary")
    (println "========================================")
    (let [total (count @results)
          passed (count (filter :passed @results))
          failed (- total passed)]
      (println "Total:" total)
      (println "Passed:" passed)
      (println "Failed:" failed)
      
      (when (pos? failed)
        (println "\nFailed tests:")
        (doseq [r (filter (complement :passed) @results)]
          (println "  ✗" (:file r)
                   "- expected" (:expected r)
                   "but got" (:actual r))))
      
      (if (zero? failed)
        (do
          (println "\n✓ All tests passed!")
          (System/exit 0))
        (do
          (println "\n✗ Some tests failed")
          (System/exit 1))))))

(defn -main [& args]
  (run-tests))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
