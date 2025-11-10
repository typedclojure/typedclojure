#!/usr/bin/env bb
;; Test runner for documentation test files
;;
;; Iterates over test files and verifies they type-check with expected results
;; based on the 'type' metadata field and check-ns-info results.
;;
;; Usage:
;;   bb script/run-doc-tests.clj           # Run tests and compare with expected results
;;   bb script/run-doc-tests.clj --update  # Update test files with actual check-ns-info results

(ns run-doc-tests
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [babashka.process :as p]))

;; Load common utilities
(load-file "script/doc-test-common.clj")
(require '[doc-test-common :as common])

(def test-dir "typed/clj.checker/test/typed_test/doc")

(defn extract-namespace
  "Extract namespace from file content"
  [content]
  (let [ns-match (re-find #"\(ns\s+(?:\^[^\s]+\s+)?([^\s)]+)" content)]
    (when ns-match (second ns-match))))

(defn run-type-check
  "Run type checking on a namespace file.
   Returns the check-ns-info result map"
  [file-path]
  (try
    (let [content (slurp file-path)
          ns-name (extract-namespace content)
          result (p/shell {:out :string
                           :err :string
                           :continue true}
                          "clojure" 
                          "-M:test" 
                          "-e"
                          (str "(require 'clojure.core.typed)"
                               "(clojure.core.typed/check-ns-info '"
                               ns-name
                               " :max-parallelism 1)"))
          output-lines (str/split-lines (:out result))
          ;; The last line should contain the EDN map
          last-line (last output-lines)
          out-map (edn/read-string last-line)]
      out-map)
    (catch Exception e
      (println "Error running type check on" file-path ":" (.getMessage e))
      {:error (.getMessage e)})))

(defn update-test-file
  "Update test file with actual check-ns-info results"
  [file-path check-result]
  (let [parsed (common/parse-test-file file-path)
        metadata (:metadata parsed)
        code (:code parsed)
        ;; Extract doc name from file path
        file-name (.getName (io/file file-path))
        doc-name (-> file-name
                    (str/replace #"_[a-f0-9]{8}\.clj$" "")
                    (str/replace #"_" "-"))
        new-content (common/generate-test-file-content metadata code check-result doc-name)]
    (spit file-path new-content)
    (println "✓ Updated" file-path "with check-ns-info results")))

(defn test-file-with-update
  "Test a single documentation test file and update it with results"
  [file-path]
  (println "\n----------------------------------------")
  (println "Processing:" file-path)
  
  (let [check-result (run-type-check file-path)]
    (if (:error check-result)
      (do
        (println "✗ Error running type check")
        {:file file-path :error true})
      (do
        (update-test-file file-path check-result)
        {:file file-path :updated true}))))

(defn test-file-with-check
  "Test a single documentation test file and verify expected results"
  [file-path]
  (println "\n----------------------------------------")
  (println "Testing:" file-path)
  
  (let [parsed (common/parse-test-file file-path)
        metadata (:metadata parsed)
        expected-type (keyword (get metadata :type "success"))
        expected-result (:result parsed)
        actual-result (run-type-check file-path)]
    
    (if (:error actual-result)
      (do
        (println "✗ Error running type check")
        {:file file-path
         :passed false
         :error true})
      (let [success? (empty? (:type-errors actual-result))
            type-matches? (= success? (= expected-type :success))
            ;; Compare actual result with expected result from comment block
            results-match? (= (common/sort-data-structure expected-result)
                             (common/sort-data-structure actual-result))
            passed? (and type-matches? results-match?)]
        
        (println "Expected type:" expected-type)
        (println "Actual type:" (if success? :success :fail))
        (println "Type check:" (if type-matches? "✓" "✗"))
        
        (if (nil? expected-result)
          (println "Results check: ⚠ No expected result in comment block")
          (println "Results check:" (if results-match? "✓" "✗")))
        
        (println "Test:" (if passed? "✓ PASS" "✗ FAIL"))
        
        (when-not passed?
          (when-not type-matches?
            (println "\nType mismatch!"))
          (when (and expected-result (not results-match?))
            (println "\nExpected result:")
            (println (common/pprint-str expected-result))
            (println "\nActual result:")
            (println (common/pprint-str actual-result))))
        
        {:file file-path
         :expected-type expected-type
         :actual-type (if success? :success :fail)
         :type-matches type-matches?
         :results-match results-match?
         :passed passed?
         :metadata metadata}))))

(defn run-tests-update
  "Run all documentation tests in update mode"
  []
  (println "========================================")
  (println "Updating Documentation Tests")
  (println "========================================")
  
  (let [results (atom [])
        dir (io/file test-dir)]
    (when (.exists dir)
      (println "\nScanning test directory:" test-dir)
      (doseq [f (filter #(str/ends-with? (.getName %) ".clj")
                       (file-seq dir))
              :when (.isFile f)]
        (let [result (test-file-with-update (.getPath f))]
          (swap! results conj result))))
    
    (println "\n========================================")
    (println "Update Summary")
    (println "========================================")
    (let [total (count @results)
          updated (count (filter :updated @results))
          errors (count (filter :error @results))]
      (println "Total:" total)
      (println "Updated:" updated)
      (println "Errors:" errors)
      
      (when (pos? errors)
        (println "\nFiles with errors:")
        (doseq [r (filter :error @results)]
          (println "  ✗" (:file r))))
      
      (if (zero? errors)
        (do
          (println "\n✓ All files updated!")
          (System/exit 0))
        (do
          (println "\n✗ Some files had errors")
          (System/exit 1))))))

(defn run-tests-check
  "Run all documentation tests in check mode"
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
        (let [result (test-file-with-check (.getPath f))]
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
                   (when (:error r) "(error)")
                   (when-not (:type-matches r) "(type mismatch)")
                   (when-not (:results-match r) "(result mismatch)"))))
      
      (if (zero? failed)
        (do
          (println "\n✓ All tests passed!")
          (System/exit 0))
        (do
          (println "\n✗ Some tests failed")
          (System/exit 1))))))

(defn -main [& args]
  (cond
    (some #{"--update"} args)
    (run-tests-update)
    
    (seq args)
    (do
      (println "Usage:")
      (println "  bb script/run-doc-tests.clj           # Run tests")
      (println "  bb script/run-doc-tests.clj --update  # Update test files with results")
      (System/exit 1))
    
    :else
    (run-tests-check)))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
