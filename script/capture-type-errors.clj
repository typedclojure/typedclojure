#!/usr/bin/env bb
;; Script to run type checker on doc tests and capture error messages
;;
;; This script:
;; 1. Runs type checking on a test file
;; 2. Captures any type errors
;; 3. Replaces TYPE-ERROR-PLACEHOLDER comments with actual error messages
;; 4. Increments the version number
;; 5. Saves the updated test file
;;
;; Usage:
;;   bb script/capture-type-errors.clj <test-file>

(ns capture-type-errors
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [babashka.process :as p]))

(defn parse-metadata
  "Parse metadata from test file comments"
  [content]
  (let [pattern #";;\s*doc-test:\s*(.+)"
        match (re-find pattern content)]
    (when match
      (let [pairs (re-seq #"(\w+)=([^\s]+)" (second match))]
        (into {} (map (fn [[_ k v]] [(keyword k) v]) pairs))))))

(defn format-metadata
  "Format metadata for Clojure comment"
  [metadata]
  (let [pairs (str/join " "
                       (map (fn [[k v]] (str (name k) "=" v))
                            (sort-by first metadata)))]
    (str ";; doc-test: " pairs)))

(defn increment-version
  "Increment version number in metadata"
  [metadata]
  (update metadata :version #(str (inc (parse-long (or % "1"))))))

(defn run-type-check
  "Run type checking on a test file and capture output"
  [file-path]
  (try
    (let [;; Extract namespace from file
          content (slurp file-path)
          ;; Match namespace, skipping any metadata (^:...)
          ns-match (re-find #"\(ns\s+(?:\^[^\s]+\s+)?([^\s)]+)" content)
          ns-name (when ns-match (second ns-match))
          
          ;; Build the expression as a single string with proper quoting
          expr (str "(do (require (quote typed.clojure)) "
                   "(require (quote " ns-name ")) "
                   "(typed.clojure/check-ns-clj (quote " ns-name ")))")
          
          ;; Run type checking using check-ns-clj (the correct API)
          result (p/shell {:out :string
                          :err :string
                          :continue true}
                         "clojure" "-M:test" "-e" expr)]
      {:success (zero? (:exit result))
       :stdout (:out result)
       :stderr (:err result)
       :output (str (:out result) "\n" (:err result))})
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :output (.getMessage e)})))

(defn extract-error-message
  "Extract relevant error message from type checker output"
  [output]
  (let [;; Try to find the Type Error section
        lines (str/split-lines output)
        ;; Find where "Type Error" starts
        type-error-idx (first (keep-indexed #(when (str/includes? %2 "Type Error") %1) lines))
        ;; Find where "Execution error" starts  
        exec-error-idx (first (keep-indexed #(when (str/includes? %2 "Execution error") %1) lines))]
    (if type-error-idx
      ;; Extract from "Type Error" to just before "Execution error" or "in:"
      (let [end-idx (or (first (keep-indexed #(when (or (str/includes? %2 "Execution error")
                                                          (and (> %1 type-error-idx)
                                                               (str/starts-with? (str/trim %2) "in:")))
                                                     %1)
                                             lines))
                        (count lines))
            error-lines (subvec (vec lines) type-error-idx end-idx)]
        (str/join "\n;; " error-lines))
      ;; Fallback: take last few lines
      (str/join "\n;; " (take-last 5 lines)))))

(defn replace-placeholder
  "Replace TYPE-ERROR-PLACEHOLDER with actual error message"
  [content error-message]
  (str/replace content
               #";;\s*TYPE-ERROR-PLACEHOLDER"
               (str ";; " error-message)))

(defn update-metadata-in-content
  "Update metadata line in file content"
  [content new-metadata]
  (str/replace-first content
                     #";;\s*doc-test:.*"
                     (format-metadata new-metadata)))

(defn process-test-file
  "Process a single test file: run type checker, capture errors, update file"
  [file-path]
  (println "\n========================================")
  (println "Processing:" file-path)
  (println "========================================")
  
  (let [content (slurp file-path)
        metadata (parse-metadata content)]
    
    (if-not metadata
      (do
        (println "ERROR: No metadata found in file")
        false)
      
      (if-not (str/includes? content "TYPE-ERROR-PLACEHOLDER")
        (do
          (println "INFO: No TYPE-ERROR-PLACEHOLDER found, skipping")
          true)
        
        (do
          (println "Running type checker...")
          (let [result (run-type-check file-path)]
            (if (:success result)
              (do
                (println "WARNING: Type checking succeeded but placeholder exists")
                (println "File expects type error but none occurred")
                false)
              
              (do
                (println "Type error captured!")
                (let [error-msg (extract-error-message (:output result))
                      new-metadata (increment-version metadata)
                      updated-content (-> content
                                         (replace-placeholder error-msg)
                                         (update-metadata-in-content new-metadata))]
                  (println "\nUpdating file with:")
                  (println "  Version:" (:version metadata) "→" (:version new-metadata))
                  (println "  Error message:")
                  (println "    " (first (str/split-lines error-msg)))
                  (spit file-path updated-content)
                  (println "\n✓ File updated successfully")
                  true)))))))))

(defn -main [& args]
  (when (empty? args)
    (println "Usage: bb script/capture-type-errors.clj <test-file>")
    (System/exit 1))
  
  (let [file-path (first args)]
    (if-not (.exists (io/file file-path))
      (do
        (println "ERROR: File not found:" file-path)
        (System/exit 1))
      
      (if (process-test-file file-path)
        (do
          (println "\n✓ Processing complete")
          (println "  Next step: Run sync script to update markdown")
          (println "  bb script/sync-doc-tests.clj <markdown-file>")
          (System/exit 0))
        (do
          (println "\n✗ Processing failed")
          (System/exit 1))))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
