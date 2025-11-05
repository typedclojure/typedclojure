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

(ns sync-doc-tests
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; ============================================================================
;; Configuration
;; ============================================================================

(def test-dir "typed/clj.checker/test/typed_test/doc")

(def default-type :success)

;; ============================================================================
;; UUID and Version Management
;; ============================================================================

(defn generate-uuid
  "Generate a short UUID for code block identification"
  []
  (let [uuid (str (java.util.UUID/randomUUID))]
    ;; Use first 8 chars for readability
    (subs uuid 0 8)))

(defn parse-metadata
  "Parse metadata from comment lines.
   Format: <!-- doc-test: key=value key=value -->
   or:     ;; doc-test: key=value key=value"
  [text]
  (let [patterns [#"<!--\s*doc-test:\s*([^>]+)\s*-->"
                  #";;\s*doc-test:\s*(.+)"]
        matches (some #(re-find % text) patterns)]
    (when matches
      (let [content (second matches)
            pairs (re-seq #"(\w+)=([^\s]+)" content)]
        (into {} (map (fn [[_ k v]] [(keyword k) v]) pairs))))))

(defn format-metadata
  "Format metadata for insertion into file.
   target: :markdown or :clojure"
  [metadata target]
  (let [pairs (str/join " "
                       (map (fn [[k v]] (str (name k) "=" v))
                            (sort-by first metadata)))]
    (case target
      :markdown (str "<!-- doc-test: " pairs " -->")
      :clojure (str ";; doc-test: " pairs))))

;; ============================================================================
;; Code Block Parsing
;; ============================================================================

(defn parse-code-blocks
  "Extract code blocks from markdown with their metadata.
   Returns seq of maps with :metadata, :code, :line-start, :line-end"
  [markdown-content]
  (let [lines (str/split-lines markdown-content)
        code-block-start #"^```clojure\s*$"
        code-block-end #"^```\s*$"]
    (loop [i 0
           in-block? false
           current-block nil
           preceding-line nil
           blocks []]
      (if (>= i (count lines))
        blocks
        (let [line (nth lines i)]
          (cond
            ;; Start of code block - check for metadata in preceding line
            (and (not in-block?) (re-matches code-block-start line))
            (let [metadata (when preceding-line
                            (parse-metadata preceding-line))]
              (recur (inc i)
                     true
                     {:metadata metadata
                      :line-start (inc i)
                      :metadata-line (when metadata (dec i))
                      :code []}
                     nil
                     blocks))
            
            ;; End of code block
            (and in-block? (re-matches code-block-end line))
            (recur (inc i)
                   false
                   nil
                   nil
                   (conj blocks (assoc current-block
                                      :code (str/join "\n" (:code current-block))
                                      :line-end i)))
            
            ;; Inside code block - collect code
            in-block?
            (recur (inc i)
                   true
                   (update current-block :code conj line)
                   nil
                   blocks)
            
            ;; Outside code block - track line for potential metadata
            :else
            (recur (inc i)
                   false
                   nil
                   line
                   blocks)))))))

;; ============================================================================
;; Test File Management
;; ============================================================================

(defn test-file-path
  "Generate test file path from metadata.
   Converts namespace-style names (with hyphens) to file paths (with underscores)
   following Clojure's namespace resolution rules."
  [{:keys [id type] :as metadata} doc-name]
  (let [;; Convert hyphens to underscores for file path
        file-safe-doc-name (str/replace doc-name #"-" "_")
        filename (str file-safe-doc-name "_" id ".clj")]
    (str test-dir "/" filename)))

(defn parse-test-file
  "Parse a test file, extracting metadata and code"
  [file-path]
  (when (.exists (io/file file-path))
    (let [content (slurp file-path)
          lines (str/split-lines content)
          ;; Extract metadata from initial comments
          metadata-lines (take-while #(str/starts-with? % ";;") lines)
          metadata-text (str/join "\n" metadata-lines)
          metadata (parse-metadata metadata-text)
          ;; Everything after metadata is the test code
          code-start (count metadata-lines)
          code-lines (drop code-start lines)
          code (str/join "\n" code-lines)]
      {:metadata metadata
       :code (str/trim code)
       :file-path file-path})))

(defn generate-test-namespace
  "Generate a namespace name from doc name and id.
   Namespace uses hyphens (Clojure convention)."
  [doc-name id]
  (str "typed-test.doc." doc-name "-" id))

(defn generate-test-file-content
  "Generate complete test file content"
  [metadata code doc-name]
  (let [id (:id metadata)
        ns-name (generate-test-namespace doc-name id)
        metadata-str (format-metadata metadata :clojure)]
    (str metadata-str "\n"
         "(ns ^:typed.clojure " ns-name "\n"
         "  (:require [typed.clojure :as t]))\n"
         "\n"
         code "\n")))

;; ============================================================================
;; Synchronization Logic
;; ============================================================================

(defn compare-versions
  "Compare two version strings. Returns :equal, :left-newer, or :right-newer"
  [v1 v2]
  (let [n1 (parse-long (or v1 "1"))
        n2 (parse-long (or v2 "1"))]
    (cond
      (= n1 n2) :equal
      (> n1 n2) :left-newer
      :else :right-newer)))

(defn sync-code-block
  "Synchronize a single code block with its test file.
   Returns updated block or nil if no changes needed."
  [block doc-name]
  (let [metadata (:metadata block)
        id (:id metadata)]
    (cond
      ;; New code block - generate UUID and create test file
      (nil? id)
      (let [new-id (generate-uuid)
            new-metadata (assoc metadata
                               :id new-id
                               :version "1"
                               :type (name default-type))
            test-path (test-file-path new-metadata doc-name)
            test-content (generate-test-file-content new-metadata (:code block) doc-name)]
        (io/make-parents test-path)
        (spit test-path test-content)
        (println "Created new test file:" test-path)
        (assoc block :metadata new-metadata :updated true))
      
      ;; Existing code block - check for sync
      :else
      (let [test-path (test-file-path metadata doc-name)
            test-file (parse-test-file test-path)]
        (if-not test-file
          (do
            (println "Warning: Test file not found for id" id "- creating it")
            (let [test-content (generate-test-file-content metadata (:code block) doc-name)]
              (io/make-parents test-path)
              (spit test-path test-content)
              (assoc block :updated true)))
          
          ;; Both exist - check versions and content
          (let [doc-version (get metadata :version "1")
                test-version (get-in test-file [:metadata :version] "1")
                doc-code (str/trim (:code block))
                test-code (str/trim (:code test-file))
                codes-match? (= doc-code test-code)
                version-cmp (compare-versions doc-version test-version)]
            
            (cond
              ;; Codes match - no sync needed
              codes-match?
              (do
                (println "✓ Code block" id "in sync")
                nil)
              
              ;; Same version but different content - ERROR
              (= version-cmp :equal)
              (do
                (println "ERROR: Version conflict for code block" id)
                (println "  Both have version" doc-version "but different content")
                (println "  Update version in either markdown or test file to resolve")
                (throw (ex-info "Version conflict"
                               {:id id
                                :version doc-version
                                :doc-file doc-name
                                :test-file test-path})))
              
              ;; Doc version is newer - update test file
              (= version-cmp :left-newer)
              (do
                (println "Updating test file" test-path "from markdown (v" test-version "→" doc-version ")")
                (spit test-path (generate-test-file-content metadata doc-code doc-name))
                nil)
              
              ;; Test version is newer - update markdown
              (= version-cmp :right-newer)
              (do
                (println "Updating markdown code block" id "from test file (v" doc-version "→" test-version ")")
                (assoc block
                       :metadata (:metadata test-file)
                       :code (:code test-file)
                       :updated true)))))))))

;; ============================================================================
;; Markdown Update
;; ============================================================================

(defn update-markdown
  "Update markdown content with synchronized code blocks"
  [original-content blocks]
  (let [lines (str/split-lines original-content)
        updates (into {} (map (fn [b] [(:line-start b) b])
                             (filter :updated blocks)))]
    (if (empty? updates)
      original-content
      (loop [i 0
             result []
             in-update nil]
        (if (>= i (count lines))
          (str/join "\n" result)
          (let [line (nth lines i)]
            (cond
              ;; Start of updated block
              (contains? updates (inc i))
              (let [block (get updates (inc i))
                    metadata-str (format-metadata (:metadata block) :markdown)]
                (recur (inc i)
                       (-> result
                           (conj metadata-str)
                           (conj line))
                       block))
              
              ;; Inside updated block - at first code line, replace all code
              (and in-update (= i (:line-start in-update)))
              ;; Insert new code and jump to closing ``` line
              (recur (:line-end in-update)
                     (into result (str/split-lines (:code in-update)))
                     nil)
              
              ;; Inside updated block - skip old code lines
              (and in-update
                   (> i (:line-start in-update))
                   (< i (:line-end in-update)))
              (recur (inc i) result in-update)
              
              ;; Normal line
              :else
              (recur (inc i)
                     (conj result line)
                     nil))))))))

;; ============================================================================
;; Main Entry Point
;; ============================================================================

(defn sync-doc-file
  "Synchronize a single documentation file"
  [doc-file]
  (println "\n========================================")
  (println "Syncing:" doc-file)
  (println "========================================")
  (let [content (slurp doc-file)
        doc-name (-> doc-file
                    (str/replace #".*/" "")
                    (str/replace #"\.md$" "")
                    (str/replace #"[^a-zA-Z0-9_-]" "-"))
        blocks (parse-code-blocks content)
        _ (println "Found" (count blocks) "code blocks")
        synced-blocks (keep #(sync-code-block % doc-name) blocks)]
    
    (when (seq synced-blocks)
      (let [updated-content (update-markdown content synced-blocks)]
        (spit doc-file updated-content)
        (println "\nUpdated markdown file:" doc-file)))
    
    (println "✓ Sync complete for" doc-file)))

(defn -main [& args]
  (cond
    (empty? args)
    (do
      (println "Usage:")
      (println "  bb script/sync-doc-tests.clj <markdown-file>")
      (println "  bb script/sync-doc-tests.clj --all")
      (System/exit 1))
    
    (= "--all" (first args))
    (do
      (println "Syncing all documentation files...")
      (doseq [f (file-seq (io/file "website/docs"))
              :when (and (.isFile f)
                        (str/ends-with? (.getName f) ".md"))]
        (try
          (sync-doc-file (.getPath f))
          (catch Exception e
            (println "Error syncing" (.getPath f) ":" (.getMessage e))))))
    
    :else
    (doseq [doc-file args]
      (sync-doc-file doc-file))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
