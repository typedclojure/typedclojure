#!/usr/bin/env bb
;; Common utilities for documentation testing scripts

(ns doc-test-common
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [clojure.walk :as walk]
            [clojure.pprint :as pp]))

;; ============================================================================
;; Metadata Parsing
;; ============================================================================

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
;; Test File Parsing
;; ============================================================================

(defn extract-between
  "Extract content between start and end delimiters in a vector of lines.
   Handles cases where end delimiter is on same line as content."
  [lines start end]
  (let [indexed (map-indexed vector lines)
        start-idx (some (fn [[i line]]
                         (when (str/includes? line start)
                           i))
                       indexed)
        end-idx (some (fn [[i line]]
                       (when (str/includes? line end)
                         i))
                     indexed)]
    (when (and start-idx end-idx)
      (cond
        ;; Start and end on same line
        (= start-idx end-idx)
        (let [line (nth lines start-idx)
              start-pos (+ (.indexOf line start) (count start))
              end-pos (.indexOf line end)]
          (when (< start-pos end-pos)
            (str/trim (subs line start-pos end-pos))))
        
        ;; End delimiter on same line as last content
        (and (= end-idx (inc start-idx))
             (str/includes? (nth lines end-idx) end))
        (let [end-line (nth lines end-idx)
              end-pos (.indexOf end-line end)]
          (str/trim (subs end-line 0 end-pos)))
        
        ;; Multi-line case
        :else
        (let [result-start (inc start-idx)
              ;; Check if end delimiter is on its own line or with content
              end-line (nth lines end-idx)
              end-on-own-line? (str/starts-with? (str/trim end-line) end)
              result-end (if end-on-own-line?
                          (dec end-idx)
                          end-idx)
              result-lines (if end-on-own-line?
                            (subvec (vec lines) result-start (inc result-end))
                            (let [lines-before (subvec (vec lines) result-start result-end)
                                  last-line-trimmed (subs end-line 0 (.indexOf end-line end))]
                              (conj lines-before last-line-trimmed)))]
          (str/trim (str/join "\n" result-lines)))))))

(defn parse-test-file
  "Parse a test file, extracting metadata, code, and expected result"
  [file-path]
  (let [content (slurp file-path)
        lines (vec (str/split-lines content))
        ;; Extract metadata from initial comments
        metadata-lines (take-while #(str/starts-with? % ";;") lines)
        metadata-text (str/join "\n" metadata-lines)
        metadata (parse-metadata metadata-text)
        code (extract-between lines ";; start-markdown:" ";; end-markdown:")
        result-text (extract-between lines ";; start-result:" ";; end-result:")]
    {:metadata metadata
     :code code
     :result-text result-text
     :result (when result-text
               (try
                 ;; Extract the content inside (comment ...)
                 (let [trimmed (str/trim result-text)]
                   (if (str/starts-with? trimmed "(comment")
                     (let [content (subs trimmed 8) ; Remove "(comment"
                           content (str/trim content)
                           content (subs content 0 (dec (count content)))] ; Remove trailing ")"
                       (edn/read-string content))
                     (edn/read-string trimmed)))
                 (catch Exception e
                   (println "Warning: Could not parse result as EDN:" (.getMessage e))
                   nil)))
     :file-path file-path}))

;; ============================================================================
;; Deterministic Pretty Printing
;; ============================================================================

(defn sort-data-structure
  "Recursively sort maps and sets for deterministic output"
  [data]
  (walk/postwalk
    (fn [v]
      (cond
        (map? v) (into (sorted-map) v)
        (set? v) (into (sorted-set) v)
        :else v))
    data))

(defn pprint-str
  "Pretty-print data structure to string with deterministic sorting"
  [data]
  (with-out-str
    (pp/pprint (sort-data-structure data))))

;; ============================================================================
;; Test File Generation
;; ============================================================================

(defn generate-test-namespace
  "Generate a namespace name from doc name and id.
   Namespace uses hyphens (Clojure convention)."
  [doc-name id]
  (str "typed-test.doc." doc-name "-" id))

(defn generate-test-file-content
  "Generate complete test file content"
  [metadata code result doc-name]
  (let [id (:id metadata)
        ns-name (generate-test-namespace doc-name id)
        metadata-str (format-metadata metadata :clojure)]
    (str metadata-str "\n"
         "(ns ^:typed.clojure " ns-name "\n"
         "  (:require [typed.clojure :as t]))\n"
         "\n"
         ";; start-markdown:\n"
         code "\n"
         ";; end-markdown:\n"
         "\n"
         ";; start-result:\n"
         (format "(comment\n%s)\n" (pprint-str result))
         ";; end-result:\n")))
