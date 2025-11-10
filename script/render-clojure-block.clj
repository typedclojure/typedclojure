#!/usr/bin/env bb
;; Render Clojure code block with type error annotations
;;
;; This script is called by the MkDocs typedclojure_blocks plugin
;; to generate HTML with error highlighting for documentation.
;;
;; Usage:
;;   bb script/render-clojure-block.clj <markdown-file-path> <doc-test-id>

(ns render-clojure-block
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.edn :as edn]))

;; Load common utilities
(load-file "script/doc-test-common.clj")
(require '[doc-test-common :as common])

(def test-dir "typed/clj.checker/test/typed_test/doc")

;; ============================================================================
;; Test File Location
;; ============================================================================

(defn find-test-file
  "Find test file by doc-test-id.
   Returns file path if found, nil otherwise."
  [doc-test-id]
  (let [dir (io/file test-dir)]
    (when (.exists dir)
      (first
        (filter
          (fn [f]
            (and (.isFile f)
                 (str/ends-with? (.getName f) (str "_" doc-test-id ".clj"))))
          (file-seq dir))))))

;; ============================================================================
;; Error Line Mapping
;; ============================================================================

(defn extract-markdown-code
  "Extract code from markdown file for a specific doc-test-id.
   Returns the code as a string."
  [markdown-file-path doc-test-id]
  (let [content (slurp markdown-file-path)
        ;; Pattern to find code block with matching doc-test id
        pattern (re-pattern
                  (str "<!--\\s*doc-test:.*?id=" doc-test-id ".*?-->\\s*```clojure\\s*([\\s\\S]*?)```"))
        match (re-find pattern content)]
    (when match
      (str/trim (second match)))))

(defn map-error-line-to-markdown
  "Map error line number from test file to markdown code block line number.
   The markdown code starts at line 1, while in the test file it starts after
   the namespace declaration and ;; start-markdown: delimiter."
  [test-file-content error-line]
  (let [lines (str/split-lines test-file-content)
        ;; Find the line number where markdown content starts
        markdown-start-idx (some
                             (fn [[idx line]]
                               (when (str/includes? line ";; start-markdown:")
                                 (inc idx)))
                             (map-indexed vector lines))]
    (when markdown-start-idx
      ;; Calculate the line number relative to markdown block (1-based)
      (- error-line markdown-start-idx))))

;; ============================================================================
;; HTML Generation
;; ============================================================================

(defn escape-html
  "Escape HTML special characters"
  [s]
  (-> s
      (str/replace "&" "&amp;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")
      (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn find-form-on-line
  "Find the form on a specific line in the code.
   Returns a map with :start-col, :end-col, and :form-text"
  [code-lines line-num column]
  (when (and (pos? line-num) (<= line-num (count code-lines)))
    (let [line (nth code-lines (dec line-num))]
      ;; For now, just return the whole line as the form
      ;; A more sophisticated parser could extract the exact form
      {:start-col 0
       :end-col (count line)
       :form-text line})))

(defn generate-line-html
  "Generate HTML for a single line of code with optional error highlighting"
  [line-text line-num errors-by-line]
  (let [errors (get errors-by-line line-num)
        escaped-line (escape-html line-text)]
    (if (seq errors)
      (let [error (first errors) ; For now, handle first error on line
            message (escape-html (:message error))]
        (str "    <span class=\"clj-line type-error\">\n"
             "      <span class=\"type-error-underline\" title=\"" message "\">\n"
             "        " escaped-line "\n"
             "      </span>\n"
             "    </span>"))
      (str "    <span class=\"clj-line\">" escaped-line "</span>"))))

(defn generate-html-output
  "Generate complete HTML output for a code block with error annotations"
  [doc-test-id code-lines errors test-file-path]
  (let [;; Map errors to markdown line numbers
        test-file-content (slurp test-file-path)
        errors-with-mapped-lines
        (map
          (fn [error]
            (let [test-line (get-in error [:env :line])
                  markdown-line (map-error-line-to-markdown test-file-content test-line)]
              (assoc error :markdown-line markdown-line)))
          errors)
        ;; Group errors by markdown line number
        errors-by-line (group-by :markdown-line errors-with-mapped-lines)
        ;; Remove nil line numbers
        errors-by-line (dissoc errors-by-line nil)
        ;; Generate HTML for each line
        lines-html (map-indexed
                     (fn [idx line]
                       (generate-line-html line (inc idx) errors-by-line))
                     code-lines)]
    (str "<div class=\"clojure-block\" data-doc-test-id=\"" doc-test-id "\">\n"
         "  <pre><code>\n"
         (str/join "\n" lines-html) "\n"
         "  </code></pre>\n"
         "</div>")))

;; ============================================================================
;; Main Entry Point
;; ============================================================================

(defn render-block
  "Main rendering function"
  [markdown-file-path doc-test-id]
  (let [test-file (find-test-file doc-test-id)]
    (if-not test-file
      (do
        (binding [*out* *err*]
          (println "ERROR: Test file not found for doc-test-id:" doc-test-id))
        (System/exit 1))
      (let [parsed (common/parse-test-file (.getPath test-file))
            result (:result parsed)
            type-errors (get result :type-errors [])
            markdown-code (extract-markdown-code markdown-file-path doc-test-id)
            code-lines (str/split-lines markdown-code)
            html (generate-html-output doc-test-id code-lines type-errors test-file)]
        (println html)))))

(defn -main [& args]
  (if (not= (count args) 2)
    (do
      (binding [*out* *err*]
        (println "Usage: bb script/render-clojure-block.clj <markdown-file-path> <doc-test-id>"))
      (System/exit 1))
    (let [[markdown-file-path doc-test-id] args]
      (render-block markdown-file-path doc-test-id))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
