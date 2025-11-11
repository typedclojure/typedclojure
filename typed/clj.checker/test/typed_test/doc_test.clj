(ns ^:typed.clojure typed-test.doc-test
  (:require [clojure.core.typed :as t]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.walk :as walk]
            [typed.clj.checker.test-utils :as test-utils]))

(def test-dir 
  (let [user-dir (System/getProperty "user.dir")]
    (if (str/ends-with? user-dir "/typed/clj.checker")
      (str user-dir "/test/typed_test/doc")
      (str user-dir "/typed/clj.checker/test/typed_test/doc"))))

(def ^:dynamic *sync-to-disk* 
  "JVM property to control whether to sync test results to disk.
   When true, outputs check-ns-info results to comment block and test passes if type checking passes/fails as annotated.
   When false (default), compares actual result to embedded result and fails if not equal."
  (Boolean/parseBoolean (System/getProperty "typed.clojure.doc-test.sync" "false")))

(defn normalize-file-paths
  "Normalize file paths in check-ns-info result to be repo-root relative.
   This makes the results reproducible across different environments."
  [result]
  (let [user-dir (System/getProperty "user.dir")
        repo-root (if (str/ends-with? user-dir "/typed/clj.checker")
                    (str/replace user-dir #"/typed/clj\.checker$" "")
                    user-dir)]
    (walk/postwalk
      (fn [v]
        (if (and (string? v) (str/starts-with? v "file:"))
          (let [path (subs v 5)] ; Remove "file:" prefix
            (if (str/starts-with? path repo-root)
              (str "file:" (subs path (inc (count repo-root)))) ; +1 for trailing slash
              v))
          v))
      result)))

(defn run-type-check-on-file
  "Run type checking on a namespace file.
   Returns the check-ns-info result map"
  [file-path]
  (try
    (let [content (slurp file-path)
          ns-name (test-utils/extract-namespace-from-file content)
          _ (when-not ns-name
              (throw (ex-info "Could not extract namespace from file" {:file file-path})))
          ;; Load the namespace to ensure it's available for checking
          _ (load-file file-path)
          ;; Run type checking using check-ns-info
          check-result (t/check-ns-info (symbol ns-name) :max-parallelism 1)
          ;; Normalize file paths to be repo-root relative
          normalized-result (normalize-file-paths check-result)]
      normalized-result)
    (catch Exception e
      {:error (.getMessage e)
       :ex e})))

(defn test-doc-test-file
  "Test a single documentation test file.
   Returns a map with test results."
  [file-path]
  (let [parsed (test-utils/parse-doc-test-file file-path)
        metadata (:metadata parsed)
        expected-type (keyword (get metadata :type "success"))
        expected-result (:result parsed)
        actual-result (run-type-check-on-file file-path)]
    
    (if (:error actual-result)
      {:file file-path
       :passed false
       :error true
       :error-message (:error actual-result)
       :metadata metadata}
      (let [success? (empty? (:type-errors actual-result))
            type-matches? (= success? (= expected-type :success))
            ;; Compare actual result with expected result from comment block
            results-match? (if expected-result
                            (= (test-utils/sort-data-structure expected-result)
                               (test-utils/sort-data-structure actual-result))
                            true) ; If no expected result, we only check type
            
            ;; In sync mode, update the file if type check matches expectation
            _ (when (and *sync-to-disk* type-matches?)
                (let [doc-name (-> (.getName (io/file file-path))
                                  (str/replace #"_[a-f0-9]{8}\.clj$" "")
                                  (str/replace #"_" "-"))
                      new-content (test-utils/generate-doc-test-file-content 
                                   metadata 
                                   (:code parsed) 
                                   actual-result 
                                   doc-name)]
                  (spit file-path new-content)))
            
            ;; In sync mode, test passes if type matches; otherwise check result too
            passed? (if *sync-to-disk*
                     type-matches?
                     (and type-matches? results-match?))]
        
        {:file file-path
         :expected-type expected-type
         :actual-type (if success? :success :fail)
         :type-matches type-matches?
         :results-match results-match?
         :passed passed?
         :metadata metadata
         :actual-result actual-result}))))

(defn ensure-doc-test-up-to-date
  "Ensure that a doc test file's markdown section matches the expected format.
   Returns true if up-to-date, false otherwise."
  [file-path]
  (let [parsed (test-utils/parse-doc-test-file file-path)
        metadata (:metadata parsed)
        code (:code parsed)]
    (and (some? metadata)
         (some? code)
         (contains? metadata :id)
         (contains? metadata :version)
         (contains? metadata :type))))

(deftest doc-tests
  "Test all documentation test files.
   For each file in typed_test/doc:
   1. Ensure the test namespace is up-to-date with its testing block
   2. Run type checker on the doc test
   3. Assert expected result matches check-ns-info output"
  (let [dir (io/file test-dir)
        test-files (if (.exists dir)
                    (->> (file-seq dir)
                         (filter #(and (.isFile %)
                                     (str/ends-with? (.getName %) ".clj")))
                         (map #(.getPath %))
                         sort)
                    [])]
    
    (when (empty? test-files)
      (println "Warning: No doc test files found in" test-dir))
    
    (doseq [file-path test-files]
      (testing (str "Doc test: " (.getName (io/file file-path)))
        ;; First ensure the file is up-to-date
        (is (ensure-doc-test-up-to-date file-path)
            (str "Doc test file should be up-to-date: " file-path))
        
        ;; Then run the actual type check test
        (let [result (test-doc-test-file file-path)]
          (when (:error result)
            (println "\nError in" file-path ":" (:error-message result)))
          
          (when-not (:type-matches result)
            (println "\nType mismatch in" file-path)
            (println "  Expected:" (:expected-type result))
            (println "  Actual:" (:actual-type result)))
          
          (when (and (not *sync-to-disk*) 
                     (not (:results-match result))
                     (some? (get-in result [:metadata :result])))
            (println "\nResult mismatch in" file-path)
            (println "  Run with -Dtyped.clojure.doc-test.sync=true to update"))
          
          (is (:passed result)
              (str "Doc test should pass: " file-path
                   (when (:error result) 
                     (str " (error: " (:error-message result) ")"))
                   (when-not (:type-matches result)
                     (str " (type mismatch: expected " (:expected-type result) 
                          " but got " (:actual-type result) ")"))
                   (when-not (:results-match result)
                     " (result mismatch)"))))))))
