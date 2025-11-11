(ns ^:typed.clojure typed.clj.checker.test-utils
  (:require #?(:clj [babashka.fs :as fs])
            #?(:clj [clojure.edn :as edn])
            #?(:clj [clojure.java.io :as io])
            #?(:clj [clojure.pprint :as pp])
            #?(:clj [clojure.walk :as walk])
            [clojure.core.typed :as t]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :as test :refer [is]]
            [typed.clj.checker.check :as chk]
            [typed.clj.checker.parse-unparse :refer [parse-type parse-clj] :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.clj.runtime.env :as clj-env]
            [typed.cljc.checker.test-utils :as common-test]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.clj.checker.utils :refer [->opts]]))

(defn check-opt [opt]
  #_(assert (empty? (set/difference (set (keys opt))
                                    #{:expected :ret}))))

(def ^:dynamic *remove-nsyms* nil)

(defmacro with-delayed-remove-ns [& body]
  `(binding [*remove-nsyms* (atom #{})]
     (let [res# (do ~@body)]
       (run! remove-ns @*remove-nsyms*)
       res#)))

(defn tc-common* [frm {{:keys [syn provided?]} :expected-syntax :keys [expected-ret requires ns-meta check-config] :as opt}]
  (check-opt opt)
  (let [file *file*
        nsym (gensym 'clojure.core.typed.test.temp)
        ns-form 
        `(ns ~nsym
           ~@(when ns-meta [ns-meta])
           (:refer-clojure :exclude 
                           ~'[type defprotocol #_letfn fn loop let dotimes
                              #_def #_filter #_remove defn atom ref cast])
           (:require ~@'[;; TODO use typed.clojure
                         [clojure.core.typed :refer :all :as t]
                         [clojure.core.typed.unsafe :as unsafe]
                         [clojure.core :as core]
                         [clojure.core :as cc]]
                     ~@requires))]
    `(do
       (t/load-if-needed)
       (binding [*ns* *ns*
                 *file* ~file]
         (let [expected-ret# (clj ~expected-ret)
               check-config# ~check-config
               {ex# :ex} (t/check-form-info '~ns-form
                                            :check-config check-config#)
               _# (assert (nil? ex#) ex#)
               res# (t/check-form-info 
                      '~frm
                      :expected-ret expected-ret#
                      :expected '~syn
                      :type-provided? ~provided?
                      :check-config check-config#)]
           (or (some-> *remove-nsyms* (swap! conj '~nsym))
               (remove-ns '~nsym))
           res#)))))

(defmacro tc-e 
  "Type check an an expression in namespace that :refer's
  all of clojure.core.typed (aliased to t) and aliases clojure.core
  to core.

  Takes one form and then options, and returns true if the form checks
  with the expected input/output types according to the provided options.
  
  The first form in the options can be a static type syntax scoped
  in the new namespace. This is disambiguated with a call to keyword?
  (literal keywords aren't valid type syntax).
  
  eg. (tc-e (+ 1 1) Num)
      ;=> Num

  Keyword Options:

    :expected-ret An expected ret, evaluated in the current namespace (not the new
                  one that refers c.c.t). Cannot be provided in combination with the implicit
                  first option as a type, as above.
    :ret          Check the return TCResult of this expression against this ret. Evaluated
                  in the current namespace.
    :ns-meta      Map to use as ns metadata.
    :check-config Map specifying check-config."
  [frm & opts]
  (apply common-test/tc-e tc-common* frm opts))

(comment
  (tc-e 1)
  (tc-e 1 Num)
  (tc-e 1 Num :ret )
  (tc-err (fn [] (inc 'a)))
  (tc-err (fn [] (inc 1)))
  (tc-err (fn [] (inc 1)) Num)
  )

(defmacro tc-err [frm & opts]
  (apply common-test/tc-err tc-common* frm opts))

(defmacro is-tc-e [& body]
  `(test/is (do (tc-e ~@body)
                true)))

(defmacro is-tc-err
  "Returns the result of the tc-err call"
  [& body]
  (let [frm `(tc-err ~@body)]
    `(let [res# ~frm]
       (test/is res# (str '~frm))
       res#)))

(defn extract-error-messages [tc-err-res]
  (let [ungensym-ns (fn [msg] (str/replace msg #"clojure\.core\.typed\.test\.temp\d+/" "clojure.core.typed.temp/"))
        ;;TODO reparse line numbers in relation to is-tc-err-messages
        remove-line-number (fn [msg line] (str/replace msg (str ":" line ":") ":REMOVED_LINE:"))
        cp-relative-file (fn [msg line] (str/replace msg (str ":" line ":") ":REMOVED_LINE:"))
        mask-line-number (fn [m]
                           (let [line (-> m :env :line)
                                 file (-> m :env :file)
                                 file-name (if (= file "NO_SOURCE_FILE")
                                             file
                                             (some-> file #?(:clj fs/file-name :default str)))]
                             (cond-> m
                               (contains? m :env) (-> (update-in [:env :line] #(when (integer? %) "REMOVED_LINE"))
                                                      (assoc-in [:env :file] file-name))
                               (:message m) (-> (update :message remove-line-number line)
                                                (update :message ungensym-ns)
                                                (cond-> file (update :message str/replace file file-name))))))]
    (-> tc-err-res
        (update :type-errors #(mapv mask-line-number %))
        (cond->
          (:ex tc-err-res) (update :ex (fn [ex]
                                         (let [d (ex-data ex)
                                               _ (assert (not (contains? d :message)) d)]
                                           (mask-line-number
                                             (assoc d :message (ex-message ex))))))))))

(defmacro is-tc-err-messages
  "Performs an is-tc-err and returns error messages"
  [& body]
  `(extract-error-messages
     (is-tc-err ~@body)))

(defmacro throws-tc-error? [& body]
  `(err/with-ex-info-handlers
     [err/tc-error? (constantly true)]
     ~@body
     false))

(defmacro sub? [s t]
  `(let [opts# (assoc (clj-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns '~'clojure.core.typed)]
     (subtype? (parse-type '~s opts#)
               (parse-type '~t opts#))))

(defmacro sub?-q [s t]
  `(let [opts# (assoc (clj-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns '~'clojure.core.typed)]
     (subtype? (parse-type ~s opts#)
               (parse-type ~t opts#))))

(defn clj-opts []
  (-> (->opts)
      (assoc :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))
      (prs/with-unparse-ns (ns-name *ns*))))

(defn subtype? [s t]
  (sub/subtype? s t (clj-opts)))

(defn both-subtype? [s t]
  (and (not= r/-error s)
       (not= r/-error t)
       (subtype? s t)
       (subtype? t s)))

(defmacro both-sub? [s t]
  `(both-subtype? (parse-clj '~s)
                  (parse-clj '~t)))

(defn check [& as]
  (apply chk/check-expr as))

(defmacro is-cf [& args]
  `(is (do
         (t/cf ~@args)
         true)))

(defmacro is-clj [& args]
  `(clj (is ~@args)))

(defmacro cljs [& body]
  `(do ~@body))

(defmacro clj [& body]
  `(do ~@body))

;return ret for an expression f
(defmacro eret [f]
  `(let [ret# (-> ~(tc-common* f {}) :ret)]
     (assert (r/TCResult? ret#))
     ret#))

;return type for an expression f
(defmacro ety [f]
  `(-> (eret ~f) r/ret-t))

(defmacro caught-top-level-errors [nfn & body]
  `(err/with-ex-info-handlers
     [err/top-level-error? (fn [data# _#]
                             (~nfn (count (:errors data#))))]
     ~@body
     false))

(defmacro equal-types-noparse [l r]
  `(clj (is (let [l# (ety ~l)
                  r# ~r]
              (or (both-subtype? l# r#)
                  (do (println "Actual" l#)
                      (println "Expected" r#)
                      (println "In" (quote ~l))
                      nil))))))

(defmacro equal-types [l r]
  `(equal-types-noparse ~l (parse-type (quote ~r) (assoc (clj-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns '~'clojure.core.typed))))

(defmacro tc-t [form]
  `(let [{ex# :ex ret# :ret}
         ~(tc-common* form {})]
     (if ex#
       (throw ex#)
       ret#)))

(defmacro tc [form]
  `(t/check-form* '~form))

;; from clojure.test-helper
(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
   return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

;; from clojure.test-helper
(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
   (as identified by messages to *err*)."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer ~form))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer ~form))))))

;Aliases used in unit tests
(defmacro is-with-aliases [tst]
  `(do (t/check-ns '~'clojure.core.typed.test.util-aliases)
       (is-clj ~tst)))

;; ============================================================================
;; Documentation Testing Utilities
;; ============================================================================

#?(:clj
(do
  (defn parse-doc-test-metadata
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

  (defn format-doc-test-metadata
    "Format metadata for insertion into file.
     target: :markdown or :clojure"
    [metadata target]
    (let [pairs (str/join " "
                         (map (fn [[k v]] (str (name k) "=" v))
                              (sort-by first metadata)))]
      (case target
        :markdown (str "<!-- doc-test: " pairs " -->")
        :clojure (str ";; doc-test: " pairs))))

  (defn extract-between
    "Extract content between start and end delimiters in a vector of lines."
    [lines start end]
    (let [indexed (map-indexed vector lines)
          start-idx (some (fn [[i line]]
                           (when (str/includes? line start) i))
                         indexed)
          end-idx (some (fn [[i line]]
                         (when (str/includes? line end) i))
                       indexed)]
      (when (and start-idx end-idx)
        (cond
          (= start-idx end-idx)
          (let [line (nth lines start-idx)
                start-pos (+ (.indexOf line start) (count start))
                end-pos (.indexOf line end)]
            (when (< start-pos end-pos)
              (str/trim (subs line start-pos end-pos))))
          
          (and (= end-idx (inc start-idx))
               (str/includes? (nth lines end-idx) end))
          (let [end-line (nth lines end-idx)
                end-pos (.indexOf end-line end)]
            (str/trim (subs end-line 0 end-pos)))
          
          :else
          (let [result-start (inc start-idx)
                end-line (nth lines end-idx)
                end-on-own-line? (str/starts-with? (str/trim end-line) end)
                result-end (if end-on-own-line? (dec end-idx) end-idx)
                result-lines (if end-on-own-line?
                              (subvec (vec lines) result-start (inc result-end))
                              (let [lines-before (subvec (vec lines) result-start result-end)
                                    last-line-trimmed (subs end-line 0 (.indexOf end-line end))]
                                (conj lines-before last-line-trimmed)))]
            (str/trim (str/join "\n" result-lines)))))))

  (defn parse-doc-test-file
    "Parse a test file, extracting metadata, code, and expected result"
    [file-path]
    (let [content (slurp file-path)
          lines (vec (str/split-lines content))
          metadata-lines (take-while #(str/starts-with? % ";;") lines)
          metadata-text (str/join "\n" metadata-lines)
          metadata (parse-doc-test-metadata metadata-text)
          code (extract-between lines ";; start-markdown:" ";; end-markdown:")
          result-text (extract-between lines ";; start-result:" ";; end-result:")]
      {:metadata metadata
       :code code
       :result-text result-text
       :result (when result-text
                 (try
                   (let [trimmed (str/trim result-text)]
                     (if (str/starts-with? trimmed "(comment")
                       (let [content (subs trimmed 8)
                             content (str/trim content)
                             content (subs content 0 (dec (count content)))]
                         (edn/read-string content))
                       (edn/read-string trimmed)))
                   (catch Exception e
                     (println "Warning: Could not parse result as EDN:" (.getMessage e))
                     nil)))
       :file-path file-path}))

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

  (defn generate-doc-test-namespace
    "Generate a namespace name from doc name and id."
    [doc-name id]
    (str "typed-test.doc." doc-name "-" id))

  (defn generate-doc-test-file-content
    "Generate complete test file content"
    [metadata code result doc-name]
    (let [id (:id metadata)
          ns-name (generate-doc-test-namespace doc-name id)
          metadata-str (format-doc-test-metadata metadata :clojure)]
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

  (defn generate-uuid
    "Generate a short UUID for code block identification"
    []
    (let [uuid (str (java.util.UUID/randomUUID))]
      (subs uuid 0 8)))

  (defn doc-test-file-path
    "Generate test file path from metadata."
    [{:keys [id type] :as metadata} doc-name]
    (let [file-safe-doc-name (str/replace doc-name #"-" "_")
          filename (str file-safe-doc-name "_" id ".clj")]
      (str "typed/clj.checker/test/typed_test/doc/" filename)))

  (defn parse-code-blocks
    "Extract code blocks from markdown with their metadata."
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
              (and (not in-block?) (re-matches code-block-start line))
              (let [metadata (when preceding-line
                              (parse-doc-test-metadata preceding-line))]
                (recur (inc i)
                       true
                       {:metadata metadata
                        :line-start (inc i)
                        :metadata-line (when metadata (dec i))
                        :code []}
                       nil
                       blocks))
              
              (and in-block? (re-matches code-block-end line))
              (recur (inc i)
                     false
                     nil
                     nil
                     (conj blocks (assoc current-block
                                        :code (str/join "\n" (:code current-block))
                                        :line-end i)))
              
              in-block?
              (recur (inc i)
                     true
                     (update current-block :code conj line)
                     nil
                     blocks)
              
              :else
              (recur (inc i)
                     false
                     nil
                     line
                     blocks)))))))

  (defn extract-namespace-from-file
    "Extract namespace from file content"
    [content]
    (let [ns-match (re-find #"\(ns\s+(?:\^[^\s]+\s+)?([^\s)]+)" content)]
      (when ns-match (second ns-match))))))

  ;; Sync Functions
  (def test-dir "typed/clj.checker/test/typed_test/doc")
  (def default-type :success)

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
              test-path (doc-test-file-path new-metadata doc-name)
              test-content (generate-doc-test-file-content new-metadata (:code block) {} doc-name)]
          (io/make-parents test-path)
          (spit test-path test-content)
          (println "Created new test file:" test-path)
          (assoc block :metadata new-metadata :updated true))
        
        ;; Existing code block - check for sync
        :else
        (let [test-path (doc-test-file-path metadata doc-name)
              test-file (parse-doc-test-file test-path)]
          (if-not test-file
            (do
              (println "Warning: Test file not found for id" id "- creating it")
              (let [test-content (generate-doc-test-file-content metadata (:code block) {} doc-name)]
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
                  (spit test-path (generate-doc-test-file-content metadata doc-code (:result test-file) doc-name))
                  nil)
                
                ;; Test version is newer - update markdown
                (= version-cmp :right-newer)
                (do
                  (println "Updating markdown code block" id "from test file (v" doc-version "→" test-version ")")
                  (assoc block
                         :metadata (:metadata test-file)
                         :code (:code test-file)
                         :updated true)))))))))

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
                      metadata-str (format-doc-test-metadata (:metadata block) :markdown)]
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

  (defn sync-all-docs
    "Sync all documentation files"
    []
    (println "Syncing all documentation files...")
    (doseq [f (file-seq (io/file "website/docs"))
            :when (and (.isFile f)
                      (str/ends-with? (.getName f) ".md"))]
      (try
        (sync-doc-file (.getPath f))
        (catch Exception e
          (println "Error syncing" (.getPath f) ":" (.getMessage e))))))
