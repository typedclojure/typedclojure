(ns typed.fnl.analyzer-test
  "Test suite for Fennel analyzer following typed.clj.analyzer patterns."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [typed.fnl.analyzer :as fnl-ana]
            [typed.fnl.reader :as fnl-reader]
            [typed.cljc.analyzer :as ana]
            [typed.cljc.analyzer.env :as env]))

(defn analyze-str
  "Analyze a Fennel form from a string."
  [source]
  (let [form (fnl-reader/read-string source)
        opts (env/ensure (fnl-ana/default-opts) (atom {}))]
    (fnl-ana/analyze form (fnl-ana/empty-env) opts)))

(deftest test-const-values
  (testing "Constant values"
    (doseq [s ["42"
               "\"hello\""
               "true"
               "false"
               "nil"]]
      (testing (pr-str s)
        (is (= :const (:op (analyze-str s))))))))

(deftest test-do-form
  (testing "'do' form reuses Clojure parser"
    (let [ast (analyze-str "(do 1 2 3)")]
      (is (= :do (:op ast)))
      (is (vector? (:statements ast)))
      (is (= 2 (count (:statements ast))))
      (is (map? (:ret ast))))))

(deftest test-let-form
  (testing "Non-destructuring 'let' form"
    (let [ast (analyze-str "(let [x 1] x)")]
      (is (= :let (:op ast)) (:form ast))
      (is (vector? (:bindings ast)))
      (is (= 1 (count (:bindings ast))))
      (is (map? (:body ast))))))

(deftest test-local-form
  (testing "'local' form creates binding"
    (let [ast (analyze-str "(local x 1)")]
      (is (= :local (:op ast)))
      (is (symbol? (:name ast)))
      (is (map? (:init ast))))))

(deftest test-var-form
  (testing "'var' form creates mutable binding"
    (let [ast (analyze-str "(var x 1)")]
      (is (= :var (:op ast)))
      (is (symbol? (:name ast)))
      (is (map? (:init ast))))))

(deftest test-values-form
  (testing "'values' form for multiple returns"
    (let [ast (analyze-str "(values 1 2 3)")]
      (is (= :values (:op ast)))
      (is (vector? (:exprs ast)))
      (is (= 3 (count (:exprs ast)))))))

(deftest test-lua-form
  (testing "'lua' form for raw Lua code"
    (let [ast (analyze-str "(lua \"return 42\")")]
      (is (= :lua (:op ast)))
      (is (string? (:code ast))))))

(deftest test-comment-form
  (testing "'comment' form"
    (let [ast (analyze-str "(comment 1 2 3)")]
      (is (= :const (:op ast)))
      (is (= nil (:val ast))))))

(deftest test-or-form
  (testing "'or' form"
    (let [ast (analyze-str "(or false 42)")]
      (is (= :or (:op ast)))
      (is (vector? (:exprs ast)))
      (is (= 2 (count (:exprs ast)))))))

(deftest test-and-form
  (testing "'and' form"
    (let [ast (analyze-str "(and true 42)")]
      (is (= :and (:op ast)))
      (is (vector? (:exprs ast)))
      (is (= 2 (count (:exprs ast)))))))

(deftest test-quote-form
  (testing "'quote' form"
    (let [ast (analyze-str "(quote x)")]
      (is (= :quote (:op ast)))
      (is (= 'x (:form (:expr ast)))))))

(deftest test-include-form
  (testing "'include' form loads Fennel module"
    (let [ast (analyze-str "(include :some.module)")]
      (is (= :include (:op ast)))
      (is (string? (:module ast))))))

(deftest test-require-macros-form
  (testing "'require-macros' form imports macros"
    (let [ast (analyze-str "(require-macros :my.macros)")]
      (is (= :require-macros (:op ast)))
      (is (= ["my.macros"] (:args ast))))))

#_ ;;TODO
(deftest test-unquote-form
  (testing "'unquote' form inside quasiquote"
    (let [ast (analyze-str ",x")]
      (is (= :unquote (:op ast)))
      (is (symbol? (:expr ast))))))

(deftest test-macroexpand-1
  (testing "macroexpand-1 expands ann-forwarded macro using file scope"
    (let [fennel-file-repo-root-relative "example-projects/fennel/src/typed-fennel-demo.fnl"
          fennel-file (-> (let [f (io/file fennel-file-repo-root-relative)]
                            (if (.exists f)
                              f
                              ;; via subproject tests
                              (io/file "../.." fennel-file-repo-root-relative)))
                          .getCanonicalPath)
          form '(ann-forwarded demo-func t/Str)
          ;; Use opts-with-file to automatically populate macros from the file
          opts (env/ensure (fnl-ana/opts-with-file fennel-file (fnl-ana/default-opts))
                           (atom {}))
          expanded (fnl-ana/macroexpand-1 form (fnl-ana/empty-env) opts)]
      ;; Should expand to (t.ann-form demo-func [t/Str :-> t/Str])
      (is (seq? expanded))
      (is (= 't.ann-form (first expanded)))
      (is (= 'demo-func (second expanded)))))
  
  (testing "macroexpand-1 returns non-macro forms unchanged"
    (let [form '(some-func x y)
          opts (env/ensure (fnl-ana/default-opts) (atom {}))
          expanded (fnl-ana/macroexpand-1 form (fnl-ana/empty-env) opts)]
      (is (identical? form expanded))))
  
  (testing "macroexpand-1 expands core Fennel macro icollect"
    (let [;; Create a temp file with icollect
          temp-file (java.io.File/createTempFile "test_icollect" ".fnl")
          _ (spit temp-file "(icollect [i x (ipairs [1 2 3])] (* x x))")
          fennel-file (.getAbsolutePath temp-file)
          form '(icollect [i x (ipairs [1 2 3])] (* x x))
          ;; Use opts-with-file to automatically populate macros from the file
          opts (env/ensure (fnl-ana/opts-with-file fennel-file (fnl-ana/default-opts))
                           (atom {}))
          expanded (fnl-ana/macroexpand-1 form (fnl-ana/empty-env) opts)]
      ;; icollect should expand to a let form
      (is (seq? expanded))
      (is (= 'let (first expanded)))
      (.delete temp-file))))
