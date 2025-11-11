(ns typed.fnl.checker-test
  "End-to-end test suite for Fennel type checker"
  (:require [clojure.test :refer :all]
            [clojure.core.typed.current-impl :as impl]
            [typed.fnl.reader :as fnl-reader]
            [typed.fnl.analyzer :as fnl-ana]
            [typed.clj.analyzer :as clj-ana]
            [typed.fnl.checker.check-form :as typed.fnl.checker]
            [typed.fnl.checker.utils :as fnl-utils]))

(deftest test-simple-string-literal
  (testing "Read and parse a simple string literal"
    (let [fennel-src "\"Hello, world!\""
          form (fnl-reader/read-string fennel-src)]
      (is (= "Hello, world!" form))
      (is (string? form)))))

(deftest test-simple-number-literal
  (testing "Read and parse a simple number literal"
    (let [fennel-src "42"
          form (fnl-reader/read-string fennel-src)]
      (is (= 42 form))
      (is (number? form)))))

(deftest test-simple-fn
  (testing "Read and parse a simple function"
    (let [fennel-src "(fn [x] x)"
          form (fnl-reader/read-string fennel-src)]
      (is (= '(fn [x] x) form))
      (is (seq? form))
      (is (= 'fn (first form))))))

(deftest test-unanalyzed
  (testing "Create unanalyzed AST nodes from Fennel forms"
    (let [form "Hello, world!"
          ;; Use clj analyzer's environment setup
          env (clj-ana/empty-env 'user)
          ;; Create minimal opts with required passes
          opts (clj-ana/default-opts)
          ast (fnl-ana/unanalyzed form env opts)]
      (is (map? ast))
      (is (= :unanalyzed (:op ast)))
      (is (= "Hello, world!" (:form ast))))))

(deftest test-complex-fennel-form
  (testing "Create unanalyzed AST for complex Fennel form"
    (let [fennel-src "(fn demo-func [x y] (if (> x y) x y))"
          form (fnl-reader/read-string fennel-src)
          env (clj-ana/empty-env 'user)
          opts (clj-ana/default-opts)
          ast (fnl-ana/unanalyzed form env opts)]
      (is (= :unanalyzed (:op ast)))
      (let [form (:form ast)]
        (is (seq? form))
        (is (= 'fn (first form)))
        (is (= 'demo-func (second form)))))))

(deftest test-multiple-forms
  (testing "Create unanalyzed AST nodes for multiple Fennel forms"
    (let [fennel-src "(fn [x] x) (+ 1 2) [a b c]"
          forms (fnl-reader/read-all fennel-src)
          env (clj-ana/empty-env 'user)
          opts (clj-ana/default-opts)
          asts (mapv #(fnl-ana/unanalyzed % env opts) forms)]
      (is (= 3 (count asts)))
      (is (every? #(= :unanalyzed (:op %)) asts))
      (is (= '(fn [x] x) (:form (nth asts 0))))
      (is (= '(+ 1 2) (:form (nth asts 1))))
      (is (= '[a b c] (:form (nth asts 2)))))))

(deftest test-check-form-info-string
  (testing "Check a Fennel string literal end-to-end with type inference"
    (let [fennel-src "\"Hello, world!\""
          form (fnl-reader/read-string fennel-src)
          result (typed.fnl.checker/check-form-info form {:check-config {}} (fnl-utils/->opts))]
      (is (map? result))
      (is (not (contains? result :ex)) "Should not have exception")
      (is (contains? result :ret) "Should have return type")
      ;; Result type should be Any (check that it's a TCResult)
      (is (map? (:ret result)) "Result should be a TCResult map"))))

(deftest test-check-form-info-number
  (testing "Check a Fennel number literal end-to-end"
    (let [fennel-src "42"
          form (fnl-reader/read-string fennel-src)
          result (typed.fnl.checker/check-form-info form {:check-config {}} (fnl-utils/->opts))]
      (is (map? result))
      (is (not (contains? result :ex)) "Should not have exception")
      (is (contains? result :ret) "Should have return type")
      (is (map? (:ret result)) "Result should be a TCResult map"))))

(deftest test-check-form-info-list
  (testing "Check a Fennel list expression end-to-end"
    (let [fennel-src "(+ 1 2)"
          form (fnl-reader/read-string fennel-src)
          result (typed.fnl.checker/check-form-info form {:check-config {}} (fnl-utils/->opts))]
      (is (map? result))
      (is (not (contains? result :ex)) "Should not have exception")
      (is (contains? result :ret) "Should have return type")
      (is (map? (:ret result)) "Result should be a TCResult map"))))

(deftest test-check-form-info-fn
  (testing "Check a Fennel function end-to-end"
    (let [fennel-src "(fn [x] x)"
          form (fnl-reader/read-string fennel-src)
          result (typed.fnl.checker/check-form-info form {:check-config {}} (fnl-utils/->opts))]
      (is (map? result))
      (is (not (contains? result :ex)) "Should not have exception")
      (is (contains? result :ret) "Should have return type")
      (is (map? (:ret result)) "Result should be a TCResult map"))))
