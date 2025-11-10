(ns ^:typed.clojure clojure.core.typed.test.error-msg
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.string :as str]
            [clojure.test :refer [is deftest testing]]
            [clojure.core.typed :as t :refer [cf check-form-info check-ns check-ns-info]]))

(def this-nsym (ns-name *ns*))

(deftest invoke-line-number
  (is (= {:line 4 :column 2}
         (-> (check-ns-info 'clojure.core.typed.test.line-number)
             :type-errors
             first
             :env
             (select-keys [:line :column])))))

(deftest cf-throws-test
  (is (thrown? Throwable (cf (nil))))
  (is (thrown? Throwable (cf (clojure.core/fn [:- :a]))))
  (testing "cf prints errors and throw exceptions"
    (let [stderr (with-err-string-writer
                   (is (thrown-with-msg? Exception #"cannot be cast" (cf (+ 1 "oops")))))]
      (is (str/includes? stderr "Type Error"))
      (is (str/includes? stderr "A fatal error was thrown during type checking, rethrowing.")))))

(deftest check-ns-info-result-test
  (testing "ok"
    (let [{:keys [ex type-errors]} (check-ns-info 'clojure.core.typed.test.proposition-combine)]
      (is (nil? ex))
      (is (empty? type-errors))))
  (testing "check-ns-info does not print errors"
    (let [res (promise)
          stderr (with-err-string-writer (deliver res (check-ns-info 'clojure.core.typed.test.kw-args-undeclared-fail)))
          {:keys [ex type-errors]} @res]
      (is (empty? stderr))
      (is (nil? ex))
      (when (is (= 1 (count type-errors)))
        (let [[{:keys [type-error form env data]}] type-errors]
          (is (= type-error :typed.clojure/app-type-error))
          (is (= '(undeclared-kw-invoke-test :blah (quote a)) form))
          (is (:line env))
          (is (:column env))
          (is (:file env))
          (is (= data '{:fn-type [& :optional {:foo t/Any} :-> nil],
                        :args-results
                        [{:type (t/Val :blah), :proposition-set {:then tt, :else ff}}
                         {:type (t/Val a), :proposition-set {:then tt, :else ff}}]})))))))

(deftest check-form-info-result-test
  (binding [*ns* (the-ns this-nsym)]
    (is (= 1 (:result (check-form-info '(do (do (do 1)))))))
    (testing "ok"
      (let [{:keys [ex type-errors]} (check-form-info '#(/ 1 0))]
        (is (not ex))
        (is (empty? type-errors))))
    (testing "runtime error after checking with no delayed errors"
      (let [{:keys [ex type-errors]} (check-form-info '(/ 1 0))]
        (is (str/includes? (ex-message ex) "Divide by zero"))
        (is (empty? type-errors))))
    (testing "runtime error after checking with delayed errors"
      (let [res (promise)
            stderr (with-err-string-writer (deliver res (check-form-info '(+ 1 "oops"))))
            {:keys [ex type-errors]} @res]
        (testing "check-form-info does not print errors"
          (is (empty? stderr)))
        (is (str/includes? (ex-message ex) "cannot be cast"))
        (when (is (= 1 (count type-errors)))
          (let [[{:keys [type-error form env data message]}] type-errors]
            (is (= :typed.clojure/app-type-error type-error))
            (is (= '(+ 1 "oops") form))
            (is (:line env))
            (is (:column env))
            (is (string? (:file env)))
            (is (string? message))
            (is (= '{:fn-type
                     (t/IFn
                       [Long :* :-> Long]
                       [Double Double :* :-> Double]
                       [t/AnyInteger :* :-> t/AnyInteger]
                       [t/Num :* :-> t/Num]),
                     :args-results
                     [{:type (t/Val 1), :proposition-set {:then tt, :else ff}}
                      {:type (t/Val "oops"), :proposition-set {:then tt, :else ff}}]}
                   data))))))
    (let [{:keys [ex type-errors]} (check-form-info '#(+ 1 "oops"))]
      (is (nil? ex))
      (when (= 1 (count type-errors))
        (let [[{:keys [type-error form env data]}] type-errors]
          (is (= type-error :typed.clojure/app-type-error))
          (is (= '(+ 1 "oops") form))
          (is (:line env))
          (is (:column env))
          (is (:file env))
          (is (= '{:fn-type
                   (t/IFn
                     [Long :* :-> Long]
                     [Double Double :* :-> Double]
                     [t/AnyInteger :* :-> t/AnyInteger]
                     [t/Num :* :-> t/Num]),
                   :args-results
                   [{:type (t/Val 1), :proposition-set {:then tt, :else ff}}
                    {:type (t/Val "oops"), :proposition-set {:then tt, :else ff}}],
                   :expected-result {:type t/Infer}}
                 data)))))))
