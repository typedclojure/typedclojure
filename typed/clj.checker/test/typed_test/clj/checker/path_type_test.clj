(ns ^:typed.clojure typed-test.clj.checker.path-type-test
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]
            [typed.clj.checker.test-utils :refer :all]))

;; Tests for occurrence typing with path elements
;; Related to issue: Bug in type inference with intermediate variables

(deftest occurrence-typing-inline-test
  (testing "Positive: both branches should work"
    ;; This is the minimal case - checking the value directly without intermediate variable
    (is-tc-e
      (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
        (if (string? (:access_token res))
          (t/ann-form (:access_token res) t/Str)
          (t/ann-form (:access_token res) (t/U nil t/Any))))))
  (testing "Negative: wrong type annotation in then branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 30, :file "path_type_test.clj"},
               :form (:access_token res),
               :data {:expected-type nil, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tnil\n\nActual: \tString"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
               (if (string? (:access_token res))
                 (t/ann-form (:access_token res) nil)
                 (t/ann-form (:access_token res) (t/U nil t/Any))))))))
  (testing "Negative: wrong type annotation in else branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 30, :file "path_type_test.clj"},
               :form (:access_token res),
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
               (if (string? (:access_token res))
                 (t/ann-form (:access_token res) t/Str)
                 (t/ann-form (:access_token res) t/Str))))))))

(deftest occurrence-typing-with-intermediate-var-test
  (testing "Positive: both branches should work with intermediate variable"
    ;; This is the reported issue - with intermediate variable `at`
    (is-tc-e
      (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
        (let [at (:access_token res)]
          (if (string? at)
            (t/ann-form at t/Str)
            (t/ann-form at (t/U nil t/Any)))))))
  (testing "Negative: wrong type annotation in then branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 20, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Int, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tt/Int\n\nActual: \tString"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
               (let [at (:access_token res)]
                 (if (string? at)
                   (t/ann-form at t/Int)
                   (t/ann-form at (t/U nil t/Any)))))))))
  (testing "Negative: wrong type annotation in else branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 20, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
               (let [at (:access_token res)]
                 (if (string? at)
                   (t/ann-form at t/Str)
                   (t/ann-form at t/Str)))))))))

(deftest occurrence-typing-hmap-inline-test
  (testing "Positive: both branches should work with HMap"
    (is-tc-e
      (t/fn [res :- (t/HMap :optional {:access_token t/Any})]
        (if (string? (:access_token res))
          (t/ann-form (:access_token res) t/Str)
          (t/ann-form (:access_token res) (t/U nil t/Any))))))
  (testing "Negative: wrong type annotation in then branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 30, :file "path_type_test.clj"},
               :form (:access_token res),
               :data {:expected-type t/Num, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tt/Num\n\nActual: \tString"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/HMap :optional {:access_token t/Any})]
               (if (string? (:access_token res))
                 (t/ann-form (:access_token res) t/Num)
                 (t/ann-form (:access_token res) (t/U nil t/Any))))))))
  (testing "Negative: wrong type annotation in else branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 30, :file "path_type_test.clj"},
               :form (:access_token res),
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/HMap :optional {:access_token t/Any})]
               (if (string? (:access_token res))
                 (t/ann-form (:access_token res) t/Str)
                 (t/ann-form (:access_token res) t/Str))))))))

(deftest occurrence-typing-hmap-with-intermediate-var-test
  (testing "Positive: both branches should work with HMap and intermediate variable"
    (is-tc-e
      (t/fn [res :- (t/HMap :optional {:access_token t/Any})]
        (let [at (:access_token res)]
          (if (string? at)
            (t/ann-form at t/Str)
            (t/ann-form at (t/U nil t/Any)))))))
  (testing "Negative: wrong type annotation in then branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 20, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Bool, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tt/Bool\n\nActual: \tString"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/HMap :optional {:access_token t/Any})]
               (let [at (:access_token res)]
                 (if (string? at)
                   (t/ann-form at t/Bool)
                   (t/ann-form at (t/U nil t/Any)))))))))
  (testing "Negative: wrong type annotation in else branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 20, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/HMap :optional {:access_token t/Any})]
               (let [at (:access_token res)]
                 (if (string? at)
                   (t/ann-form at t/Str)
                   (t/ann-form at t/Str)))))))))

(deftest occurrence-typing-and-check-test
  (testing "Positive: both branches with AND check and two intermediate variables"
    (is-tc-e
      (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
        (let [at (:access_token res)
              rt (:refresh_token res)]
          (if (and (string? at) (string? rt))
            {:access_token (t/ann-form at t/Str)
             :refresh_token (t/ann-form rt t/Str)}
            {:access_token (t/ann-form at (t/U nil t/Any))
             :refresh_token (t/ann-form rt (t/U nil t/Any))})))))
  (testing "Negative: wrong type annotation in then branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 35, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Kw, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tt/Kw\n\nActual: \tString"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
               (let [at (:access_token res)
                     rt (:refresh_token res)]
                 (if (and (string? at) (string? rt))
                   {:access_token (t/ann-form at t/Kw)
                    :refresh_token (t/ann-form rt t/Str)}
                   {:access_token (t/ann-form at (t/U nil t/Any))
                    :refresh_token (t/ann-form rt (t/U nil t/Any))})))))))
  (testing "Negative: wrong type annotation in else branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 35, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/Option (t/Map t/Kw t/Any))]
               (let [at (:access_token res)
                     rt (:refresh_token res)]
                 (if (and (string? at) (string? rt))
                   {:access_token (t/ann-form at t/Str)
                    :refresh_token (t/ann-form rt t/Str)}
                   {:access_token (t/ann-form at t/Str)
                    :refresh_token (t/ann-form rt (t/U nil t/Any))}))))))))

(deftest occurrence-typing-and-check-hmap-test
  (testing "Positive: both branches with AND check, HMap and two intermediate variables"
    (is-tc-e
      (t/fn [res :- (t/HMap :optional {:access_token t/Any
                                       :refresh_token t/Any})]
        (let [at (:access_token res)
              rt (:refresh_token res)]
          (if (and (string? at) (string? rt))
            {:access_token (t/ann-form at t/Str)
             :refresh_token (t/ann-form rt t/Str)}
            {:access_token (t/ann-form at (t/U nil t/Any))
             :refresh_token (t/ann-form rt (t/U nil t/Any))})))))
  (testing "Negative: wrong type annotations in then branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 35, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Num, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tt/Num\n\nActual: \tString"}
              {:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 36, :file "path_type_test.clj"},
               :form rt,
               :data {:expected-type t/Num, :actual-type String},
               :message "Type mismatch:\n\nExpected: \tt/Num\n\nActual: \tString"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/HMap :optional {:access_token t/Any
                                              :refresh_token t/Any})]
               (let [at (:access_token res)
                     rt (:refresh_token res)]
                 (if (and (string? at) (string? rt))
                   {:access_token (t/ann-form at t/Num)
                    :refresh_token (t/ann-form rt t/Num)}
                   {:access_token (t/ann-form at (t/U nil t/Any))
                    :refresh_token (t/ann-form rt (t/U nil t/Any))})))))))
  (testing "Negative: wrong type annotations in else branch should fail"
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 35, :file "path_type_test.clj"},
               :form at,
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}
              {:type-error :typed.clojure/type-mismatch-error,
               :env {:line "REMOVED_LINE", :column 36, :file "path_type_test.clj"},
               :form rt,
               :data {:expected-type t/Str, :actual-type t/Any},
               :message "Type mismatch:\n\nExpected: \tt/Str\n\nActual: \tt/Any"}]}
           (is-tc-err-messages
             (t/fn [res :- (t/HMap :optional {:access_token t/Any
                                              :refresh_token t/Any})]
               (let [at (:access_token res)
                     rt (:refresh_token res)]
                 (if (and (string? at) (string? rt))
                   {:access_token (t/ann-form at t/Str)
                    :refresh_token (t/ann-form rt t/Str)}
                   {:access_token (t/ann-form at t/Str)
                    :refresh_token (t/ann-form rt t/Str)}))))))))
