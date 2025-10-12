(ns ^:typed.clojure clojure.core.typed.test.class-path-elem-test
  (:require [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err-messages is-tc-err]]
            [clojure.test :refer [deftest is testing]]))

(deftest class-path-elem-test-basic
  (testing "class on a number returns non-nil Class"
    (is-tc-e
      (let [a 1]
        (ann-form (class a) Class)))
    (testing "via path alias"
      (is-tc-e
        (let [a 1
              b (class a)]
          (ann-form b Class))))
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 26,
                :file "class_path_elem_test.clj"},
               :form (class a),
               :data {:expected-type Class, :actual-type (t/Option Class)},
               :message
               "Type mismatch:\n\nExpected: \tClass\n\nActual: \t(t/Option Class)"}
              {:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 26,
                :file "class_path_elem_test.clj"},
               :form (class b),
               :data {:expected-type Class, :actual-type nil},
               :message "Type mismatch:\n\nExpected: \tClass\n\nActual: \tnil"}
              {:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 18,
                :file "class_path_elem_test.clj"},
               :form c,
               :data {:expected-type nil, :actual-type (t/U Class nil)},
               :message
               "Type mismatch:\n\nExpected: \tnil\n\nActual: \t(t/U Class nil)"}
              {:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 18,
                :file "class_path_elem_test.clj"},
               :form d,
               :data {:expected-type Class, :actual-type nil},
               :message "Type mismatch:\n\nExpected: \tClass\n\nActual: \tnil"}]}
           (is-tc-err-messages
             (fn [a :- (t/Nilable t/Num)
                  b :- nil]
               (ann-form (class a) Class)
               (ann-form (class b) Class)
               ;; via path alias
               (let [c (class a)
                     d (class b)]
                 (ann-form c nil)
                 (ann-form d Class))))))))

(deftest class-path-elem-nilable-test
  (is-tc-e
    (fn [x :- Object]
      (ann-form (class x) Class)))
  (is-tc-err
    (fn [x :- Object]
      (ann-form (class x) nil)))
  (is-tc-e
    (let [s "hello"]
      (ann-form (class s) Class)))
  (is-tc-err
    (let [s "hello"]
      (ann-form (class s) nil)))
  (is-tc-e
    (let [k :keyword]
      (ann-form (class k) Class)))
  (is-tc-err
    (let [k :keyword]
      (ann-form (class k) nil)))
  (is-tc-e
    (let [v [1 2 3]]
      (ann-form (class v) Class)))
  (is-tc-err
    (let [v [1 2 3]]
      (ann-form (class v) nil))))

(deftest class-path-elem-test-type-narrowing
  (testing "class enables type narrowing with ="
    (is-tc-e
      (fn [x :- t/Any]
        (if (= java.lang.Long (class x))
          (ann-form x Long)
          x)))
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 18,
                :file "class_path_elem_test.clj"},
               :form x,
               :data {:expected-type nil, :actual-type Long},
               :message "Type mismatch:\n\nExpected: \tnil\n\nActual: \tLong"}]}
           (is-tc-err-messages
             (fn [x :- t/Any]
               (if (= java.lang.Long (class x))
                 (ann-form x nil)
                 x))))))

  (testing "class in let binding enables type narrowing"
    (is-tc-e
      (fn [x :- t/Any]
        (let [c (class x)]
          (if (= java.lang.Long c)
            (ann-form x Long)
            x))))
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 20,
                :file "class_path_elem_test.clj"},
               :form x,
               :data {:expected-type nil, :actual-type Long},
               :message "Type mismatch:\n\nExpected: \tnil\n\nActual: \tLong"}]}
           (is-tc-err-messages
             (fn [x :- t/Any]
               (let [c (class x)]
                 (if (= java.lang.Long c)
                   (ann-form x nil)
                   x)))))))

  (testing "class with isa?"
    (is-tc-e
      (fn [x :- t/Any]
        (when (isa? (class x) Number)
          (ann-form x Number))))
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 18,
                :file "class_path_elem_test.clj"},
               :form x,
               :data {:expected-type nil, :actual-type Number},
               :message "Type mismatch:\n\nExpected: \tnil\n\nActual: \tNumber"}]}
           (is-tc-err-messages
             (fn [x :- t/Any]
               (when (isa? (class x) Number)
                 (ann-form x nil)))))))

  (testing "instance? followed by class"
    (is-tc-e
      (fn [x :- t/Any]
        (when (instance? Number x)
          (let [c (class x)]
            (ann-form c Class)))))
    (is (= '{:type-errors
             [{:type-error :typed.clojure/type-mismatch-error,
               :env
               {:line "REMOVED_LINE",
                :column 20,
                :file "class_path_elem_test.clj"},
               :form c,
               :data {:expected-type nil, :actual-type Class},
               :message "Type mismatch:\n\nExpected: \tnil\n\nActual: \tClass"}]}
           (is-tc-err-messages
             (fn [x :- t/Any]
               (when (instance? Number x)
                 (let [c (class x)]
                   (ann-form c nil)))))))))
