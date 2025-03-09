(ns ^:typed.clojure clojure.core.typed.test.nthnext-test
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest nthnext-test
  (is-tc-e
   (fn [stmt :- '[t/Any Long]]
     (let [body (nthnext stmt 1)]
       (ann-form body (t/HSeq [Long])))))

  (is-tc-e
   (ann-form (nthnext nil 1) nil))

  )

(deftest nthnext-test-input-types
  (testing "HVec"
    (is-tc-e
     (fn [stmt :- (t/HVec [t/Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (t/HSeq [Long]))))))

  (testing "HSeq"
    (is-tc-e
     (fn [stmt :- (t/HSeq [t/Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (t/HSeq [Long]))))))

  (testing "HSequential"
    (is-tc-e
     (fn [stmt :- (t/HSequential [t/Any Long])]
       (let [body (nthnext stmt 1)]
         (ann-form body (t/HSeq [Long])))))))

(deftest next-test-input-types
  (testing "HVec"
    (is-tc-e
     (fn [stmt :- (t/HVec [t/Any Long])]
       (let [body (next stmt)]
         (ann-form body (t/HSeq [Long]))))))

  (testing "HSeq"
    (is-tc-e
     (fn [stmt :- (t/HSeq [t/Any Long])]
       (let [body (next stmt)]
         (ann-form body (t/HSeq [Long]))))))

  (testing "HSequential"
    (is-tc-e
     (fn [stmt :- (t/HSequential [t/Any Long])]
       (let [body (next stmt)]
         (ann-form body (t/HSeq [Long])))))))

(deftest seq-test-input-types
  (testing "HVec"
    (is-tc-e
     (fn [stmt :- (t/HVec [t/Any Long])]
       (let [body (seq stmt)]
         (ann-form body (t/HSeq [t/Any Long]))))))

  (testing "HSeq"
    (is-tc-e
     (fn [stmt :- (t/HSeq [t/Any Long])]
       (let [body (seq stmt)]
         (ann-form body (t/HSeq [t/Any Long]))))))

  (testing "HSequential"
    (is-tc-e
     (fn [stmt :- (t/HSequential [t/Any Long])]
       (let [body (seq stmt)]
         (ann-form body (t/HSeq [t/Any Long])))))))

(deftest nthnext-test-fixed-types
  (testing "skipping past all the fixed types"
    (is-tc-e
     (fn [stmt :- '[t/Any Long]]
       (let [body (nthnext stmt 100)]
         (ann-form body nil))))
    (is-tc-err
     (fn [stmt :- '[t/Any Long]]
       (let [body (nthnext stmt 100)]
         (ann-form body (t/HSeq [])))))))

(deftest nthnext-test-fixed-types-and-rest
  (testing "skipping past all the fixed types with a rest type"
    (is-tc-e
     (fn [stmt :- '[t/Any Long *]]
       (let [body (nthnext stmt 100)]
         (ann-form body (t/Option (t/HSeq [Long *]))))))
    (is-tc-err
     (fn [stmt :- '[t/Any Long *]]
       (let [body (nthnext stmt 100)]
         (ann-form body (t/HSeq [Long *])))))
    (is-tc-err
     (fn [stmt :- '[t/Any Long *]]
       (let [body (nthnext stmt 100)]
         (ann-form body nil))))))

(deftest nthnext-test-union
  (testing "a union of types is also refined"
    (is-tc-e
      (fn [stmt :- (t/U '[t/Any String]
                        '[t/Any Long])]
        (let [body (nthnext stmt 1)]
          (ann-form body (t/U (t/HSeq [String])
                              (t/HSeq [Long]))))))))

(deftest nthnext-test-destructuring
  (testing "implicitly used via destructuring"
    (is-tc-e
     (fn [stmt :- '[t/Any Long]]
       (let [[_ & body] stmt]
         (ann-form body (t/HSeq [Long]))))))

  (testing "destructuring past fixed types"
    (is-tc-e
     (fn [stmt :- '[t/Any]]
       (let [[_ & body] stmt]
         (ann-form body nil)))))

  (testing "destructuring past fixed types with a rest type"
    (is-tc-e
     (fn [stmt :- '[t/Any Long *]]
       (let [[_ & body] stmt]
         (ann-form body (t/Option (t/HSeq [Long *]))))))))

(deftest nthnext-update-test
  (is-tc-e
    (fn [c :- (t/Seq t/Num)]
      (if (seq c)
        (do (t/ann-form c (t/CountRange 1))
            (t/ann-form (first c) t/Num))
        (do (t/ann-form c (t/ExactCount 0))
            (t/ann-form (first c) nil)))))
  (is-tc-e
    (fn [c :- nil]
      (when (seq c)
        (t/ann-form :unreachable (t/U)))))
  (is-tc-e
    (fn [c :- nil]
      (when (next c)
        (t/ann-form :unreachable (t/U)))))
  (is-tc-e
    (fn [c :- (t/Seq t/Num)]
      (if (next c)
        (do (t/ann-form c (t/CountRange 2))
            (t/ann-form (first c) t/Num))
        (do (t/ann-form c (t/CountRange 0 1))
            (t/ann-form (first c) (t/U nil t/Num))))))
  (is-tc-e
    (fn [c :- (t/Seq t/Num)]
      (if (nthnext c 1)
        (do (t/ann-form c (t/CountRange 2))
            (t/ann-form (first c) t/Num))
        (do (t/ann-form c (t/CountRange 0 1))
            (t/ann-form (first c) (t/U nil t/Num))))))
  (is-tc-e
    (fn [c :- (t/Seq t/Num)]
      (if (next c)
        (t/ann-form c (t/CountRange 2))
        (do
          (t/ann-form c (t/CountRange 0 1))
          (if (seq c)
            (t/ann-form c (t/ExactCount 1))
            (t/ann-form c (t/ExactCount 0))))))))
