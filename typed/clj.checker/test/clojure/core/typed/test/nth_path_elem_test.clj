(ns ^:typed.clojure clojure.core.typed.test.nth-path-elem-test
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest nth-path-elem-test-first
  (is-tc-e
    (do
      (defalias StatementA '[':params String])
      (defalias StatementB '[':no-params])
      (defalias Statement (t/U StatementA StatementB))
      (fn [stmt :- Statement] :- t/Any
        (when (= :params (first stmt))
          (let [param (nth stmt 1)]
            (ann-form param String)))))))

(deftest nth-path-elem-test-second
  (is-tc-e
    (do
      (defalias StatementA '[Number ':params String])
      (defalias StatementB '[Number ':no-params])
      (defalias Statement (t/U StatementA StatementB))
      (fn [stmt :- Statement] :- t/Any
        (when (= :params (second stmt))
          (let [param (nth stmt 2)]
            (ann-form param String))))))

  (testing "we actually do typechecking at the innermost form"
    ;; If `second` incorrectly uses an index of 0, this test fails
    (is-tc-err
      (do
        (defalias StatementA '[Number ':params String])
        (defalias StatementB '[Number ':no-params])
        (defalias Statement (t/U StatementA StatementB))
        (fn [stmt :- Statement] :- t/Any
          (when (= :params (second stmt))
            (let [param (nth stmt 2)]
              (ann-form param Number))))))))

(deftest nth-path-elem-test-nth
  (is-tc-e
    (do
      (defalias StatementA '[':params String])
      (defalias StatementB '[':no-params])
      (defalias Statement (t/U StatementA StatementB))
      (fn [stmt :- Statement] :- t/Any
        (when (= :params (nth stmt 0))
          (let [param (nth stmt 1)]
            (ann-form param String)))))))

(deftest nth-path-elem-test-existing-path
  (is-tc-e
    (do
      (defalias StatementA '[':a '[':params String]])
      (defalias StatementB '[':b '[':no-params]])
      (defalias Statement (t/U StatementA StatementB))
      (fn [stmt :- Statement] :- t/Any
        (when (= :params (nth (nth stmt 1) 0))
          (let [param (nth (nth stmt 1) 1)]
            (ann-form param String)))))))

(deftest nth-path-elem-test-multimethod
  (is-tc-e
    (do
      (defalias StatementA '[':params String])
      (defalias StatementB '[':no-params])
      (defalias Statement (t/U StatementA StatementB))

      (ann nth-path-multimethod [Statement -> t/Any])
      (defmulti nth-path-multimethod first)

      (defmethod nth-path-multimethod :params [stmt]
        (ann-form stmt StatementA))

      (defmethod nth-path-multimethod :no-params [stmt]
        (ann-form stmt StatementB)))))

(deftest nth-path-elem-test-types
  (testing "HVec"
    (is-tc-e
      (do
        (defalias StatementA (t/HVec [':params String]))
        (defalias StatementB (t/HVec [':no-params]))
        (defalias Statement (t/U StatementA StatementB))
        (fn [stmt :- Statement] :- t/Any
          (when (= :params (first stmt))
            (ann-form stmt StatementA))))))

  (testing "HSeq"
    (is-tc-e
      (do
        (defalias StatementA (t/HSeq [':params String]))
        (defalias StatementB (t/HSeq [':no-params]))
        (defalias Statement (t/U StatementA StatementB))
        (fn [stmt :- Statement] :- t/Any
          (when (= :params (first stmt))
            (ann-form stmt StatementA))))))

  (testing "HSequential"
    (is-tc-e
      (do
        (defalias StatementA (t/HSequential [':params String]))
        (defalias StatementB (t/HSequential [':no-params]))
        (defalias Statement (t/U StatementA StatementB))
        (fn [stmt :- Statement] :- t/Any
          (when (= :params (first stmt))
            (ann-form stmt StatementA))))))

  (testing "MapEntry"
    (is-tc-e
      (-> {:a 1}
          first
          (nth 0)
          (ann-form ':a))))

  (testing "a mixture of types"
    (is-tc-e
      (do
        (defalias StatementA (t/HSeq [':params String]))
        (defalias StatementB (t/HVec [':no-params]))
        (defalias Statement (t/U StatementA StatementB))
        (fn [stmt :- Statement] :- t/Any
          (when (= :params (first stmt))
            (ann-form stmt StatementA)))))))
