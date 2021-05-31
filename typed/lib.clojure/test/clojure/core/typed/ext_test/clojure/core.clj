(ns ^:no-doc clojure.core.typed.ext-test.clojure.core
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.typed :as t]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.test.test-utils :refer :all]))

(defn eval-in-ns [form]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (refer-clojure)
    (form)))

(deftest ns-test
  ; type checked
  (let [form `(ns ~'foo)
        expected nil
        res (binding [*ns* *ns*]
              (t/check-form-info form
                                 :expected expected
                                 :type-provided? true))]
    (is (-> res :delayed-errors empty?))
    (is (not (:ex res))))
  ; type error
  (let [form `(ns ~'foo)
        expected `t/Str
        res (binding [*ns* *ns*]
              (t/check-form-info form
                                 :expected expected
                                 :type-provided? true))
        err ((some-fn (comp first
                            :errors
                            ex-data
                            :ex)
                      (comp first
                            :delayed-errors))
             res)]
    (is (-> err
            ex-data
            :form
            #{form})))
  ; eval
  (binding [*ns* *ns*]
    (let [form `(ns ~'foo)
          res (t/check-form-info form)
          _ (is (= *ns* (find-ns 'foo)))]
      (is
        (-> res
            (find :result)
            #{[:result nil]})))))

(deftest defmacro-test
  ; type checked
  (let [form `(defmacro ~'foo [] 1)
        res (eval-in-ns
              #(t/check-form-info form))]
    (is (-> res :delayed-errors empty?))
    (is (not (:ex res))))
  ; type error
  (let [form `(defmacro ~'foo [] 1)
        expected `t/Str
        res (eval-in-ns
              #(t/check-form-info form
                                  :expected expected
                                  :type-provided? true))]
    (is (= form
           (some-> res
                   :ex
                   ex-data
                   :errors
                   first
                   ex-data
                   :form))
        res))
  ; eval
  (eval-in-ns
    (fn []
      (let [form `(do (defmacro ~'foo [] 1)
                      (~'foo))
            res (t/check-form-info form)]
        (is
          (= [:result 1]
             (-> res
                 (find :result))))))))

(deftest for-test
  ; type checked
  (is-tc-e (cc/for [a (range 3)] a)
           (t/Seq t/Int))
  (is-tc-err (cc/for [a (range 3)] a)
             (t/Seq t/Bool))
  (is-tc-e (cc/for [e {1 2 3 4}] e)
           (t/Seq '[t/Int t/Int]))
  ; no expected
  (let [{:keys [t]} (tc-e (cc/for [a (range 3)] a))]
    (is (subtype? (prs/parse-clj `(t/ASeq Long)) t))
    (is (subtype? t (prs/parse-clj `(t/Seq t/Int)))))
  ;; multiple clauses
  ; type checked
  (is-tc-e (cc/for [a (range 3)
                    b (repeat 10 (inc a))]
             (str (+ a b)))
           (t/Seq t/Str))
  (is-tc-err (cc/for [a (range 3)
                      b (repeat 10 (str (inc a)))]
               ;; b is Str, so type error
               (str (+ a b)))
             (t/Seq t/Str))
  ;; non-seqable clause
  (is-tc-err (cc/for [a 1] a))
  ;; destructuring
  (is-tc-e (cc/for [[:as e] {1 2 3 4}]
             e)
           (t/Seq '[t/Int t/Int]))
  (is-tc-e (cc/for [[a] {1 2 3 4}]
             a)
           (t/Seq t/Int))
  (is-tc-e (cc/for [[a b] {1 2 3 4}]
             [a b])
           (t/Seq '[t/Int t/Int]))
  )

