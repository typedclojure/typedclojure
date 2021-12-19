(ns ^:no-doc typed-test.ext.clojure.core.typed
  (:require [clojure.test :refer [deftest is]]
            typed.clj.ext.clojure.core.typed ;; load
            [typed.clj.ext.clojure.core__let :as extcc__let]
            [clojure.core.typed :as t]
            [clojure.core.typed.test.test-utils :refer :all]))

(defn eval-in-ns [form]
  (binding [*ns* *ns*]
    (in-ns (gensym))
    (refer-clojure)
    (form)))

(deftest tc-ignore-test
  ; type checked
  (let [form `(t/tc-ignore (t/ann-form 1 nil))
        expected `t/Any
        res (t/check-form-info form
                               :expected expected
                               :type-provided? true)]
    (is (-> res :delayed-errors empty?))
    (is (not (:ex res))))
  ; type error
  (let [form `(t/tc-ignore 1)
        expected `t/Num
        res (t/check-form-info form
                               :expected expected
                               :type-provided? true)]
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
  (let [form `(t/tc-ignore 1)
        res (t/check-form-info form)]
    (is
      (= [:result 1]
         (-> res
             (find :result))))))

(deftest ann-form-test
  ; type checked
  (let [form `(t/ann-form 1 t/Int)
        expected `t/Num
        res (t/check-form-info form
                               :expected expected
                               :type-provided? true)]
    (is (-> res :delayed-errors empty?))
    (is (not (:ex res))))
  ; type error
  ;; inner form does not have ascribed type
  (let [form `(t/ann-form :foo t/Int)
        expected `t/Num
        res (t/check-form-info form
                               :expected expected
                               :type-provided? true)]
    (is (or (seq (:delayed-errors res))
            (:ex res))
        res))
  ;; expected type not a supertype of ascribed type
  (let [form `(t/ann-form 1 t/Int)
        expected `t/Bool
        res (t/check-form-info form
                               :expected expected
                               :type-provided? true)]
    (is (= form
           (or (some-> res
                       :delayed-errors
                       first
                       ex-data
                       :form)
               (some-> res
                       :ex
                       ex-data
                       :errors
                       first
                       ex-data
                       :form)))
        res))
  ; eval
  (let [form `(t/ann-form 1 t/Int)
        res (t/check-form-info form)]
    (is (= 1 (eval form)))
    (is (= [:result 1]
           (-> res
               (find :result)))
        res))
  ;; top level
  (let [form `(t/ann-form (do (defmacro ~'foo [] 1) (~'foo)) t/Int)
        res (eval-in-ns #(t/check-form-info form))]
    (is (= 1 (eval-in-ns #(eval form))))
    (is
      (= [:result 1]
         (-> res
             (find :result))))))

(deftest fn-vector-destructure-error-msg-test
  (is (= (is-tc-err-messages
           (fn [[a] :- (t/Set t/Any)]))
         {:delayed-errors [[(extcc__let/bad-vector-destructure-error-msg
                              "(IPersistentSet Any)"
                              "[a]")
                            {:type-error :clojure.core.typed.errors/tc-error-parent
                             :form '(fn [[a] :- (t/Set t/Any)])}]]}))
  )
