(ns ^:no-doc clojure.core.typed.ext-test.clojure.core
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.typed :as t]))

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
