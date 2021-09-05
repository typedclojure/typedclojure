(ns ^:no-doc typed-test.clj.ext.clojure.core
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.typed :as t]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.ext.clojure.core__let :as ext-let]
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

;; tests for clojure.core.typed macros in typed-test.clj.ext.clojure.core.typed
(deftest vector-destructure-error-msg-test
  (testing "let"
    (is (= (is-tc-err-messages
             #(let [[a] #{1}]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(clojure.core/let [[a] #{1}]
                            a)}]]}))
    (is (= (is-tc-err-messages
             #(let [{[a] :foo} {:foo #{1}}]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(clojure.core/let [{[a] :foo} {:foo #{1}}]
                            a)}]]})))
  (testing "for"
    (is (= (is-tc-err-messages
             #(cc/for [[a] [#{1}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/for [[a] [#{1}]]
                            a)}]]}))
    (is (= (is-tc-err-messages
             #(cc/for [{[a] :foo} [{:foo #{1}}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/for [{[a] :foo} [{:foo #{1}}]]
                            a)}]]})))
  (testing "doseq"
    (is (= (is-tc-err-messages
             #(cc/doseq [[a] [#{1}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/doseq [[a] [#{1}]]
                            a)}]]}))
    (is (= (is-tc-err-messages
             #(cc/doseq [{[a] :foo} [{:foo #{1}}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/doseq [{[a] :foo} [{:foo #{1}}]]
                            a)}]]})))
  (testing "fn"
    (is (= (is-tc-err-messages
             (ann-form (cc/fn [[a]])
                       [(t/Set t/Any) -> t/Any]))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(IPersistentSet Any)"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/fn [[a]])}]]})))
  (testing "defn"
    (is (= (is-tc-err-messages
             (do (t/ann foo [(t/Set t/Any) :-> t/Any])
                 (cc/defn foo [[a]])))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(IPersistentSet Any)"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/defn foo [[a]])}]]})))
  )
