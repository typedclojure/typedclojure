(ns ^:no-doc typed.ext-test.clojure.core
  (:require [clojure.test :refer [deftest is]]
            [clojure.core.typed :as t]))

(deftest tc-ignore-test
  ; type checked
  (let [form `(t/tc-ignore (t/ann-form 1 nil))
        expected `t/Any
        res (binding [*ns* *ns*]
              (t/check-form-info form
                                 :expected expected
                                 :type-provided? true))]
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
