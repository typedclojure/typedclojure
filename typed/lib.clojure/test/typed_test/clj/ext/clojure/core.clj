(ns ^:no-doc typed-test.clj.ext.clojure.core
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.template :refer [do-template]]
            [clojure.core.typed :as t]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.ext.clojure.core__let :as ext-let]
            [typed.clj.checker.test-utils :refer :all]))

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
                    "(typed.clojure/HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(clojure.core/let [[a] #{1}]
                            a)}]]}))
    (is (= (is-tc-err-messages
             #(let [{[a] :foo} {:foo #{1}}]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(typed.clojure/HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(clojure.core/let [{[a] :foo} {:foo #{1}}]
                            a)}]]})))
  (testing "for"
    (is (= (is-tc-err-messages
             #(cc/for [[a] [#{1}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(typed.clojure/HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/for [[a] [#{1}]]
                            a)}]]}))
    (is (= (is-tc-err-messages
             #(cc/for [{[a] :foo} [{:foo #{1}}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(typed.clojure/HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/for [{[a] :foo} [{:foo #{1}}]]
                            a)}]]})))
  (testing "doseq"
    (is (= (is-tc-err-messages
             #(cc/doseq [[a] [#{1}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(typed.clojure/HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/doseq [[a] [#{1}]]
                            a)}]]}))
    (is (= (is-tc-err-messages
             #(cc/doseq [{[a] :foo} [{:foo #{1}}]]
                a))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(typed.clojure/HSet #{1})"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/doseq [{[a] :foo} [{:foo #{1}}]]
                            a)}]]})))
  (testing "fn"
    (is (= (is-tc-err-messages
             (ann-form (cc/fn [[a]])
                       [(t/Set t/Any) -> t/Any]))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(IPersistentSet typed.clojure/Any)"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/fn [[a]])}]]})))
  (testing "defn"
    (is (= (is-tc-err-messages
             (do (t/ann foo [(t/Set t/Any) :-> t/Any])
                 (cc/defn foo [[a]])))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(IPersistentSet typed.clojure/Any)"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(cc/defn foo [[a]])}]]})))
  (testing "defmethod"
    (is (= (is-tc-err-messages
             (do (t/ann f [(t/Set t/Any) -> t/Any])
                 (defmulti f (fn [_] nil))
                 (defmethod f nil [[a]])))
           {:ex [[(ext-let/bad-vector-destructure-error-msg
                    "(IPersistentSet typed.clojure/Any)"
                    "[a]")
                  {:type-error :clojure.core.typed.errors/tc-error-parent
                   :form '(defmethod f nil [[a]])}]]})))
  )

(deftest inlined-nth-error-msg-test
  (is (= '(clojure.core/nth :a :a)
         ;; top-level
         (-> (is-tc-err-messages (clojure.core/nth :a :a))
             :ex
             first
             second
             :form)))
  (is (= '(clojure.core/nth :a :a)
         ;; non top-level
         (-> (is-tc-err-messages (let [x (clojure.core/nth :a :a)]
                                   x))
             :ex
             first
             second
             :form)))
  (is (= '(cc/nth (and :a :b) :a)
         ;; top-level
         (-> (is-tc-err-messages
               (do (alias 'cc 'clojure.core)
                   (cc/nth (and :a :b) :a)))
             :ex
             first
             second
             :form))))

;; note: the following fns intentionally omitted as they are annotated to accept all arguments
;; - clojure.core/get 
(deftest misc-inline-error-msg-test
  (testing "annotated inlined fns"
    (do-template [FORM] (testing 'FORM
                          (let [res (is-tc-err-messages
                                      (do (alias 'cc 'clojure.core)
                                          (fn [] FORM)))]
                            (is (= 'FORM
                                   (-> res
                                       :delayed-errors
                                       first
                                       second
                                       :form))
                                (pr-str res))))
                 (cc/zero? :a)
                 (cc/count :a)
                 (cc/int :a)
                 (cc/< :a :a)
                 (cc/inc :a)
                 (cc/inc' :a)
                 (cc/+ :a :a)
                 (cc/+' :a :a)
                 (cc/+ :a :a :a :a)
                 (cc/+' :a :a :a :a)
                 (cc/* :a :a)
                 (cc/*' :a :a)
                 (cc/* :a :a :a :a)
                 (cc/*' :a :a :a :a)
                 (cc// :a :a)
                 (cc// :a :a :a :a)
                 (cc/- :a :a)
                 (cc/-' :a :a)
                 (cc/- :a :a :a :a)
                 (cc/-' :a :a :a :a)
                 (cc/<= :a :a)
                 (cc/> :a :a)
                 (cc/>= :a :a)
                 (cc/== :a :a)
                 (cc/max :a :a)
                 (cc/max :a :a :a :a)
                 (cc/min :a :a)
                 (cc/min :a :a :a :a)
                 (cc/dec :a)
                 (cc/dec' :a)
                 (cc/unchecked-inc-int :a)
                 (cc/unchecked-inc :a)
                 (cc/unchecked-dec-int :a)
                 (cc/unchecked-dec :a)
                 (cc/unchecked-negate-int :a)
                 (cc/unchecked-negate :a)
                 (cc/unchecked-add-int :a :a)
                 (cc/unchecked-add :a :a)
                 (cc/unchecked-subtract-int :a :a)
                 (cc/unchecked-subtract :a :a)
                 (cc/unchecked-multiply-int :a :a)
                 (cc/unchecked-multiply :a :a)
                 (cc/unchecked-divide-int :a :a)
                 (cc/unchecked-remainder-int :a :a)
                 (cc/pos? :a)
                 (cc/neg? :a)
                 (cc/quot :a :a)
                 (cc/rem :a :a)
                 (cc/bit-not :a)
                 (cc/bit-and :a :a)
                 (cc/bit-and :a :a :a :a)
                 (cc/bit-or :a :a)
                 (cc/bit-or :a :a :a :a)
                 (cc/bit-xor :a :a)
                 (cc/bit-xor :a :a :a :a)
                 (cc/bit-and-not :a :a)
                 (cc/bit-and-not :a :a :a :a)
                 (cc/bit-shift-left :a :a)
                 (cc/bit-shift-right :a :a)
                 (cc/unsigned-bit-shift-right :a :a)
                 (cc/num :a)
                 (cc/long :a)
                 (cc/double :a)
                 (cc/char :a)
                 (cc/alength :a)
                 (cc/aclone :a)
                 (cc/aget :a :a)
                 (cc/aset :a :a)
                 (cc/byte-array :a)
                 (cc/byte-array :a :a)
                 (cc/char-array :a)
                 (cc/char-array :a :a)
                 (cc/short-array :a)
                 (cc/short-array :a :a)
                 (cc/double-array :a)
                 (cc/double-array :a :a)
                 (cc/int-array :a)
                 (cc/int-array :a :a)
                 (cc/boolean-array :a)
                 (cc/boolean-array :a :a)))
  (testing "unannotated inlined fns"
    (do-template [FORM] (testing 'FORM
                          (let [res (is-tc-err-messages
                                      (do (alias 'cc 'clojure.core)
                                          (fn [] FORM)))]
                            (is (= (first 'FORM)
                                   (-> res
                                       :delayed-errors
                                       first
                                       second
                                       :form))
                                (pr-str res))))
                 (cc/unchecked-char :a)
                 (cc/unchecked-short :a)
                 (cc/unchecked-double :a)
                 (cc/unchecked-byte :a)
                 (cc/unchecked-long :a)
                 (cc/unchecked-float :a)
                 (cc/unchecked-int :a)
                 (cc/float-array :a)
                 (cc/float-array :a :a)
                 (cc/object-array :a)
                 (cc/object-array :a :a)
                 (cc/long-array :a)
                 (cc/long-array :a :a)
                 (cc/booleans :a)
                 (cc/bytes :a)
                 (cc/chars :a)
                 (cc/shorts :a)
                 (cc/floats :a)
                 (cc/ints :a)
                 (cc/doubles :a)
                 (cc/longs :a))))
