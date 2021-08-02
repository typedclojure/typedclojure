(ns ^:no-doc typed-test.clj.ext.clojure.core
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.typed :as t]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.ext.clojure.core :as extcc]
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
  (is-tc-e (cc/for [{:keys [a]} [{:a 1}
                                 {:a 2}]]
             a)
           (t/Seq t/Int))
  (is-tc-e (cc/for [[{:keys [a]}] [[{:a 1}]
                                   [{:a 2}]]]
             a)
           (t/Seq t/Int))
  (is-tc-err (cc/for [[{:keys [a]}] [[{:a 1}]
                                     [{:a 2}]]]
               a)
             (t/Seq t/Bool))
  (is-tc-e (cc/for [{:keys [a b]} (concat (repeat 20 {:a 1})
                                          (repeat 20 {:b 2}))]
             [a b])
           (t/Seq '[(t/Option t/Int) 
                    (t/Option t/Int)]))
  (is-tc-err (cc/for [{:keys [a b]} (concat (repeat 20 {:a 1})
                                            (repeat 20 {:b 2}))]
               [a b])
             (t/Seq '[t/Int 
                      t/Int])))

(deftest let-test
  (is-tc-e (let [a 1] a) t/Int)
  (is-tc-e (let [{:keys [a]} {:a 1}] a) t/Int)
  (is-tc-err (let [{:keys [a]} {:a 1}] a) t/Bool)
  (is-tc-e (let [a (t/ann-form 1 (t/U nil t/Int))
                 _ (assert (integer? a))]
             a)
           t/Int)
  (is-tc-err (let [a (t/ann-form 1 (t/U nil t/Int))]
               a)
             t/Int))

(deftest common-destructuring-test
  ;; Note: different combinations of & and fixed vector args
  ;; generate very different code
  (is-tc-e (let [[a] [1]]
             (ann-form a Number)))
  (is-tc-e (let [[a b] [1 2]]
             (ann-form a Number)
             (ann-form b Number)))
  (is-tc-e (let [[& a] [1]]
             (ann-form a (Seq Number))))
  (is-tc-e (let [[a & b] [1 2]]
             (ann-form a Number)
             (ann-form b (Seq Number))))
  (is-tc-e (let [[a b & c] [1 2 3]]
             (ann-form a Number)
             (ann-form b Number)
             (ann-form c (Seq Number))))
  (is-tc-e (let [[a b c d e & r :as all] [1 2 3 4 5 6 7]]
             (ann-form b Number)
             (ann-form c Number)
             (ann-form d Number)
             (ann-form e Number)
             (ann-form r (Seqable Number))
             (ann-form all (Seqable Number))))
  (is-tc-e (seq [1 2 3])
           (HSeq [Num Num Num]))
  (is-tc-e
    '(1 2 3)
    (HSeq [Num Num Num]))
  (is-tc-e
    '(1 2 3)
    (HList [Num Num Num]))
  (is-tc-e
    (seq '(1 2 3))
    (HSeq [Num Num Num]))
  (is-tc-e
    (let [[a b c & d :as e] '(1 2 3 4 5 6 7)]
      (ann-form a (t/Val 1))
      (tc-ignore (assert (= 1 a) (pr-str a)))
      (ann-form b (t/Val 2))
      (tc-ignore (assert (= 2 b) (pr-str b)))
      (ann-form c (t/Val 3))
      (tc-ignore (assert (= 3 c) (pr-str c)))
      (ann-form d (t/HSeq [(t/Val 4) (t/Val 5) (t/Val 6) (t/Val 7)]))
      (tc-ignore (assert (seq? d) (pr-str d))
                 (assert (= [4 5 6 7] d) (pr-str d)))
      (ann-form e (t/HSeq [(t/Val 1) (t/Val 2) (t/Val 3)
                           (t/Val 4) (t/Val 5) (t/Val 6) (t/Val 7)]))
      (tc-ignore (assert (seq? e) (pr-str e))
                 (assert (= [1 2 3 4 5 6 7] e) (pr-str e)))))

  (is (t/check-ns 'typed-test.clj.ext.clojure.core.succeed.destructure)))

(deftest let-tag-test
  (is-tc-e (let [a "a"]
             (.startsWith a "b")))
  (is-tc-e (let [a (let [foo 1] "a")]
             (.startsWith a "b"))))

(deftest defmulti-expansion-test
  (is-tc-e (let [v (def some-var)]
             (when-not (and (.hasRoot v)
                            (instance? clojure.lang.MultiFn @v))
               (def some-var :multifn))))
  (is-tc-e (let [v (def some-var)]
             (when (and (.hasRoot v)
                        (instance? clojure.lang.MultiFn @v)))))
  (is-tc-e (let [v (def some-var)]
             (if (let [and__5531__auto__ (.hasRoot v)]
                   (if and__5531__auto__
                     (instance? clojure.lang.MultiFn @v)
                     and__5531__auto__))
               nil)))
  (is-tc-e (let [v (def some-var)]
             (if (let [and__5531__auto__ (.hasRoot v)]
                   (if and__5531__auto__
                     (instance? clojure.lang.MultiFn @v)
                     and__5531__auto__))
               nil)))
  (is-tc-e (let [v (def some-var)]
             (if
               ;; essential to erase v from the return type
               ;; of this expression
               (let [v (.hasRoot v)]
                 v)
               nil))))

(deftest let-occurrence-typing-test
  ;; unreachable branches
  (is-tc-e #(let [a (ann-form 'a Any)
                  _ (assert (symbol? a))
                  _ (assert (not (symbol? a)))]
              (/ nil nil)))
  (is-tc-e #(let [a (ann-form 'a Any)
                  _ (assert (and (symbol? a) (not (symbol? a))))]
              (/ nil nil)))
  (is-tc-e #(let [a (ann-form 'a Any)
                  _ (assert (and (symbol? a) (not (symbol? a))))
                  _ (/ nil nil)]))
  (is-tc-err #(let [a (ann-form 'a Any)
                    _ (/ nil nil)
                    _ (assert (and (symbol? a) (not (symbol? a))))]))
  ;; propagating objects
  (is-tc-e #(let [a (ann-form 1 Any)]
              (let [b a]
                (assert (number? b)))
              (ann-form a Number)))
  (is-tc-e #(let [a (ann-form 1 Any)]
              (let [b a]
                (assert (number? a)))
              (ann-form a Number)))
  (is-tc-e #(let [a (ann-form 1 Any)
                  _ (let [b a]
                      (assert (number? b)))]
              (ann-form a Number)))
  (is-tc-e #(let [a (ann-form 1 Any)
                  _ (let [b a]
                      (assert (number? a)))]
              (ann-form a Number)))
  (is-tc-err #(let [a (ann-form 1 Any)]
                (let [b a]
                  (assert (not (number? b))))
                (ann-form a Number)))
  (is-tc-err #(let [a (ann-form 1 Any)
                    _ (let [b a]
                        (assert (not (number? b))))]
                (ann-form a Number)))
  ;; propagating complicated objects
  (is-tc-e #(let [m {:a (ann-form 1 Any)}]
              (let [b (:a m)]
                (assert (number? b)))
              (ann-form (:a m) Number)))
  (is-tc-e #(let [m {:a (ann-form 1 Any)}
                  _ (let [b (:a m)]
                      (assert (number? b)))]
              (ann-form (:a m) Number)))
  (is-tc-err #(let [m {:a (ann-form 1 Any)}]
                (let [b (:a m)]
                  (assert (not (number? b))))
                (ann-form (:a m) Number)))
  (is-tc-err #(let [m {:a (ann-form 1 Any)}
                    _ (let [b (:a m)]
                        (assert (not (number? b))))]
                (ann-form (:a m) Number)))
  ;; erased/uniquified shadowed bindings
  (is-tc-err #(let [a (ann-form 1 Any)
                    _ (let [a (ann-form 1 Number)]
                        a)]
                (ann-form a Number)))
  (is-tc-e #(let [; test if aliasing gets confused
                  m {:a (ann-form 1 Any)}
                  a (:a m)
                  m {:a (ann-form 1 Any)}
                  _ (assert (number? a))]
              (ann-form a Number)))
  (is-tc-err #(let [m {:a (ann-form 1 Any)}
                    a (:a m)
                    m {:a (ann-form 1 Any)}
                    _ (assert (number? a))]
                (ann-form (:a m) Number)))
  ;; uniquify let+do (gilardi scenario)
  (is-tc-err (do (let [m (ann-form 1 Any)]
                   (assert (number? m))
                   m)
                 (let [m (ann-form 1 Any)]
                   (ann-form m Number))))
  ;; uniquify let+do (non-gilardi scenario)
  (is-tc-err #(do (let [m (ann-form 1 Any)]
                    (assert (number? m))
                    m)
                  (let [m (ann-form 1 Any)]
                    (ann-form m Number)))))

(deftest vector-destructure-error-msg-test
  (is (= (is-tc-err-messages
           #(let [[a] #{1}]
              a))
         {:ex [[(extcc/bad-vector-destructure-error-msg
                  "(HSet #{1})"
                  "[a]")
                {:type-error :clojure.core.typed.errors/tc-error-parent
                 :form '(clojure.core/let [[a] #{1}]
                          a)}]]}))
  (is (= (is-tc-err-messages
           #(let [{[a] :foo} {:foo #{1}}]
              a))
         {:ex [[(extcc/bad-vector-destructure-error-msg
                  "(HSet #{1})"
                  "[a]")
                {:type-error :clojure.core.typed.errors/tc-error-parent
                 :form '(clojure.core/let [{[a] :foo} {:foo #{1}}]
                          a)}]]}))
  (is (= (is-tc-err-messages
           #(cc/for [[a] [#{1}]]
              a))
         {:ex [[(extcc/bad-vector-destructure-error-msg
                  "(HSet #{1})"
                  "[a]")
                {:type-error :clojure.core.typed.errors/tc-error-parent
                 :form '(cc/for [[a] [#{1}]]
                          a)}]]}))
  (is (= (is-tc-err-messages
           #(cc/for [{[a] :foo} [{:foo #{1}}]]
              a))
         {:ex [[(extcc/bad-vector-destructure-error-msg
                  "(HSet #{1})"
                  "[a]")
                {:type-error :clojure.core.typed.errors/tc-error-parent
                 :form '(cc/for [{[a] :foo} [{:foo #{1}}]]
                          a)}]]}))
  (is (= (is-tc-err-messages
           #(fn [[a] :- (t/Set t/Any)]))
         {:ex [[(extcc/bad-vector-destructure-error-msg
                  "(IPersistentSet Any)"
                  "[a]")
                {:type-error :clojure.core.typed.errors/tc-error-parent
                 ;; FIXME even better form
                 :form '(clojure.core/fn ([[a]]))}]]}))
  (is (= (is-tc-err-messages
           (ann-form (cc/fn [[a]])
                     [(t/Set t/Any) -> t/Any]))
         {:ex [[(extcc/bad-vector-destructure-error-msg
                  "(IPersistentSet Any)"
                  "[a]")
                {:type-error :clojure.core.typed.errors/tc-error-parent
                 :form '(cc/fn [[a]])}]]}))
  )
