(ns ^:no-doc typed-test.clj.ext.clojure.core__let
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.typed :as t]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :refer :all]))

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

  (is (t/check-ns 'typed-test.clj.ext.clojure.core.succeed.destructure))
  ;;TODO improve inference
  #_
  (is-tc-e (let [[a b] nil] [a b])
           '[nil nil])
  #_ ;;FIXME
  (is-tc-e (let [;; erase object
                 [a b] (t/ann-form nil nil)
                 a (or a 42)]
             a)
           (t/Val 42)))

(deftest let-tag-test
  (is-tc-e (let [a "a"]
             (.startsWith a "b")))
  (is-tc-e (let [a (let [foo 1] "a")]
             (.startsWith a "b")))
  ;; ensure inferred :tag is not propagated to final expansion.
  ;; this would otherwise fail to expand, complaining about primitive
  ;; type hints on locals.
  (is-tc-e (let [foo 1])))

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
