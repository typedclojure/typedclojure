(ns ^:typed.clojure ^:no-doc typed-test.clj.ext.clojure.core__for
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :as clj]
            [typed.cljs.checker.test-utils :as cljs]))

(defn subtype? [s t]
  (and (clj/subtype? s t)
       #_
       (cljs/subtype? s t)))

(defmacro tc-e [& args]
  `(do (clj/tc-e ~@args)
       #_
       (cljs/tc-e ~@args)))

(defmacro is-tc-e [& args]
  `(do (clj/is-tc-e ~@args)
       #_
       (cljs/is-tc-e ~@args)))

(defmacro is-tc-err [& args]
  `(do (clj/is-tc-err ~@args)
       #_
       (cljs/is-tc-err ~@args)))

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
                      t/Int]))
  ;; :when
  (is-tc-e (cc/for [{:keys [a b] :as c} (cons nil
                                              (concat (repeat 20 {:a 1})
                                                      (repeat 20 {:b 2})))
                    :when 1]
             [a b])
           (t/Seq '[(t/Option t/Int) 
                    (t/Option t/Int)]))
  (is-tc-e (cc/for [{:keys [a b] :as c} (cons nil
                                              (concat (repeat 20 {:a 1})
                                                      (repeat 20 {:b 2})))
                    ;; FIXME occurrence typing enhancement: `:when c` should be sufficient
                    :when (and a b c)]
             [a b])
           (t/Seq '[t/Int 
                    t/Int]))
  (is-tc-e (cc/for [[a b] [[1 2] [:a :b]]
                    :when (false? :a)]
             [a b])
           (t/Seq '[t/Int 
                    t/Int]))
  ;; :while
  (is-tc-e (cc/for [{:keys [a b] :as c} (cons nil
                                              (concat (repeat 20 {:a 1})
                                                      (repeat 20 {:b 2})))
                    :while 1]
             [a b])
           (t/Seq '[(t/Option t/Int) 
                    (t/Option t/Int)]))
  (is-tc-e (cc/for [{:keys [a b] :as c} (cons nil
                                              (concat (repeat 20 {:a 1})
                                                      (repeat 20 {:b 2})))
                    ;; FIXME occurrence typing enhancemnet: `:when c` should be sufficient
                    :while (and a b c)]
             [a b])
           (t/Seq '[t/Int 
                    t/Int]))
  (is-tc-e (cc/for [[a b] [[1 2] [:a :b]]
                    :while (false? :a)]
             [a b])
           (t/Seq '[t/Int 
                    t/Int]))
  ;; :let
  (is-tc-e (cc/for [[a b] [[1 2]]
                    :let [a-old a
                          a b
                          b a-old]]
             [a b])
           (t/Seq '[(t/Val 2) 
                    (t/Val 1)]))
  #_ ;;FIXME
  (is-tc-e (cc/for [[a b] [nil]
                    :let [_ (assert (= 42 a))]]
             [a b])
           (t/Seq '[(t/Val 42) 
                    nil]))
  #_ ;;FIXME
  (is-tc-e (cc/for [[a b] [nil]
                    :let [a (or a 42)]]
             [a b])
           (t/Seq '[(t/Val 42) 
                    nil]))
  #_ ;;FIXME
  (is-tc-e (cc/for [[a b] [[1 2]]
                    :let [a (or a 42)]]
             [a b])
           (t/Seq '[(t/Val 42) 
                    nil]))
  )
