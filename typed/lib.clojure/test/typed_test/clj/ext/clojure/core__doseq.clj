(ns ^:no-doc typed-test.clj.ext.clojure.core__doseq
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.core.typed :as t]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.test-utils :refer :all]))

(deftest doseq-test
  ; type checked
  (is-tc-e (cc/doseq [a (range 3)]
             (t/ann-form a t/Int))
           nil)
  (is-tc-err (cc/doseq [a (range 3)]
               (t/ann-form a t/Bool))
             nil)
  (is-tc-e (cc/doseq [e {1 2 3 4}]
             (t/ann-form e '[t/Int t/Int]))
           nil)
  ; no expected
  (let [{:keys [t]} (tc-e (cc/doseq [a (range 3)] (t/ann-form a t/Int)))]
    (is (subtype? (prs/parse-clj `nil) t))
    (is (subtype? t (prs/parse-clj `nil))))
  ;; multiple clauses
  ; type checked
  (is-tc-e (cc/doseq [a (range 3)
                      b (repeat 10 (inc a))]
             (+ a b))
           nil)
  (is-tc-err (cc/doseq [a (range 3)
                        b (repeat 10 (str (inc a)))]
               ;; b is Str, so type error
               (+ a b)))
  ;; non-seqable clause
  (is-tc-err (cc/doseq [a 1] a))
  ;; destructuring
  (is-tc-e (cc/doseq [[:as e] {1 2 3 4}]
             (t/ann-form e '[t/Int t/Int]))
           nil)
  (is-tc-e (cc/doseq [[a] {1 2 3 4}]
             (t/ann-form a t/Int))
           nil)
  (is-tc-e (cc/doseq [[a b] {1 2 3 4}]
             (t/ann-form [a b] '[t/Int t/Int]))
           nil)
  (is-tc-e (cc/doseq [{:keys [a]} [{:a 1}
                                   {:a 2}]]
             (t/ann-form a t/Int))
           nil)
  (is-tc-e (cc/doseq [[{:keys [a]}] [[{:a 1}]
                                     [{:a 2}]]]
             (t/ann-form a t/Int))
           nil)
  (is-tc-err (cc/doseq [[{:keys [a]}] [[{:a 1}]
                                       [{:a 2}]]]
               (t/ann-form a t/Bool))
             nil)
  (is-tc-e (cc/doseq [{:keys [a b]} (concat (repeat 20 {:a 1})
                                            (repeat 20 {:b 2}))]
             (t/ann-form [a b] '[(t/Option t/Int) 
                                 (t/Option t/Int)]))
           nil)
  (is-tc-err (cc/doseq [{:keys [a b]} (concat (repeat 20 {:a 1})
                                              (repeat 20 {:b 2}))]
               (t/ann-form [a b] '[t/Int 
                                   t/Int]))
             nil)
  ;; :when
  (is-tc-e (cc/doseq [{:keys [a b] :as c} (cons nil
                                                (concat (repeat 20 {:a 1})
                                                        (repeat 20 {:b 2})))
                      :when 1]
             (t/ann-form a (t/U nil t/Int))
             (t/ann-form b (t/U nil t/Int)))
           nil)
  (is-tc-e (cc/doseq [{:keys [a b] :as c} (cons nil
                                                (concat (repeat 20 {:a 1})
                                                        (repeat 20 {:b 2})))
                      ;; FIXME occurrence typing enhancement: `:when c` should be sufficient
                      :when (and a b c)]
             (+ a b))
           nil)
  (is-tc-e (cc/doseq [[a b] [[1 2] [:a :b]]
                      :when (false? :a)]
             ;; unreachable
             (inc nil))
           nil)
  ;; :while
  (is-tc-e (cc/doseq [{:keys [a b] :as c} (cons nil
                                                (concat (repeat 20 {:a 1})
                                                        (repeat 20 {:b 2})))
                      :while 1]
             (t/ann-form a (t/U nil t/Int))
             (t/ann-form b (t/U nil t/Int))))
  (is-tc-e (cc/doseq [{:keys [a b] :as c} (cons nil
                                                (concat (repeat 20 {:a 1})
                                                        (repeat 20 {:b 2})))
                      ;; FIXME occurrence typing enhancemnet: `:when c` should be sufficient
                      :while (and a b c)]
             (+ a b))
           nil)
  (is-tc-e (cc/doseq [[a b] [[1 2] [:a :b]]
                      :while (false? :a)]
             ;; unreachable
             (inc nil))
           nil)
  ;; :let
  (is-tc-e (cc/doseq [[a b] [[1 2]]
                      :let [a-old a
                            a b
                            b a-old]]
             (t/ann-form [a b] '[(t/Val 2) 
                                 (t/Val 1)]))
           nil)
  #_ ;;FIXME
  (is-tc-e (cc/doseq [[a b] [nil]
                    :let [_ (assert (= 42 a))]]
             (t/ann-form [a b] '[(t/Val 42) 
                                 nil]))
           nil)
  #_ ;;FIXME
  (is-tc-e (cc/doseq [[a b] [nil]
                    :let [a (or a 42)]]
             (t/ann-form [a b] '[(t/Val 42) 
                                 nil]))
           nil)
  #_ ;;FIXME
  (is-tc-e (cc/doseq [[a b] [[1 2]]
                    :let [a (or a 42)]]
             (t/ann-form [a b] '[(t/Val 42) 
                    nil]))
           )
  )
