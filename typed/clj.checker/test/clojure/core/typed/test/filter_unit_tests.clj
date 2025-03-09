(ns ^:typed.clojure clojure.core.typed.test.filter-unit-tests
  (:require [typed.clojure :as t]
            [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.filter-ops :refer :all]
            [typed.cljc.checker.path-rep :refer :all]
            [typed.cljc.checker.type-ctors :refer :all]))

(deftest refine-branch-test
  (is-tc-e (do
             (defalias M (t/U '{:a t/Int}
                              '{:a t/Sym}))
             (fn [a :- M]
               (if (symbol? (:a a))
                 (ann-form (:a a) t/Sym)
                 (ann-form (:a a) t/Int))))))

(deftest filter-or-test
  (is-tc-e (ann-form (fn [a]
                       (symbol? a))
                     [t/Any :-> t/Bool
                      :filters {:then (or (is t/Sym 0)
                                          (is t/Kw 0))}]))
  (is-tc-e (ann-form (fn [a]
                       (symbol? a))
                     [t/Any :-> t/Bool
                      :filters {:then (is (t/U t/Sym t/Kw) 0)}]))
  (is-tc-e (ann-form (fn [a]
                       (qualified-keyword? a))
                     [t/Any :-> t/Bool
                      :filters {:then (or (is t/Sym 0)
                                          (is t/Kw 0))}]))
  (is-tc-e (ann-form (fn [a]
                       (if (symbol? a)
                         true
                         (qualified-keyword? a)))
                     [t/Any :-> t/Bool
                      :filters {:then (or (is t/Sym 0)
                                          (is t/Kw 0))}]))
  (is-tc-e (fn [a]
             (or (symbol? a)
                 (qualified-keyword? a)))
           [t/Any :-> t/Bool
            :filters {:then (or (is t/Sym 0)
                                (is t/Kw 0))}])
  (is-tc-e (let [f (fn [a :- t/Any]
                     (or (symbol? a)
                         (qualified-keyword? a)))]
             f)
           [t/Any :-> t/Bool
            :filters {:then (is (t/U t/Sym t/Kw) 0)
                      :else (! t/Sym 0)}])
  (is-tc-err (let [f (fn [a :- t/Any]
                       (or (symbol? a)
                           (qualified-keyword? a)))]
               f)
             [t/Any :-> t/Bool
              :filters {:then (is t/Kw 0)
                        :else (! t/Sym 0)}])
  (is-tc-e (let [f (fn [a]
                     (or (symbol? a)
                         (qualified-keyword? a)))]
             f)
           [t/Any :-> t/Bool
            :filters {:then (is (t/U t/Sym t/Kw) 0)
                      :else (! t/Sym 0)}])
  (is-tc-err (let [f (fn [a]
                       (or (symbol? a)
                           (qualified-keyword? a)))]
               f)
             [t/Any :-> t/Bool
              :filters {:then (is t/Kw 0)
                        :else (! t/Sym 0)}])
  (is-tc-e (fn [a]
             (or (symbol? a)
                 (qualified-keyword? a)))
           [t/Any :-> t/Bool
            :filters {:then (is (t/U t/Sym t/Kw) 0)}]))
