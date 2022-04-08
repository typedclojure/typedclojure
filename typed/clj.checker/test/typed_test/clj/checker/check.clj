(ns typed-test.clj.checker.check
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :refer :all]))

(deftest instance?-test
  (is-tc-err #(let [Class 1]
                (instance? Class 1))))

(deftest satisfies?-test
  (is-tc-e (do (defprotocol A)
               (ann-form
                 (fn [a]
                   (satisfies? A a))
                 [t/Any :-> t/Bool :filters {:then (is A 0)
                                             :else (! A 0)}])))
  ;; no :else filter for :extend-via-metadata
  (is-tc-err (do (defprotocol A
                   :extend-via-metadata true)
                 (ann-form
                   (fn [a]
                     (satisfies? A a))
                   [t/Any :-> t/Bool :filters {:then (is A 0)
                                               :else (! A 0)}])))
  (is-tc-e (do (defprotocol A
                 :extend-via-metadata true)
               (ann-form
                 (fn [a]
                   (satisfies? A a))
                 [t/Any :-> t/Bool :filters {:then (is A 0)}])))
  (is-tc-e (do (defprotocol A)
               (ann-form
                 #(let [a (ann-form (reify A) (t/Nilable A))]
                    (assert (satisfies? A a))
                    a)
                 [:-> A])))
  (is-tc-e (do (defprotocol A)
               (ann-form
                 #(let [a (ann-form (reify A) (t/Nilable A))]
                    (assert (not (satisfies? A a)))
                    a)
                 [:-> nil])))
  (is-tc-e (do (defprotocol A)
               (extend-type nil A)
               (ann-form
                 #(let [a (ann-form (reify A) (t/Nilable A))]
                    (assert (not (satisfies? A a)))
                    a)
                 [:-> t/Nothing])))
  (is-tc-err (do (defprotocol A)
                 (ann-form
                   #(let [a (ann-form (reify A) (t/Nilable A))]
                      (assert (not (satisfies? A a)))
                      a)
                   [:-> t/Nothing])))
  (is-tc-err (do (defprotocol A)
                 (extend-type nil A)
                 (ann-form
                   #(let [a (ann-form (reify A) (t/Nilable A))]
                      (if (satisfies? A a)
                        (ann-form a A)
                        (ann-form a nil)))
                   [:-> nil])))
  (is-tc-e (do (defprotocol A)
               (extend-type nil A)
               #(let [a (ann-form (reify A) A)]
                  (assert a)
                  (ann-form a A))))
  (is-tc-e (do (defprotocol A)
               (extend-type nil A)
               #(let [a (ann-form (reify A) A)]
                  (assert (not a))
                  (ann-form a nil))))
  (is-tc-err (do (defprotocol A)
                 (extend-type nil A)
                 #(let [a (ann-form (reify A) A)]
                    (assert a)
                    (ann-form a nil))))
  (is-tc-err (do (defprotocol A)
                 (extend-type nil A)
                 #(let [a (ann-form (reify A) A)]
                    (assert (not a))
                    (ann-form a Object)))))
