(ns typed-test.clj.checker.check
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
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

(deftest meta-ann-test
  (is-tc-e (inc 1)
           t/Any)
  (is-tc-e ^::t/ignore (inc 1)
           t/Any)
  (is-tc-err ^::t/ignore (inc 1)
             t/Int)
  (is-tc-err (inc ^::t/ignore (do 1)))
  (is-tc-err #(do (inc nil)))
  (is-tc-e #(do ^::t/ignore (inc nil)))
  (is-tc-e (#(do ^::t/ignore (inc %)) 1))
  (is-tc-err #(inc nil))
  (is-tc-e ^::t/ignore #(inc nil))
  (is-tc-err (^::t/ignore #(inc %) 1))
  (is-tc-e (inc ^{::t/unsafe-cast t/Int} (do 1)))
  (is-tc-err (inc ^{::t/unsafe-cast nil} (do 1)))
  (is-tc-e ^{::t/unsafe-cast nil} (do 1) nil)
  (is-tc-err ^{::t/unsafe-cast nil} (do 1) t/Int)
  (is-tc-err ^{::t/unsafe-cast nil} #(inc nil))
  (is-tc-e ^{::t/unsafe-cast nil}
           ^::t/ignore
           #(inc nil))
  (is-tc-e ^{::t/unsafe-cast nil}
           ^::t/ignore
           #(inc nil)
           nil)
  (is-tc-e ^{::t/- t/Any} (do 1))
  (is-tc-err ^::t/dbg ^{::t/- t/Any} (do 1) t/Int)
  (is-tc-err ^{::t/dbg "dbg string"} ^{::t/- t/Any} (do 1) t/Int)
  (is-tc-err ^::t/dbg ^{::t/- t/Any} (do ^::t/dbg (do 1)) t/Int)
  (is-tc-err (inc ^{::t/- t/Any} (do 1)))
  (is-tc-e #(inc ^{::t/unsafe-cast ^:fake-quote 't/Int} (do nil)))
  (is-tc-e (identity 1) t/Int)
  (is-tc-e (^{::t/inst [t/Int]} identity 1) t/Int)
  (is-tc-err (^{::t/inst [t/Num]} identity 1) t/Int)
  (is-tc-err (^{::t/inst [t/Num]} identity nil))
  (is-tc-e ^::t/dbg identity)
  (is-tc-err ^::t/dbg (do nil) t/Int)
  (is-tc-err ^::t/dbg (do ^::t/dbg (do nil)) t/Int)
  (is-tc-e (do (t/ann a t/Int)
               ^::t/ignore (def a 1)
               a))
  ;; evaluates once
  (is-tc-e (do ^::t/ignore (def a (atom 0))
               ^::t/ignore (swap! a inc)
               ^::t/ignore (swap! a inc)
               ^::t/ignore (assert (= 2 @a))))
  ;; evaluates once
  (is-tc-e (do ^::t/ignore (def a (atom 0))
               ^{::t/- t/Any} ^::t/dbg ^::t/ignore (swap! a inc)
               ^{::t/- t/Any} ^::t/dbg ^::t/ignore (swap! a inc)
               ^::t/ignore (assert (= 2 @a)))))
