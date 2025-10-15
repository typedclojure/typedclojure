(ns ^:typed.clojure clojure.core.typed.test.proposition-expected
  (:refer-clojure :exclude [cast])
  (:require 
    ; this loads the type system, must go first
    [typed.clj.checker.test-utils :refer :all]
    [typed.cljc.checker.type-rep :refer :all]
    [typed.clj.checker.parse-unparse :refer :all]
    [typed.cljc.checker.proposition-rep :refer :all]
    [typed.cljc.checker.proposition-ops :refer :all]
    [typed.cljc.checker.object-rep :refer :all]
    [typed.cljc.checker.path-rep :refer :all]
    [clojure.test :refer :all])
  (:use [clojure.core.typed :as t :exclude [Seqable loop fn defprotocol let dotimes
                                            for doseq def remove filter defn atom ref]]))

(deftest filter-expected-test
  (testing "integers are truthy"
    (is-tc-e 1
             :expected-ret (ret (parse-clj `t/Num)
                                (-FS -top -bot)))
    (is-tc-err 1 
               :expected-ret (ret (parse-clj `t/Num)
                                  (-FS -bot -top)
                                  -empty))
    (is-tc-err 1 
               :expected-ret (ret (parse-clj `t/Num)
                                  (-FS -bot -bot)
                                  -empty))
    (is-tc-err 1 
               :expected-ret (ret (parse-clj `t/Num)
                                  (-FS -top -top)
                                  (-path nil 'a)))
    (is-tc-err 1
               :expected-ret (ret (parse-clj `t/Sym)))
    (is (= (ret (parse-clj `t/Num)
                (-true-proposition))
           (tc-e 1
                 :expected-ret
                 (ret (parse-clj `t/Num)
                      (-true-proposition)
                      -no-object)))))
  (testing "nil is falsy"
    (is-tc-e nil
             :expected-ret (ret -nil
                                (-FS -bot -top)))
    (is-tc-err nil
               :expected-ret (ret -nil
                                  (-FS -bot -top)
                                  (-path nil 'a)))
    (is-tc-err nil
               :expected-ret (ret -nil
                                  (-FS -top -bot))))
  (testing "false is falsy"
    (is-tc-e false
             :expected-ret (ret -false
                                (-FS -bot -top)))
    (is-tc-err false
               :expected-ret (ret -false
                                  (-FS -bot -top)
                                  (-path nil 'a)))
    (is-tc-e false
             :expected-ret (ret -false
                                (-FS -bot -top)))
    (is-tc-err false
               :expected-ret (ret -false
                                  (-FS -top -bot))))
  (testing "conditionals"
    (is-tc-e (if 1 2 3) t/Num)
    (is-tc-err (if 1 2 3) t/Sym)
    (is-tc-e (if 1 2 3) 
             :expected-ret (ret (parse-clj `t/Num)
                                (-FS -top -bot)))
    (is-tc-err (if 1 2 3) 
               :expected-ret (ret (parse-clj `t/Num)
                                  (-FS -top -bot)
                                  (-path nil 'a)))
    (is-tc-e (if 1 nil nil) 
             :expected-ret (ret (parse-clj `nil)
                                (-FS -bot -top)))
    (is-tc-err (if 1 2 3) 
               :expected-ret (ret (parse-clj `t/Num)
                                  (-FS -bot -top)))
    (is-tc-e (fn [a] (if a nil nil))
             [t/Any -> nil :filters {:then ff :else tt}])
    (is-tc-e (fn [a b] (if a a a))
             [t/Any t/Any -> t/Any :object {:id 0}])
    (is-tc-err (fn [a b] (if b b b))
               [t/Any t/Any -> t/Any :object {:id 0}]))
  (testing "functions are truthy"
    (is-tc-e (fn [])
             :expected-ret (ret -any
                                (-true-proposition)))
    (is-tc-err (fn [])
               :expected-ret (ret -any
                                  (-true-proposition)
                                  (-path nil 'a)))
    (is-tc-err (fn [])
             :expected-ret (ret -any
                                (-false-proposition)))
    ;TODO
    #_(is-tc-e (core/fn [])
             :expected-ret (ret -any
                                (-false-proposition))))
  (testing "quote"
    (is-tc-e 'a 
             :expected-ret
             (ret (parse-clj `t/Sym)
                  (-true-proposition)))
    (is-tc-err 'a 
               :expected-ret
               (ret (parse-clj `t/Sym)
                    (-false-proposition)))
    (is-tc-err 'a 
               :expected-ret
               (ret (parse-clj `t/Sym)
                    (-true-proposition)
                    (-path nil 'a)))
    (is-tc-e ''a 
             :expected-ret
             (ret (parse-clj `(t/Coll t/Sym))
                  (-true-proposition)))
    (is-tc-e '''a 
             :expected-ret
             (ret (parse-clj `(t/Coll (t/U t/Sym (t/Coll t/Sym))))
                  (-true-proposition))))
  (testing "do"
    (is-tc-e (do 1 2)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-true-proposition)))
    (is-tc-err (do 1 2)
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-true-proposition)
                    (-path nil 'a)))
    (is-tc-e #(do 1 (throw (Exception.)) 2)
             :expected-ret
             (ret (parse-clj `[:-> t/Nothing
                               :filters {:then ~'ff :else ~'ff}])))
    (is-tc-err (do 1 2)
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-false-proposition)))
    (is-tc-e #(do nil %)
             :expected-ret
             (ret (parse-clj `[t/Num :-> t/Num :object {:id 0}])))
    (is-tc-e #(do nil %2)
             :expected-ret
             (ret (parse-clj `[t/Num t/Num :-> t/Num :object {:id 1}])))
    (is-tc-err #(do nil %2)
               :expected-ret
               (ret (parse-clj `[t/Num t/Num :-> t/Num :object {:id 0}]))))
  (testing "let"
    (is-tc-e (let [] 1)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-true-proposition)))
    (is-tc-err (let [] 1)
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-false-proposition)))
    (is-tc-err (let [] 1)
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "map values"
    (is-tc-e {:a 1}
             :expected-ret
             (ret (parse-clj `'{:a t/Num})
                  (-true-proposition)))
    (is-tc-err {:a 1}
               :expected-ret
               (ret (parse-clj `'{:a t/Num})
                    (-false-proposition)))
    (is-tc-err {:a 1}
               :expected-ret
               (ret (parse-clj `'{:a t/Num})
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "map expressions"
    (is-tc-e (let [a 1] {:a a})
             :expected-ret
             (ret (parse-clj `'{:a t/Num})
                  (-true-proposition)))
    (is-tc-err (let [a 1] {:a a})
               :expected-ret
               (ret (parse-clj `'{:a t/Num})
                    (-false-proposition)))
    (is-tc-err (let [a 1] {:a a})
               :expected-ret
               (ret (parse-clj `'{:a t/Num})
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "set values"
    (is-tc-e #{1})
    (is-tc-e #{1} (t/Set t/Num))
    (is-tc-err #{1} 
               (t/Set t/Sym))
    (is-tc-e #{1} 
             :expected-ret
             (ret (parse-clj `(t/Set t/Num))
                  (-true-proposition)))
    (is-tc-err #{1} 
               :expected-ret
               (ret (parse-clj `(t/Set t/Num))
                    (-false-proposition)))
    (is-tc-err #{1} 
               :expected-ret
               (ret (parse-clj `(t/Set t/Num))
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "set expression"
    (is-tc-e (let [a 1] #{a}))
    (is-tc-e (let [a 1] #{a})
             (t/Set t/Num))
    (is-tc-err (let [a 1] #{a})
               (t/Set t/Sym))
    (is-tc-e (let [a 1] #{a})
             :expected-ret
             (ret (parse-clj `(t/Set t/Num))
                  (-true-proposition)))
    (is-tc-err (let [a 1] #{a})
               :expected-ret
               (ret (parse-clj `(t/Set t/Num))
                    (-false-proposition)))
    (is-tc-err (let [a 1] #{a})
               :expected-ret
               (ret (parse-clj `(t/Set t/Num))
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "vector values"
    (is-tc-e [1])
    (is-tc-e [1] (t/Vec t/Num))
    (is-tc-err [1] (t/Vec t/Sym))
    (is-tc-e [1]
             :expected-ret
             (ret (parse-clj `(t/Vec t/Num))
                  (-true-proposition)))
    (is-tc-err [1]
               :expected-ret
               (ret (parse-clj `(t/Vec t/Num))
                    (-false-proposition)))
    (is-tc-err [1]
               :expected-ret
               (ret (parse-clj `(t/Vec t/Num))
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "vector expressions"
    (is-tc-e (let [a 1] [a]))
    (is-tc-e (let [a 1] [a]) (t/Vec t/Num))
    (is-tc-err (let [a 1] [a]) (t/Vec t/Sym))
    (is-tc-e (let [a 1] [a])
             :expected-ret
             (ret (parse-clj `(t/Vec t/Num))
                  (-true-proposition)))
    (is-tc-err (let [a 1] [a])
               :expected-ret
               (ret (parse-clj `(t/Vec t/Num))
                    (-false-proposition)))
    (is-tc-err (let [a 1] [a])
               :expected-ret
               (ret (parse-clj `(t/Vec t/Num))
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "ann-form"
    (is-tc-e (ann-form 1 t/Num)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-true-proposition)
                  -empty))
    (is-tc-err (ann-form 1 t/Num)
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-false-proposition))))
  (testing "loop"
    (is-tc-e (loop [a :- t/Num 1] a)
             :expected-ret
             (ret (parse-clj `t/Num)))
    (is-tc-e (loop [a :- t/Num 1] a)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-true-proposition)))
    (is-tc-err (loop [a :- t/Num 1] a)
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-false-proposition)))
    ;TODO better gensyms?
    #_(is-tc-err (loop [a :- t/Num 1] a)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-FS -top -top)
                  (-path nil 'a__#0))))
  (testing "application"
    (is-tc-e ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)))
    (is-tc-err ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)
                  (-true-proposition)))
    (is-tc-e ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)
                  (-false-proposition)))
    (is-tc-err ((fn []))
             :expected-ret
             (ret (parse-clj 'nil)
                  (-false-proposition)
                  (-path nil 'a))))
  (testing "instance method"
    (is-tc-e (.getParent (java.io.File. "a"))
             :expected-ret
             (ret (parse-clj `(t/U nil t/Str))))
    (is-tc-err (.getParent (java.io.File. "a"))
               :expected-ret
               (ret (parse-clj `(t/U nil t/Str))
                    (-true-proposition)))
    (is-tc-err (.getParent (java.io.File. "a"))
               :expected-ret
               (ret (parse-clj `(t/U nil t/Str))
                    (-false-proposition)))
    (is-tc-err (.getParent (java.io.File. "a"))
               :expected-ret
               (ret (parse-clj `(t/U nil t/Str))
                    (-FS -top -top)
                    (-path nil 'a))))
  (testing "static fields"
    (is-tc-e Long/SIZE
             :expected-ret
             (ret (parse-clj `t/Num)))
    (is-tc-err Long/SIZE
               :expected-ret
               (ret (parse-clj `t/Sym)))
    (is-tc-err Long/SIZE
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-false-proposition)))
    (is-tc-err Long/SIZE
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-true-proposition)))
    (is-tc-err Long/SIZE
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-FS -top -top)
                    (-path nil 'a))))
  (testing "instance fields"
    (is-tc-e (do (ann-datatype A [a :- t/Num])
                 (deftype A [a])
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `t/Num)))
    ; ctor call in method
    (is-tc-e (do (ann-datatype A [a :- t/Num])
                 (deftype A [a]
                   Object
                   (toString [this]
                     (A. 1)
                     "foo"))
                 (.a (A. 1)))
             :expected-ret
             (ret (parse-clj `t/Num)))
    (is-tc-err (do (ann-datatype A [a :- t/Num])
                   (deftype A [a])
                   (.a (A. 1)))
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-true-proposition)))
    (is-tc-err (do (ann-datatype A [a :- t/Num])
                   (deftype A [a])
                   (.a (A. 1)))
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-false-proposition)))
    (is-tc-err (do (ann-datatype A [a :- t/Num])
                   (deftype A [a])
                   (.a (A. 1)))
               :expected-ret
               (ret (parse-clj `t/Num)
                    (-FS -top -top)
                    (-path nil 'a))))
  (testing "static methods"
    (is-tc-e (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(t/U nil t/Num))))
    (is-tc-err (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(t/U nil t/Num))
                  (-true-proposition)))
    (is-tc-err (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(t/U nil t/Num))
                  (-false-proposition)))
    (is-tc-err (Long/valueOf 1)
             :expected-ret
             (ret (parse-clj `(t/U nil t/Num))
                  (-FS -top -top)
                  (-path nil 'a))))
  (testing "instance? call"
    (is-tc-e (instance? Long 1)
             Boolean)
    ;TODO scoping
    #_(is-tc-err (let [a 1]
                 (instance? Long a))
               :expected-ret
               (ret (parse-clj `Boolean)
                    (-FS (-proposition (parse-clj `Long)
                                  'a__#0)
                         (-not-proposition (parse-clj `Long)
                                  'a__#0))))
    (is-tc-e (fn [a] (instance? Long a))
             (t/Pred Long)))
  (testing "multifn"
    ;FIXME
    #_(is-tc-e (clojure.lang.MultiFn. 'foo
                                    class
                                    :default
                                    #'clojure.core/global-hierarchy)
             :expected-ret
             (ret (parse-clj `[t/Any :-> t/Any]))))
  (testing "new"
    (is-tc-e (Boolean. true)
             Boolean)
    (is-tc-e (Boolean. true)
             :expected-ret
             (ret (parse-clj `Boolean)
                  (-true-proposition)))
    (is-tc-err (Boolean. true)
             :expected-ret
             (ret (parse-clj `Boolean)
                  (-false-proposition)))
    (is-tc-err (Boolean. true)
             :expected-ret
             (ret (parse-clj `Boolean)
                  (-true-proposition)
                  (-path nil 'a))))
  (testing "throw"
    (is-tc-e (fn [a :- Throwable] :- t/Nothing
               (throw a)))
    (is-tc-err (fn [a :- t/Any]
                 (throw a)))
    (is-tc-e (fn [a]
               (throw a))
             [Throwable -> t/Nothing
              :filters {:then ff :else ff}])
    (is-tc-err (fn [a]
                 1)
               [Throwable -> t/Nothing
                :filters {:then ff :else ff}])
    (is-tc-e (fn [a]
               (throw a))
             [Throwable -> t/Nothing])
    (is-tc-e (fn [a]
               (throw a))
             [Throwable -> t/Nothing
              :filters {:then ff :else ff}])
    (is-tc-err (fn [a] 1)
               [Throwable -> t/Any
                :filters {:then ff :else ff}])
    (is-tc-e (core/fn [a]
               (throw a))
             [Throwable -> t/Nothing
              :filters {:then ff :else ff}])
    (is-tc-e (fn [a :- Throwable]
               (throw a))
             [Throwable -> t/Nothing
              :filters {:then ff :else ff}])
    (is-tc-err (core/fn [a] 1)
             [Throwable -> t/Any
              :filters {:then ff :else ff}]))
  (testing "try catch"
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e))
             nil)
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e))
             t/Num)
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e
                    2))
             t/Num)
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e))
             :expected-ret
             (ret -nil
                  (-false-proposition)))
    (is-tc-err (try (throw (Exception.))
                    (catch Exception e))
               :expected-ret
               (ret -nil
                    (-true-proposition))))
  (testing "finally"
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             t/Num)
    (is-tc-err (try (throw (Exception.))
                    (catch Exception e
                      2)
                    (finally nil))
               nil)
    (is-tc-e (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-true-proposition)))
    (is-tc-err (try (throw (Exception.))
                  (catch Exception e
                    2)
                  (finally nil))
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-false-proposition))))
  (testing "var"
    (is-tc-e (do (t/def foo :- t/Num 1)
                 foo))
    (is-tc-e (do (t/def foo :- t/Num 1)
                 foo)
             t/Num)
    (is-tc-err (do (t/def foo :- t/Num 1)
                   foo)
               nil))
  (testing "set!"
    (is-tc-e (do (t/def ^:dynamic *foo* :- Number 1)
                 (binding [*foo* 1]
                   (set! *foo* 2))))
    (is-tc-err (do (t/def ^:dynamic *foo* :- Number 1)
                   (binding [*foo* 1]
                     (set! *foo* nil))))
    (is-tc-e (do (t/def ^:dynamic *foo* :- Number 1)
                 (binding [*foo* 1]
                   (set! *foo* 2)))
             t/Num)
    (is-tc-err (do (t/def ^:dynamic *foo* :- Number 1)
                   (binding [*foo* 1]
                     (set! *foo* 2)))
             nil))
  (testing "the var"
    (is-tc-e (do (t/def foo :- t/Num 1)
                 #'foo))
    (is-tc-e (do (t/def foo :- t/Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(t/Var t/Num))))
    (is-tc-e (do (t/def foo :- t/Num 1)
                 #'foo)
             :expected-ret
             (ret (parse-clj `(t/Var t/Num))
                  (-true-proposition)))
    (is-tc-err (do (t/def foo :- t/Num 1)
                   #'foo)
               :expected-ret
               (ret (parse-clj `(t/Var t/Num))
                    (-false-proposition)))
    (is-tc-err (do (t/def foo :- t/Num 1)
                   #'foo)
               :expected-ret
               (ret (parse-clj `(t/Var t/Num))
                    (-true-proposition)
                    (-path nil 'a))))
  (testing "cast"
    (is-tc-e (core/cast Number 1))
    (is-tc-e (core/cast Number 1)
             t/Num)
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-true-proposition)))
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-false-proposition)))
    (is-tc-err (core/cast Number 1)
             :expected-ret
             (ret (parse-clj `t/Num)
                  (-FS -top -top)
                  (-path nil 'a))))
  (testing "tc-ignore"
    (is-tc-e (tc-ignore 1)
             t/Any)
    (is-tc-err (tc-ignore 1)
             t/Num)
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `t/Any)
                    (-true-proposition)))
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `t/Any)
                    (-false-proposition)))
    (is-tc-err (tc-ignore 1)
               :expected-ret
               (ret (parse-clj `t/Any)
                    (-FS -top -top)
                    (-path nil 'a)))
    (is-tc-e (tc-ignore 1)
             :expected-ret
             (ret (parse-clj `t/Any)
                  (-FS -top -top)
                  -empty)))
  (testing "local"
    (is-tc-e (let [a 1] a))
    (is-tc-e (let [a 1] a) t/Num)
    (is-tc-err (let [a 1] a) t/Sym)
    (is-tc-e (let [a 1] a)
             :expected-ret
             (ret (parse-clj `t/Any)
                  (-true-proposition)))
    (is-tc-err (let [a 1] a)
               :expected-ret
               (ret (parse-clj `t/Any)
                    (-false-proposition)))
    (is-tc-err (let [a 1] a)
               :expected-ret
               (ret (parse-clj `t/Any)
                    (-FS -top -top)
                    (-path nil 'a))))
  (testing "monitor-enter"
    (is-tc-e #(monitor-enter 1))
    (is-tc-err #(monitor-enter nil))
    (is-tc-e #(monitor-enter 1)
             [-> nil :filters {:then ff :else tt}])
    (is-tc-err #(monitor-enter 1)
               [-> nil :filters {:then tt :else ff}])
    (is-tc-err (fn [a] (monitor-enter 1))
               [t/Any -> nil :filters {:then ff :else tt} :object {:id 0}]))
  (testing "monitor-exit"
    (is-tc-e #(monitor-exit 1))
    (is-tc-err #(monitor-exit nil))
    (is-tc-e #(monitor-exit 1)
             [-> nil :filters {:then ff :else tt}])
    (is-tc-err #(monitor-exit 1)
               [-> nil :filters {:then tt :else ff}])
    (is-tc-err (fn [a] (monitor-exit 1))
               [t/Any -> nil :filters {:then ff :else tt} :object {:id 0}]))
  (testing "def"
    (is-tc-e #(def a 1) [-> (t/Var (t/Val 1))])
    (testing ":dynamic metadata works"
      (is-tc-e (do (def ^:dynamic *blob* 1)
                   (tc-ignore
                     (assert (-> #'*blob*
                                 meta
                                 :dynamic))))))
    (testing "bad metadata throws static type error"
      (is-tc-err (def ^{:npe (inc nil)} a 1)))
    (is-tc-err #(def a 1) [-> (t/Var (t/Val 2))])
    (is-tc-e #(def a 1) 
             [-> (t/Var (t/Val 1)) :filters {:then tt :else ff}])
    (is-tc-err #(def a 1) 
             [-> (t/Var (t/Val 1)) :filters {:then ff :else tt}])
    (is-tc-err (fn [f] (def a 1))
               [t/Any -> (t/Var (t/Val 1)) :filters {:then tt :else ff}
                :object {:id 0}])))
