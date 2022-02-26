(ns clojure.core.typed.test.cljs-core
    (:require [cljs.core :as core]
              [cljs.core.typed :as t]
              [clojure.test :refer :all]
              [typed.cljs.checker.test-utils :refer [is-tc-e]]))

;;; defining tests this way should help estimate the coverage
;;; cljs.core vars not included in (keys @core-tests) are totally untested

(def core-tests (atom {}))

(defmacro add-test [f & pairs]
  (assert (even? (count pairs)))
  `(swap! core-tests conj 
      ['~(symbol "cljs.core" (name f)) '~pairs]))

;;tests for single parameter functions
(defmacro add-test1 [f & pairs]
  (assert (even? (count pairs)))
  `(swap! core-tests conj
      [(with-meta '~(symbol "cljs.core" (name f)) 
        {:single-arg true})
       '~pairs]))

(defmacro run-core-tests []
  (cons 'do
    (for [[f cases] @core-tests
          [args expected] (partition 2 cases)]
      (if (:single-arg (meta f))
          `(is-tc-e (~f ~args) ~expected)
          `(is-tc-e (~f ~@args) ~expected)))))


;;; core vars test

(add-test1 seq
    [1 2 3] (t/NonEmptySeqable t/JSnumber)
    []      (t/Option (t/NonEmptyASeq t/Any)))

(add-test1 first 
    [8]           t/JSnumber
    (seq [1 2 3]) t/JSnumber
    nil           nil)

(add-test1 rest
    [1 2 3]       (core/ASeq t/JSnumber)
    (seq [1 2 3]) (core/ASeq t/JSnumber)
    []            (core/ASeq t/Any))

(add-test1 last
    [1 2 3]       t/JSnumber
    (seq [1 2 3]) t/JSnumber
    []            (t/Option t/Any))

(add-test1 butlast
    [1 2 3] (core/ASeq t/JSnumber)
    (seq [1 2 3]) (core/ASeq t/JSnumber)
    []  (core/ASeq t/Any))

(add-test1 second
    [1 2 3] t/JSnumber
    []      nil
    nil     nil
    (seq [1 2 3]) (t/Option t/JSnumber))


(add-test1 clj->js
    {:a 1} t/Any)

(add-test1 nil?
    nil   t/JSboolean
    "nil" t/JSboolean)

(add-test1 ifn?
    (fn [x] x) t/JSboolean
    "foo"      t/JSboolean)

(add-test1 identity
    8       t/JSnumber
    "hello" t/JSString
    [0]     (t/Vec t/JSnumber))

(add-test take
    [2 [1 2 3 4]] (core/ASeq t/CLJSInteger))

(add-test drop
    [2 [1 2 3 4]] (core/ASeq t/CLJSInteger))

(add-test1 number?
    8     t/JSboolean
    [1 2] t/JSboolean)

(add-test1 string?
    "hello" t/JSboolean)

(add-test1 seq?
    (seq [1 2 3]) t/JSboolean
    [1 2 3]       t/JSboolean)

(add-test apply
    [+ [2 3]]               t/JSnumber
    [str ["hello" "world"]] t/JSString)

(add-test conj
    [["foo"] "bar"] (core/IVector t/JSString)
    [(seq [3 4 5]) 1 2] (core/ASeq t/JSnumber)
    [(seq [[1] [2 3]]) [8] [9]] (core/ASeq (t/Vec t/JSnumber))
    ;[{:foo 5} [:bar 8]] (t/Map core/Keyword t/JSnumber)
    ;[{:foo "bar"} {:baz 1 2 3}] (t/Map core/Keyword t/Any)
    )

(add-test get
    [#{1 2 3} 3] (t/Option t/JSnumber)
    [{:a true :b false} :c false] t/JSboolean)

(add-test assoc
    [["foo" "bar"] 2 "baz"] (t/Vec t/JSString)
    [{[2 3] "foo"} [4 5] "bar"] (t/Map (t/Vec t/JSnumber) t/JSString))

(add-test dissoc
    [{:foo 8 :bar 9} :foo] (t/Map core/Keyword t/JSnumber))

(add-test1 fn?
    (fn [x y] y)  t/JSboolean
    cljs.core/map t/JSboolean)

(add-test1 peek
    [{:foo "bar" :baz "zot"} {:foo "bar"}] (t/Map core/Keyword t/JSString))

(add-test1 pop
    [1 2 3] (t/Vec t/JSnumber))

(add-test disj
    [#{1 2 3 4} 3 4] (t/Set t/JSnumber))

(add-test1 empty?
    []  t/JSboolean
    #{} t/JSboolean
    [1] t/JSboolean
    {:a 1} t/JSboolean)

(add-test1 coll?
    {:a 1} t/JSboolean
    #{1 2} t/JSboolean
    [2 3]  t/JSboolean
    "foo"  t/JSboolean
    2      t/JSboolean)

(add-test1 map?
    {:a 1} t/JSboolean
    {}     t/JSboolean
    [0 1]  t/JSboolean)

(add-test1 vector?
    []     t/JSboolean
    [1]    t/JSboolean
    {:a 1} t/JSboolean
    "foo"  t/JSboolean)

(add-test1 false?
    []    t/JSboolean
    false t/JSboolean
    true  t/JSboolean
    1     t/JSboolean)

(add-test1 true?
    []    t/JSboolean
    false t/JSboolean
    true  t/JSboolean
    1     t/JSboolean)

(add-test1 seq?
    []           t/JSboolean
    #{2 3}       t/JSboolean
    (seq [1 2])  t/JSboolean
    (seq #{2 3}) t/JSboolean)

(add-test1 boolean
    []     t/JSboolean
    "true" t/JSboolean
    :false t/JSboolean)

(add-test1 integer?
    2   t/JSboolean
    1.3 t/JSboolean
    "8" t/JSboolean)

(add-test contains?
    [[1 2 3] 2]    t/JSboolean
    [#{1 2} 8]     t/JSboolean
    [(seq []) "a"] t/JSboolean)

(add-test find
    [{:a 1 :b 2} :b] (t/Option (t/HVec [Keyword t/JSnumber]))
    [[:a :b] 0]      (t/Option (t/HVec [t/CLJSInteger Keyword])))

(add-test distinct?
    [:a :b]       t/JSboolean
    [[1] [1] [1]] t/JSboolean
    [:a "foo" []] t/JSboolean)

(add-test compare
    [[1 2] [2 3]] t/JSnumber
    ["foo" "bar"] t/JSnumber
    [2 1]         t/JSnumber)

(add-test sort
    [[2 4 2 1 5 3]] (t/Option (core/ASeq t/CLJSInteger))
    [[:a :c :b]]    (t/Option (core/ASeq Keyword))
    [(t/fn [x :- t/CLJSInteger y :- t/CLJSInteger] :- t/CLJSInteger (- x y))
     [6 1 7 3 2]]   (t/Option (core/ASeq t/CLJSInteger)))

(add-test1 shuffle
    [1 2 3 4] (t/Vec t/CLJSInteger)
    #{4 8 2}  (t/Vec t/CLJSInteger))

;FIXME reenable after porting to tools.analyzer.js. Some issue with hygienic renaming
#_(add-test reduce
    [(t/fn [x :- t/CLJSInteger y :- t/JSString] :- t/CLJSInteger 0)
     ["foo" "bar" "baz"]] t/CLJSInteger
    [(t/fn [x :- t/CLJSInteger y :- t/CLJSInteger] :- (core/Reduced t/CLJSInteger) (reduced 0))
     #{8 36 2}] t/CLJSInteger
    [(t/fn [x :- t/CLJSInteger y :- t/CLJSInteger] :- (core/Reduced t/CLJSInteger) (reduced 0))
     #{8 36 2} 0] t/CLJSInteger)

(add-test reduce-kv
    [(t/fn [a :- t/CLJSInteger k :- Keyword v :- t/CLJSInteger] :- t/CLJSInteger (+ a v))
     0 {:a 1 :b 2 :c 3}] t/CLJSInteger)

(add-test <
    [1 5]     t/JSboolean
    [1.8 0.8] t/JSboolean
    [1/2 3/2] t/JSboolean)

(add-test1 int
    1      t/CLJSInteger)

(add-test mod
    [8 2] t/AnyInteger
    [8.1 2.1] t/JSnumber)

(add-test rand
    []  t/JSnumber
    [8] t/JSnumber)

(add-test1 rand-int
    1    t/CLJSInteger
    1000 t/CLJSInteger)

(add-test nthnext
    [nil 8] nil
    [#{2 3 1} 1] (t/Option (t/NonEmptyASeq t/CLJSInteger))
    [[1 2 3]  9] (t/Option (t/NonEmptyASeq t/CLJSInteger)))

(comment add-test1 rseq
    [1 2 3] (t/Option (t/NonEmptyASeq t/CLJSInteger)))

(add-test1 reverse
    [1 2 3] (core/ASeq t/CLJSInteger)
    {:a 1 :b 2} (core/ASeq t/Any)
    #{1 2 3}  (core/ASeq t/CLJSInteger))

(comment add-test list
    [1 2 3] (t/PersistentList t/CLJSInteger)
    [:a 2]  (t/PersistentList t/Any))

(add-test cons
    [1 [2 3]]   (core/ASeq t/CLJSInteger)
    [[1] [[2]]] (core/ASeq (t/Vec t/CLJSInteger)))







;;TODO uncomment
;(deftest cljs-core-fns-refined-test
;    (run-core-tests))
