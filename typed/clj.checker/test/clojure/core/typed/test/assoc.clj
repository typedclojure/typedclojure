(ns clojure.core.typed.test.assoc
  (:require [typed.clj.checker.test-utils :refer :all]
            [clojure.test :refer :all]))

(deftest assoc-test
  (is-tc-e (assoc {:a 1} :b 2)
           '{:a Number :b Number})

  (is-tc-err #(let [a 1]
                (assoc a :b 2)))
  (is-tc-err
    (do (t/ann-record FooRec [a :- t/Num
                              b :- t/Symbol])
        (defrecord FooRec [a b])
        (assoc (->FooRec 1 'a) :a 'b)))

  (is-tc-e
    (do (t/ann-record FooRec [a :- t/Num
                              b :- t/Symbol])
        (defrecord FooRec [a b])
        (assoc (->FooRec 1 'a) :a 4)))

  ;intersections
  (is-tc-e (t/fn [m :- (t/I (t/HMap :mandatory {:foo t/Num})
                            (t/HMap :mandatory {:bar t/Num}))]
             :- '{:foo t/Num, :bar t/Num, :baz t/Num}
             (assoc m :baz 2)))
  (is-tc-e (fn :forall [K]
             [m :- '{:a t/Int}
              k :- K]
             :- (t/Assoc '{:a t/Int} K '2)
             (assoc m k 2)))
  (is-tc-err (fn :forall [[M :< (t/Option (t/Map t/Any t/Any))]
                          K]
               [m :- M
                k :- K]
               (assoc m k 1)
               nil))
  (is-tc-err (fn [x y]
               (assoc x y 1)
               nil)
             (t/All [x y] [x y :-> nil]))
  (is-tc-err (fn [& args]
               (apply assoc [] args)
               nil)
             (t/All [c :..] [c :.. c :-> nil]))
  (is-tc-err (fn :forall [M K]
               [m :- M
                k :- K]
               :- (t/Assoc M K '2)
               (assoc m k 2)))
  (is-tc-e (fn :forall [[K :< t/Int]]
             [v :- '[]
              k :- K]
             :- (t/Assoc '[] K '2)
             (assoc v k 2)))
  (is-tc-err (fn :forall [K] ;; missing bounds
               [v :- '[]
                k :- K]
               :- (t/Assoc '[] K '2)
               (assoc v k 2)))
  (is-tc-e (fn [k :- t/Int] :- (t/Assoc '[] t/Int '2)
             (assoc [] k 2)))
  (is-tc-e (fn [k :- t/Int] :- (t/Assoc '[] t/Int '2)
             (assoc [] k 2)))
  (is-tc-e (fn [m :- (t/Map t/Nothing t/Nothing)]
             :- (t/Map ':a '1)
             (assoc m :a 1)))
  (is-tc-err (fn [m :- (t/Map t/Nothing t/Nothing)]
               :- (t/Map ':a '2)
               (assoc m :a 1)))
  (is-tc-err (fn [m :- (t/Map t/Nothing t/Nothing)]
               :- (t/Map ':b '1)
               (assoc m :a 1)))
  )
