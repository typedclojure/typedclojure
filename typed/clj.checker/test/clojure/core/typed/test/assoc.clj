(ns clojure.core.typed.test.assoc
  (:require [clojure.core.typed :refer [check-ns]]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.type-rep :refer :all :as r]
            [typed.cljc.checker.filter-ops :refer :all]
            [typed.cljc.checker.object-rep :refer :all]
            [typed.cljc.checker.type-ctors :refer :all]
            [clojure.test :refer :all]))

(deftest invoke-assoc-test
  
  ; HMaps
  (equal-types (assoc {} :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc nil :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil (t/U nil (HMap))) :a 5)
               (HMap :mandatory {:a '5}))
  
  (equal-types (assoc (clojure.core.typed/ann-form nil (t/U nil (t/HMap :complete? true))) :a 5)
               (HMap :mandatory {:a '5} :complete? true))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (t/HMap)) :a 5)
               (HMap :mandatory {:a '5}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (t/HMap :optional {:a clojure.core.typed/Any})) :b "v")
               (HMap :mandatory {:b (Value "v")} :optional {:a Any}))
  
  (equal-types (assoc (clojure.core.typed/ann-form {} (t/HMap :optional {:a clojure.core.typed/Any})) :a "v")
               (HMap :mandatory {:a (Value "v")}))

  ;CTYP-79 resolve types properly in assoc
  (is (check-ns 'clojure.core.typed.test.hmap-resolve-assoc))
  
  ; HVecs
  (equal-types-noparse (assoc [] 0 1)
                       (-hvec [(-val 1)]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (assoc [3] 1 2)
                       (-hvec [(-val 3) (-val 2)]
                              :filters [(-true-filter)
                                        (-true-filter)]
                              :objects [-empty -empty]))
  
  (equal-types-noparse (assoc [0] 0 1)
                       (-hvec [(-val 1)]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  (equal-types-noparse (assoc [0] 0 (if (clojure.core.typed/ann-form 1 clojure.core.typed/Any) 1 2))
                       (-hvec [(Un (-val 1) (-val 2))]
                              :filters [(-true-filter)]
                              :objects [-empty]))
  
  ; Basic types
  (is-tc-e (assoc {} 'a 5)
           (t/Map 'a '5))
  
  (is-tc-e (assoc {:b 6} 'a 5)
           (t/Map (t/U 'a ':b) (t/U '5 '6)))
  (is-tc-err (assoc {:b 6} 'a 5)
             (t/Map ':b (t/U '5 '6)))
  (is-tc-err (assoc {:b 6} 'a 5)
             (t/Map (t/U 'a ':b) '5))
  
  (is-tc-e (fn [m :- (t/U nil (t/Map t/Any t/Any))]
             :- (t/Map t/Any t/Any)
             (assoc m :a 5)))
  
  (is-tc-e (fn [m :- (t/Map t/Any t/Any)]
             :- (t/Map t/Any t/Any)
             (assoc m :a 5)))
  
  (is-tc-e (fn [m :- (t/Vec t/Any)] 
             :- (t/Vec t/Any)
             (assoc m 0 2)))
  
  ;; TODO: assocs on records
  
  )

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
  (is-tc-e (fn [k :- t/Int] :- (t/I (t/CountRange 0)
                                    (t/Map t/Int '2))
             (assoc {} k 2)))
  (is-tc-e (fn [k :- t/Int] :- (t/CountRange 0)
             (assoc {} k 2)))
  (is-tc-err (fn [k :- t/Int] :- (t/CountRange 0 1)
               (assoc {} k 2)))
  (is-tc-err (fn [k :- t/Int] :- (t/CountRange 0 2)
               (assoc {} k 2)))
  (is-tc-e (fn [m :- (t/Map t/Nothing t/Nothing)]
             :- (t/Map ':a '1)
             (assoc m :a 1)))
  (is-tc-err (fn [m :- (t/Map t/Nothing t/Nothing)]
               :- (t/Map ':a '2)
               (assoc m :a 1)))
  (is-tc-err (fn [m :- (t/Map t/Nothing t/Nothing)]
               :- (t/Map ':b '1)
               (assoc m :a 1))))

(deftest dissoc-test
  (is-tc-e (dissoc {:a 1} :a) (t/HMap :complete? true))
  (is-tc-e (dissoc {:a 1} :b) (t/HMap :mandatory {:a '1} :complete? true))
  (is-tc-e (fn [k :- t/Kw] :- (t/HMap :optional {:a '1} :complete? true) (dissoc {:a 1} k)))
  (is-tc-err (fn [k :- t/Kw] :- (t/HMap :mandatory {:a '1} :complete? true) (dissoc {:a 1} k)))
  (is-tc-err (fn [k :- t/Any] :- (t/CountRange 1)
               (dissoc {:a 1} k)))
  (is-tc-e (fn [k :- t/Any] :- (t/CountRange 1)
             (dissoc {:a 1} k)))
  (is-tc-e (fn [m :- (t/Nilable '{:a '1})] :- (t/Nilable (t/HMap :absent-keys #{:a}))
             (dissoc m :a))))
