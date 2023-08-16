(ns clojure.core.typed.test.symbolic-closures
  (:require 
    [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err]]
    [typed.cljc.checker.type-rep :as r]
    [clojure.test :refer :all]))

(deftest symbolic-closure-test
  ;; thunks always checked
  (is-tc-err #(identity))
  (is-tc-err (t/fn [] (identity)))
  (is-tc-err (fn [] (identity)))
  (is-tc-err (clojure.core.typed/fn [] (identity)))
  (is-tc-err (typed.clojure/fn [] (identity)))
  (is-tc-err (core/fn [] (identity)))
  (is-tc-err (fn* [] (identity)))
  (is-tc-err (let [f #(identity)]))
  ;; type error can hide if bad (non-thunk) fn never called
  (is-tc-e (let [f #(identity % %)]))
  (is-tc-e (let [f #(do %)]
             (f 1))
           t/Int)
  (is-tc-e (let [f (fn* [x] x)]
             (f 1))
           t/Int)
  (is-tc-e (let [f (t/fn [x] x)]
             (f 1))
           t/Int)
  (is-tc-e (let [f (core/fn [x] x)]
             (f 1)))
  (is-tc-e (let [f (t/fn [x] x)]
             (f 1)))
  #_ ;;FIXME
  (is-tc-e (let [f (t/fn [x] x)]
             (f 1))
           t/Int)
  (is-tc-err (let [f (fn* [x] x)]
               (f 1))
             t/Bool)
  (is-tc-err (let [f (t/fn [x] x)]
               (f 1))
             t/Bool)
  (is-tc-err (let [f (t/fn [x] x)]
               (f 1))
             t/Bool)
  (is-tc-e (let [f (t/ann-form
                     #(% 1)
                     [[t/Int :-> t/Bool] :-> t/Bool])]
             (f #(boolean (inc %))))
           t/Bool)
  (is-tc-err (let [f (t/ann-form
                       #(% 1)
                       [[t/Int :-> t/Bool] :-> t/Bool])]
               (f #(boolean (inc %))))
             t/Int)
  (is-tc-e (let [comp (fn* [f g] (fn* [x] (f (g x))))
                 f (fn* [x] x)
                 g (fn* [y] y)]
             ((comp f g) 1))
           t/Int)
  (is-tc-err (let [comp (fn* [f g] (fn* [x] (f (g x))))
                   f (fn* [x] x)
                   g (fn* [y] y)]
               ((comp f g) 1))
             t/Bool))

(deftest poly-infer
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
               (def app #(%1 %2))
               (app #(inc %) 1))
           t/Int)
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
               (def app #(%1 %2))
               (let [res (app #(do %) 1)]
                 (t/ann-form res (t/Val 1))
                 nil)))
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (let [res (app #(do %) 1)]
                   (t/ann-form res (t/Val 2))
                   nil)))
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
               (def app #(%1 %2))
               (app #(do (t/ann-form % (t/Val 1)))
                    1)))
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (app #(do (t/ann-form % (t/Val 2)))
                      1)))
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (app #(inc %1 %2) 1)))
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (app #(inc %1 %2) 1))
             t/Int)
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (app #(inc %) 1))
             t/Bool)
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (curry #(+ %1 %2) 1)))
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (curry #(+ (t/ann-form %1 t/Nothing) %2) 1)))
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (curry #(+ %1 (t/ann-form %2 t/Nothing)) 1)))
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (curry #(+ %1 %2) 1))
           [t/Int :-> t/Int])
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (curry #(+ (t/ann-form %1 t/Nothing) %2) 1))
             [t/Int :-> t/Int])
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (curry #(+ %1 (t/ann-form %2 t/Nothing)) 1))
             [t/Int :-> t/Int])
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (let [f (curry #(+ %1 %2) 1)]
                 (t/ann-form f [t/Int :-> t/Int])
                 nil)))
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (let [f (curry #(+ %1 %2) 1)]
                   (t/ann-form f [t/Num :-> t/Int])
                   nil)))
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (let [f (curry #(+ %1 %2) 1)]
                 (t/ann-form f [t/Int :-> t/Num])
                 nil)))
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (curry #(+ %1 %2) 1))
             [t/Bool :-> t/Int])
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (curry #(+ %1 %2) 1))
             [t/Int :-> t/Bool])
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 (curry #(+ %1 %2) 1))
             [t/Int t/Int :-> t/Int])
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               ((curry #(+ %1 %2) 1) 2))
           t/Int)
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 ((curry #(+ %1 %2) 1) true))
             t/Int)
  (is-tc-err (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
                 (def curry cc/partial)
                 ((curry #(+ %1 %2) 1) 2))
             t/Bool)
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (let [f (curry #(+ %1 %2) 1)]
                 (f 2)))
           t/Int))

;; when symbolic closure can be called iteratively based on its type,
;; type variables should be fixed early to avoid more complicated logic
;; for now (e.g., rechecking the symbolic closure or other arguments).
(deftest poly-iter-infer
  (is-tc-e (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
               (def app #(%1 %2))
               (let [res (app #(do %) 1)]
                 (t/ann-form res (t/Val 1))
                 nil)))
  (is-tc-err (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
                 (def app #(%1 %2))
                 (app #(do % true) 1)))
  (is-tc-err (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
                 (def app #(%1 %2))
                 (app #(inc %) 1)))
  )

;; expected return type of symbolic closure can be partially erased
(deftest poly-structured-return
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> (t/Vec y)] x :-> (t/Vec y)]))
                 (def app #(%1 %2))
                 (app #(do %) 1)))
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> (t/Vec y)] x :-> (t/Vec y)]))
               (def app #(%1 %2))
               (app #(do [%]) 1)))
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
               (def app #((%1 %2) %2))
               (app #(do % (fn [x] true)) 1)))
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
               (def app #((%1 %2) %2))
               (app #(do % (fn [x] (t/ann-form x (t/Val 1)) true)) 1)))
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
               (def app #((%1 %2) %2))
               (app #(do % (fn [x] (t/ann-form x (t/Val 1)) true)) 1))
           true)
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
                 (def app #((%1 %2) %2))
                 (app #(do % (fn [x] (t/ann-form x (t/Val 1)) true)) 1))
             (t/Val 1)))

(deftest poly-rest-infer
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> y] x nil :* :-> y]))
               (def app #(%1 (do %& %2)))
               (app #(inc %) 1 nil nil))
           t/Int)
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x nil :* :-> y]))
                 (def app #(%1 (do %& %2)))
                 (app #(inc %) 1 1 nil))
             t/Int)
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> y] x nil :* :-> y]))
                 (def app #(%1 (do %& %2)))
                 (app #(inc %) 1 nil nil))
             t/Bool))

(deftest poly-dots-infer
  (is-tc-e (do (t/ann ^:no-check app (t/All [x z y :..] [[x y :.. y :-> z] x y :.. y :-> z]))
               (def app #(apply %1 %2 %&))
               (app #(inc %) 1))
           t/Int)
  (is-tc-e (do (t/ann ^:no-check app (t/All [x z y :..] [[x y :.. y :-> z] x y :.. y :-> z]))
               (def app #(apply %1 %2 %&))
               (let [res (app #(do %) 1)]
                 (t/ann-form res (t/Val 1))
                 nil)))
  (is-tc-e (do (t/ann ^:no-check app (t/All [x z y :..] [[x y :.. y :-> z] x y :.. y :-> z]))
               (def app #(apply %1 %2 %&))
               (let [res (app #(do [% %2]) 1 2)]
                 ;(t/ann-form res '[(t/Val 1) (t/Val 2)])
                 nil)))
  (is-tc-err (do (t/ann ^:no-check app (t/All [x z y :..] [[x y :.. y :-> z] x y :.. y :-> z]))
                 (def app #(apply %1 %2 %&))
                 (app #(inc %) 1))
             t/Bool)
  (is-tc-e (let [f (fn* [x] x)]
             (map f [1]))
           (t/Seqable t/Int))
  (is-tc-e (map #(inc %) [1 2]))
  (is-tc-e (map #(inc (t/ann-form % t/Int)) [1 2]))
  (is-tc-err (map #(inc (t/ann-form % t/Bool)) [1 2]))
  (is-tc-e (map #(+ % %2) [1 2] [3 4]))
  (is-tc-e (map #(do (t/ann-form % t/Int) (t/ann-form %2 t/Bool)) [1 2] [true false])
           (t/Seq t/Bool))
  (is-tc-err (map #(do (t/ann-form % t/Int) (t/ann-form %2 t/Bool)) [1 2] [true false])
             (t/Seq t/Int))
  (is-tc-err (map #(do (t/ann-form % t/Bool) (t/ann-form %2 t/Bool)) [1 2] [true false])
             (t/Seq t/Bool))
  (is-tc-err (map #(do (t/ann-form % t/Int) (t/ann-form %2 t/Int)) [1 2] [true false])
             (t/Seq t/Bool))
  (is-tc-err #(map (fn [_]) [1 2] [true false]))
  (is-tc-err #(map (fn [_ _ _]) [1 2] [true false]))
  (is-tc-e (map (fn [_ _ _]) [1 2] [true false] [:a :b])))

(deftest transducer-symb
  (is-tc-e (map #(do %)))
  #_;;TODO
  (is-tc-e (map identity))
  #_;;TODO
  (is-tc-e (map identity)
           (t/Transducer t/Int t/Int))
  (is-tc-e (map #(t/ann-form % t/Int)))
  (is-tc-e (map #(t/ann-form % t/Nothing)))
  (is-tc-e (map #(inc %)))
  (is-tc-e (map #(inc %)) (t/Transducer t/Int t/Int))
  (is-tc-e (map #(inc (t/ann-form % t/Int))) (t/Transducer t/Int t/Int))
  (is-tc-err (map #(inc (t/ann-form % t/Bool))) (t/Transducer t/Int t/Int))
  (is-tc-err (map #(inc %)) (t/Transducer t/Bool t/Int))
  (is-tc-err (map #(inc %)) (t/Transducer t/Int t/Bool))
  (is-tc-err (map (t/fn [% :- t/Int] %)) (t/Transducer t/Int t/Bool))
  (is-tc-err (map #(t/ann-form % t/Nothing)) (t/Transducer t/Int t/Bool))
  (is-tc-e (let [res (map #(do %))]
             (t/ann-form res (t/Transducer t/Nothing t/Any))))
  (is-tc-e (let [res (map (fn [% :- t/Any] %))]
             (t/ann-form res (t/Transducer t/Nothing t/Any))))
  (is-tc-e (let [res (map (fn [% :- t/Any] %))]
             (t/ann-form res (t/Transducer t/Any t/Any))))
  (is-tc-e (let [res (map #(do %))]
             (t/ann-form res (t/Transducer t/Any t/Any))))
  (is-tc-e (map #(do %)) (t/Transducer t/Int t/Int))
  (is-tc-e (let [map (t/ann-form map (t/All [c a b :..] [[a :-> c] :-> (t/Transducer a c)]))
                 res (map #(do %))]
             (t/ann-form res (t/Transducer t/Int t/Int))))
  (is-tc-e (let [res (map #(do %))]
             (t/ann-form res (t/Transducer t/Int t/Int))))
  (is-tc-err (let [res (map #(do %))]
               (t/ann-form res [t/Any :-> t/Any])))
  (is-tc-e (let [res (map #(do %))]
             (t/ann-form res [t/Nothing :-> t/Any])))
  (is-tc-err (let [res (map (fn [% :- t/Int] (do %)))]
               (t/ann-form res [t/Any :-> t/Any])))
  (is-tc-e (let [res (map (fn [% :- t/Int] (do %)))]
             (t/ann-form res [t/Nothing :-> t/Any])))
  (is-tc-e (into [] (map #(do %)) [1]))
  #_ ;;NYI
  (is-tc-e (into [] (map identity) [1]))
  (is-tc-e (into [] (map #(t/ann-form % t/Int)) [1]))
  (is-tc-err (into [] (map #(t/ann-form % t/Int)) [true]))
  (is-tc-err (into [] (map #(t/ann-form % t/Bool)) [1]))
  (is-tc-e (into [] (map (fn [% :- (t/Val 1)] %)) [1])
           (t/Vec (t/Val 1)))
  (is-tc-err (into [] (map (fn [% :- (t/Val 1)] %)) [1])
             (t/Vec (t/Val 2)))
  (is-tc-err (into [] (map (fn [% :- (t/Val 1)] %)) [2])
             (t/Vec (t/Val 1)))
  (is-tc-err (into [] (map (fn [% :- (t/Val 1)] %)) [1])
             (t/Vec t/Nothing))
  (is-tc-e (into (t/ann-form [] (t/Vec (t/Val 1)))
                 (map #(do %)) [1]))
  (is-tc-e (let [map (t/ann-form map (t/All [c a b :..] [[a :-> c] :-> (t/Transducer a c)]))
                 into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))
                 res (into [1] (map #(do %)) [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-e (let [res (into (t/ann-form [] (t/Vec (t/Val 1)))
                           (map #(do %)) [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-e (let [res (into [] (t/ann-form (map #(do %)) (t/Transducer (t/Val 1) (t/Val 1))) [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-err (let [res (into [] (t/ann-form (map #(do %)) (t/Transducer (t/Val 2) (t/Val 2))) [1])]
               (t/ann-form res (t/Vec (t/Val 1)))
               nil))
  (is-tc-e (into (t/ann-form [] (t/Vec (t/Val 1)))
                 (map #(do %)) [1])
           (t/Vec (t/Val 1)))
  (is-tc-e (into (t/ann-form [] (t/Vec (t/Val 1)))
                 (map #(do %)) []))
  (is-tc-e (into (t/ann-form [] (t/Vec (t/Val 1)))
                 (map #(do %)) [])
           (t/Vec (t/Val 1)))
  (is-tc-err (into (t/ann-form [] (t/Vec (t/Val 2)))
                   (map #(do %)) [1])
             (t/Vec (t/Val 1)))
  (is-tc-err (into (t/ann-form [] (t/Vec (t/Val 2)))
                   (map #(do %)) [2])
             (t/Vec (t/Val 1)))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [:-> c]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [:-> ^:clojure.core.typed/infer t/Any])]
               (t/ann-form res [:-> true]))))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> c]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [true :-> ^:clojure.core.typed/infer t/Any])]
               (t/ann-form res [true :-> true]))))
  #_;;TODO perhaps if any infer-any's exist in return type, we should return a symbolic closure?
  ;; (and remember the return type in the symbolic closure)
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> c]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [^:clojure.core.typed/infer t/Any :-> true])]
               (t/ann-form res [true :-> true]))))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[c :-> c] :-> [c :-> c]])]
             (t/ann-form (map (fn* [x] x))
                         [true :-> true])))
  (is-tc-err (fn [map :- (t/All [c a b :..] [[c :-> c] :-> [c :-> c]])]
               (t/ann-form (map (fn* [x] (t/ann-form x false)))
                           [true :-> true])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[c :-> c] :-> [c :-> c]])]
             (let [res (t/ann-form (map (fn* [x] x))
                                   [true :-> ^:clojure.core.typed/infer t/Any])]
               (t/ann-form res [true :-> true]))))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> t/Any]])]
             (t/ann-form (map (fn* [_] true)) [true :-> t/Any])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> t/Any]])]
             (t/ann-form (map (fn* [_] true)) [true :-> t/Any])))
  #_;;FIXME ????
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> t/Any]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [^:clojure.core.typed/infer t/Any :-> t/Any])]
               (t/ann-form res [true :-> t/Any]))))
  (is-tc-e (fn [map :- (t/All [c] [[t/Any :-> c] :-> [[c :-> t/Any] :-> t/Any]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [[^:clojure.core.typed/infer t/Any :-> t/Any] :-> t/Any])]
               (t/ann-form res [[true :-> t/Any] :-> t/Any]))))
  (is-tc-e (fn [map :- (t/All [c] [[:-> c] :-> [[c :-> t/Any] :-> t/Any]])]
             (t/ann-form (map (fn* [] true))
                         [[true :-> t/Any] :-> t/Any])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[a :-> c] :-> [[c :-> t/Any] :-> [a :-> t/Any]]])]
             (let [res (t/ann-form (map #(do %))
                                   [[^:clojure.core.typed/infer t/Any :-> t/Any] :-> [(t/Val 1) :-> t/Any]])]
               (t/ann-form res [[(t/Val 1) :-> t/Any] :-> [(t/Val 1) :-> t/Any]]))))
  (is-tc-e (let [res (t/ann-form (map #(do %))
                                 (t/Transducer (t/Val 1) ^:clojure.core.typed/infer t/Any))]
             (t/ann-form res (t/Transducer (t/Val 1) (t/Val 1)))))
  (is-tc-err (into [] (map #(do %)) [2])
             (t/Vec (t/Val 1)))
  (is-tc-err (into [] (map #(do %)) [1])
             (t/Vec t/Nothing))
  (is-tc-err (fn [into :- (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)])
                  map :- (t/All [c a b :..] [[a :-> c] :-> (t/Transducer a c)])]
               (let [res (into [] (map #(do %)) [2])]
                 (t/ann-form res (t/Vec (t/Val 1))))))
  (is-tc-err (let [res (into [] (map #(do %)) [2])]
               (t/ann-form res (t/Vec t/Nothing))))
  (is-tc-err (into (t/ann-form [] (t/Vec t/Nothing))
                   (t/ann-form (map #(do %)) (t/Transducer (t/Val 2) (t/Val 2)))
                   (t/ann-form [2] (t/Vec (t/Val 2))))
             (t/Vec (t/Val 1)))
  (is-tc-err (into (t/ann-form [] (t/Vec (t/Val 1)))
                   (t/ann-form (map #(do %)) (t/Transducer (t/Val 2) (t/Val 2)))
                   (t/ann-form [2] (t/Vec (t/Val 2))))
             (t/Vec (t/Val 1)))
  (is-tc-err (fn [x :- (t/Vec (t/Val 1))
                  xf :- (t/Transducer (t/Val 2) (t/Val 2))
                  y :- (t/Vec (t/Val 2))]
               :- (t/Vec (t/Val 1))
               (into x xf y)))
  (is-tc-e (fn [x :- (t/Vec (t/Val 1))
                xf :- (t/Transducer (t/Val 2) (t/Val 1))
                y :- (t/Vec (t/Val 2))]
             :- (t/Vec (t/Val 1))
             (into x xf y)))
  (is-tc-e (do (t/ann pour (t/All [x y] [(t/Vec x) [y :-> x] (t/Seqable y) :-> (t/Vec x)]))
               (def pour #(into %1 (map %2) %3))
               (pour [] #(do %) [1])))
  (is-tc-e (do (t/ann pour (t/All [x y] [(t/Vec x) [y :-> x] (t/Seqable y) :-> (t/Vec x)]))
               (def pour #(into %1 (map %2) %3))
               (let [res (pour [] #(do %) [1])]
                 (t/ann-form res (t/Vec (t/Val 1)))
                 nil)))
  (is-tc-e (do (t/ann pour (t/All [x y] [(t/Vec x) [y :-> x] (t/Seqable y) :-> (t/Vec x)]))
               (def pour #(into %1 (map %2) %3))
               (pour [] #(do %) [1]))
           (t/Vec (t/Val 1)))
  (is-tc-err (do (t/ann pour (t/All [x y] [(t/Vec x) [y :-> x] (t/Seqable y) :-> (t/Vec x)]))
                 (def pour #(into %1 (map %2) %3))
                 (pour [] #(do %) [1]))
             (t/Vec (t/Val 2)))
  (is-tc-e (into [] (map #(do %)) [1])
           (t/Vec (t/Val 1)))
  (is-tc-e (let [map (t/ann-form map (t/All [c a b :..] [[a :-> c] :-> (t/Transducer a c)]))
                 into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))]
             (into [] (map #(do %)) [1]))
           (t/Vec (t/Val 1)))
  (is-tc-e (do (t/ann map* (t/All [c a b :..] [[a :-> c] :-> [a :-> c]]))
               (def map* #(do %))
               (t/ann pour (t/All [x y] [(t/Vec x) [y :-> x] (t/Seqable y) :-> (t/Vec x)]))
               (def pour #(into %1 (map %2) %3))
               (pour [] (map* #(do %)) [1]))
           (t/Vec (t/Val 1)))
  (is-tc-e (into [] (map #(do %)) [1])
           (t/Vec t/Any))
  (is-tc-e (let [res (into [] (map (fn [% :- (t/Val 1)] %)) [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-e (let [into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))
                 xf (t/ann-form (map (fn [% :- (t/Val 1)] %)) (t/Transducer (t/Val 1) (t/Val 1)))
                 res (into [] xf [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-err (let [f (t/ann-form #(do %) [(t/Val 1) :-> (t/Val 1)])]
               (clojure.core.typed/print-env "f should not be SymbolicClosure!")
               (t/ann-form f [(t/Val 2) :-> (t/Val 2)])
               nil))
  (is-tc-e (let [f #(do %)]
             (t/ann-form f [(t/Val 1) :-> (t/Val 1)])
             (t/ann-form f [(t/Val 2) :-> (t/Val 2)])
             nil))
  (is-tc-err (let [f #(do %)]
               (t/ann-form f [(t/Val 1) :-> (t/Val 2)])
               (t/ann-form f [(t/Val 2) :-> (t/Val 2)])
               nil))
  (is-tc-e (let [xf (map #(do %))]
             (t/ann-form xf (t/Transducer (t/Val 1) (t/Val 1)))
             (t/ann-form xf (t/Transducer (t/Val 2) (t/Val 2)))
             nil))
  (is-tc-err (let [xf (t/ann-form (map #(do %)) (t/Transducer (t/Val 1) (t/Val 1)))]
               (clojure.core.typed/print-env "xf should not be SymbolicClosure!")
               (t/ann-form xf (t/Transducer (t/Val 2) (t/Val 2)))
               nil))
  (is-tc-e (let [into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))
                 xf (t/ann-form (map #(do %)) (t/Transducer (t/Val 1) (t/Val 1)))
                 _ (t/ann-form xf (t/Transducer (t/Val 1) (t/Val 1)))
                 res (into [] xf [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-e (let [into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))
                 xf (t/ann-form (map #(do %)) (t/Transducer (t/Val 1) (t/Val 1)))
                 res (into [] xf [1])]
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  (is-tc-e (let [into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))
                 res (into [] (map #(do %)) [1])]
             (clojure.core.typed/print-env "foo")
             (t/ann-form res (t/Vec (t/Val 1)))
             nil))
  )
