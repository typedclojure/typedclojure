(ns clojure.core.typed.test.symbolic-closures
  (:require
    [typed.clojure :as t]
    [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err tc-e]]
    [typed.clj.checker.parse-unparse :refer [parse-clj]]
    [clojure.core.typed.util-vars :as vs]
    [typed.cljc.checker.type-rep :as r]
    [clojure.test :refer [deftest is]]))

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
  (is-tc-err (let [f #(identity % %)]))
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
  (is (thrown? clojure.lang.ArityException
               (tc-e (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                         (def app #(%1 %2))
                         (app #(inc %1 %2) 1)))))
  (is (thrown? clojure.lang.ArityException
               (tc-e (do (t/ann app (t/All [x y] [[x :-> y] x :-> y]))
                         (def app #(%1 %2))
                         (app #(inc %1 %2) 1))
                     t/Int)))
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
  (is-tc-e (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
               (def app #(%1 %2))
               (app #(do % true) 1)))
  (is-tc-e (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
               (def app #(%1 %2))
               (app #(do % true) 1))
           (t/U true '1))
  (is-tc-err (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
                 (def app #(%1 %2))
                 (app #(do % true) 1))
             true)
  (is-tc-err (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
                 (def app #(%1 %2))
                 (app #(do % true) 1))
             '1)
  (is-tc-e (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
               (def app #(%1 %2))
               (app #(inc %) 1)))
  (is-tc-e (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
               (def app #(%1 %2))
               (let [res (app #(inc %) 1)]
                 (t/ann-form res Long)
                 nil)))
  (is-tc-err (do (t/ann app (t/All [x] [[x :-> x] x :-> x]))
                 (def app #(%1 %2))
                 (let [res (app #(inc %) 1)]
                   (t/ann-form res (t/Val 2))
                   nil))))

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
               (app #(do % (fn* [x] (t/ann-form x (t/Val 1)) true))
                    1)))
  (is-tc-e (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
               (def app #((%1 %2) %2))
               (app #(do % (cc/fn [x] (t/ann-form x (t/Val 1)) true))
                    1)))
  (binding [vs/*verbose-types* false #_true]
    (is-tc-e (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
                 (def app #((%1 %2) %2))
                 (app #(do % (fn [x] (t/ann-form x (t/Val 1)) true))
                      1))))
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
  (is-tc-e (map identity))
  (is-tc-e (map identity)
           (t/Transducer t/Int t/Int))
  (is-tc-err (map identity)
             (t/Transducer t/Int t/Bool))
  (is-tc-e (map identity)
           (t/Transducer t/Int (t/U t/Bool t/Int)))
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
  ;;TODO assert this returns a good error message:
  ;; (t/Transducer t/Nothing t/Nothing) <!: (t/Transducer t/Any t/Nothing)
  (is-tc-err (let [res (map #(do %))]
               (t/ann-form res (t/Transducer t/Any t/Nothing))))
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
  ;;FIXME better error message, should use Transducer type, not its expansion
  (is-tc-err (let [res (map (fn [% :- t/Int] (do %)))]
               (t/ann-form res [t/Any :-> t/Any])))
  (is-tc-e (let [res (map (fn [% :- t/Int] (do %)))]
             (t/ann-form res [t/Nothing :-> t/Any])))
  (is-tc-e (into [] (map #(do %)) [1]))
  (is-tc-e (into [] (map identity) [1]))
  (is-tc-e (into [] (map identity) [1]) (t/Vec t/Int))
  (is-tc-err (into [] (map identity) [1]) (t/Vec t/Bool))
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
  (is-tc-e (into (t/ann-form [] (t/Vec (t/Val 1)))
                 (map identity) [1]))
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
                                   [:-> t/Infer])]
               (t/ann-form res [:-> true]))))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> c]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [true :-> t/Infer])]
               (t/ann-form res [true :-> true]))))
  #_;;TODO perhaps if any wild's exist in return type, we should return a symbolic closure?
  ;; (and remember the return type in the symbolic closure)
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> c]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [t/Infer :-> true])]
               (t/ann-form res [true :-> true]))))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[c :-> c] :-> [c :-> c]])]
             (t/ann-form (map (fn* [x] x))
                         [true :-> true])))
  (is-tc-err (fn [map :- (t/All [c a b :..] [[c :-> c] :-> [c :-> c]])]
               (t/ann-form (map (fn* [x] (t/ann-form x false)))
                           [true :-> true])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[c :-> c] :-> [c :-> c]])]
             (let [res (t/ann-form (map (fn* [x] x))
                                   [true :-> t/Infer])]
               (t/ann-form res [true :-> true]))))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> t/Any]])]
             (t/ann-form (map (fn* [_] true)) [true :-> t/Any])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> t/Any]])]
             (t/ann-form (map (fn* [_] true)) [true :-> t/Any])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[t/Any :-> c] :-> [c :-> t/Any]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [t/Infer :-> t/Any])]
               (t/ann-form res [true :-> t/Any])
               (t/ann-form res [false :-> t/Any])
               (t/ann-form res [t/Any :-> t/Any]))))
  (is-tc-e (fn [map :- (t/All [c] [[t/Any :-> c] :-> [[c :-> t/Any] :-> t/Any]])]
             (let [res (t/ann-form (map (fn* [_] true))
                                   [[t/Infer :-> t/Any] :-> t/Any])]
               (t/ann-form res [[true :-> t/Any] :-> t/Any]))))
  (is-tc-e (fn [map :- (t/All [c] [[:-> c] :-> [[c :-> t/Any] :-> t/Any]])]
             (t/ann-form (map (fn* [] true))
                         [[true :-> t/Any] :-> t/Any])))
  (is-tc-e (fn [map :- (t/All [c a b :..] [[a :-> c] :-> [[c :-> t/Any] :-> [a :-> t/Any]]])]
             (let [res (t/ann-form (map #(do %))
                                   [[t/Infer :-> t/Any] :-> [(t/Val 1) :-> t/Any]])]
               (t/ann-form res [[(t/Val 1) :-> t/Any] :-> [(t/Val 1) :-> t/Any]]))))
  (is-tc-e (let [res (t/ann-form (map #(do %))
                                 (t/Transducer (t/Val 1) t/Infer))]
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
  (binding [clojure.core.typed.util-vars/*verbose-types* true]
    (is-tc-e (let [into (t/ann-form into (t/All [x y :named [a]] [(t/Vec x) (t/Transducer y x) (t/Seqable y) :-> (t/Vec x)]))
                   res (into [] (map #(do %)) [1])]
               (clojure.core.typed/print-env "foo")
               (t/ann-form res (t/Vec (t/Val 1)))
               nil)))
  )

(deftest reduce-test
  (is-tc-e (reduce (fn [a :- t/Int, b :- t/Int] (+ a b)) 0 [1])
           t/Int)
  ;; (All [a c] [[a c :-> (t/U (t/Reduced a) a)] a (t/Seqable c) :-> a])
  (binding [vs/*verbose-types* true]
    (is-tc-e (fn [reduce :- (t/All [a] [[a a :-> a] (t/NonEmptySeqable a) :-> a])]
               ;:- t/Int
               (reduce (fn [a b] (+ a b)) [1]))))
  (is-tc-e (reduce (fn [a b] (+ a b)) [1]))
  (is-tc-e (reduce (fn [a b] (+ a b)) [1]) Long)
  #_;;TODO
  (is-tc-e (reduce (fn [a b]
                     (reduce + a b))
                   1 [[1]]))
  (is-tc-err (reduce (fn [a b] (+ a b)) [1]) (t/Val 1))
  #_;;TODO
  (is-tc-e (reduce (fn [a b] (+ a (if b 1 2))) 1 [true]))
  )

(deftest comp-test
  (is-tc-e (fn [comp :- (t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])]
             (comp (fn [y] y)
                   (fn [x] x))))
  (is-tc-e (fn [comp :- (t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])]
             (let [f (comp (fn [y] y)
                           (fn [x] x))]
               (f 1))))
  (is-tc-e (fn [comp :- (t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])]
             :- (t/Val 1)
             (let [f (comp (fn [y] y)
                           (fn [x] x))]
               (f 1))))
  (is-tc-err (fn [comp :- (t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])]
               :- (t/Val 0)
               (let [f (comp (fn [y] y)
                             (fn [x] x))]
                 (f 1))))
  (is-tc-e (fn [comp :- (t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])
                f :- (t/Transducer t/Int t/Bool)
                g :- (t/Transducer t/Bool t/Int)]
             (comp f g)))
  (is-tc-e (fn [comp :- (t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])
                f :- (t/Transducer t/Any t/Any)
                g :- (t/Transducer t/Any t/Any)]
             (comp f g)))
  (is-tc-e (fn* [comp f g] (comp f g))
           [(t/All [x y z] [[y :-> z] [x :-> y] :-> [x :-> z]])
            (t/Transducer t/Any t/Any)
            (t/Transducer t/Any t/Any)
            :-> t/Infer])
  #_;;FIXME
  (is-tc-e (comp (t/ann-form (map #(do %))
                             (t/Transducer t/Nothing t/Any))
                 (t/ann-form (map #(do %))
                             (t/Transducer t/Nothing t/Any))))
  #_;;FIXME
  (is-tc-e (comp (t/ann-form (map identity)
                             (t/Transducer t/Nothing t/Any))
                 (t/ann-form (map identity)
                             (t/Transducer t/Nothing t/Any))))
  (is-tc-e (comp (fn [a :- t/Any])))
  #_;;FIXME
  ;; WIP demote-check-free with invariant dotted variable
  (is-tc-e (fn [comp :- (t/All [x y :..] [[y :.. y :-> x] :-> [y :.. y :-> x]])]
             ;; comp :- (t/All [x y :..] [[y :.. y :-> x] :-> [y :.. y :-> x]])
             ;; comp :- (t/All [x y :..] [[Nothing :.. :? :-> ?] :-> [y :.. y :-> x]])
             (comp (fn [a]))))
  (is-tc-e [] '[t/Any :*])
  (is-tc-err (fn []) [t/Any :* :-> t/Any])
  (is-tc-e (fn []) (t/U [:-> nil]
                        [t/Any :-> nil]
                        [t/Any t/Any :-> nil]))
  (is-tc-e (let [f (t/ann-form (fn [])
                               (t/U [:-> t/Infer]
                                    [t/Any :-> nil]
                                    [t/Any t/Any :-> nil]))]
             (t/ann-form f
                         (t/U [:-> nil]
                              [t/Any :-> nil]
                              [t/Any t/Any :-> nil]))))
  (is-tc-err (let [f (t/ann-form (fn [])
                                 (t/U [:-> t/Infer]
                                      [t/Any :-> nil]
                                      [t/Any t/Any :-> nil]))]
               (t/ann-form f
                           (t/U [:-> true]
                                [t/Any :-> nil]
                                [t/Any t/Any :-> nil]))))
  (is-tc-err (fn []) (t/U [:-> true]
                          [t/Any :-> true]
                          [t/Any t/Any :-> true]))
  (is-tc-err (fn []) [t/Nothing :* :-> t/Any])
  (is (r/HSequential? (parse-clj `(t/HSequential [t/Nothing] :repeat true))))
  (is-tc-e (fn [& _]) [(t/HSequential [t/Nothing] :repeat true) <* :-> t/Any])
  #_;;FIXME
  (is-tc-e (fn [comp :- (t/All [x y :..] [[y :.. y :-> x] :-> [y :.. y :-> x]])]
             (comp identity)))
  (is-tc-e (fn [comp :- (t/All [x y :..] [[y :.. y :-> x] :-> [y :.. y :-> x]])]
             :- [t/Any :-> t/Any]
             (comp (fn [a]))))
  #_;;FIXME
  (is-tc-e (fn [comp :- (t/All [x y :..] [[y :.. y :-> x] :-> [y :.. y :-> x]])]
             :- [t/Any :-> t/Any]
             (comp identity)))
  #_;;FIXME
  (is-tc-e (comp (fn [a])))
  #_;;FIXME
  (is-tc-e (comp identity))
  #_
  (is-tc-e (comp (map identity)))
  #_;;FIXME
  ;; needs better support for dotted types in prepping expected types for deferred args
  (is-tc-e (comp (map #(do %))
                 (map #(do %))))
  #_;;FIXME
  (is-tc-err (comp (map #(do %))
                   (map #(do %)))
             (t/Transducer t/Nothing t/Nothing))
  #_;;FIXME
  (is-tc-err (comp (map identity)
                   (map identity))
             (t/Transducer t/Nothing t/Nothing))
  (is-tc-err (comp (t/ann-form (map #(do %))
                               (t/Transducer t/Any t/Any))
                   (t/ann-form (map #(do %))
                               (t/Transducer t/Any t/Any)))
             (t/Transducer t/Nothing t/Nothing))
  #_;TODO
  (is-tc-e (comp (t/ann-form (map #(do %))
                             (t/Transducer t/Any t/Any))
                 (t/ann-form (map #(do %))
                             (t/Transducer t/Any t/Any)))
           (t/Transducer t/Any t/Any))
  #_;TODO
  (is-tc-e (into [] (comp (t/ann-form (map #(do %))
                                      (t/Transducer t/Any t/Any))
                          (t/ann-form (map #(do %))
                                      (t/Transducer t/Any t/Any)))
                 [1]))
  #_;TODO
  (is-tc-e (into [] (comp (map #(do %))
                          (map #(do %)))
                 [1]))
  #_;TODO
  (is-tc-e (comp (map #(do %))
                 (map #(do %)))
           (t/Transducer '1 '1))
  #_;TODO
  (is-tc-e (comp (fn [y] y)
                 (fn [x y] y))))

(deftest update-test
  (is-tc-err #(update {} :a inc))
  (is-tc-e (update {:a 1} :a inc))
  (is-tc-err (fn [x y]
               (update x y inc)
               nil)
             (t/All [x y] [x y :-> nil])))
