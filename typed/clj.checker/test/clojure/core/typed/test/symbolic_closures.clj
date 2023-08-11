(ns clojure.core.typed.test.symbolic-closures
  (:require 
    [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err]]
    [typed.cljc.checker.type-rep :as r]
    [clojure.test :refer :all]))

(deftest symbolic-closure-test
  ;; thunks always checked
  (is-tc-err #(identity))
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
  (is-tc-e (let [f (core/fn [x] x)]
             (f 1)))
  #_ ;;FIXME
  (is-tc-e (let [f (t/fn [x] x)]
             (f 1))
           t/Int)
  (is-tc-err (let [f (fn* [x] x)]
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
  ;; WIP delay poly app with fn return
  ; 1 <: x
  ; #(+ %1 %2) <: [x y -> z]
  #_ ;;WIP
  (is-tc-e (do (t/ann ^:no-check curry (t/All [x y z] [[x y -> z] x -> [y -> z]]))
               (def curry cc/partial)
               (let [f (curry #(+ %1 %2) 1)]
                 (comment
                   (SymbolicClosure (curry #(+ %1 %2) 1)) :- [t/Int :-> t/Int]
                   )
                 ; 1 <: x
                 ; #(+ %1 %2) <: [x y -> z]
                 ; 2 <: y
                 ;;FIXME y is being substited as Any...
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
  (is-tc-err (do (t/ann app (t/All [x y] [[x :-> [x :-> y]] x :-> y]))
                 (def app #((%1 %2) %2))
                 (app #(do % (fn [x] (t/ann-form x (t/Val 1)) true)) 1)))
  )

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
