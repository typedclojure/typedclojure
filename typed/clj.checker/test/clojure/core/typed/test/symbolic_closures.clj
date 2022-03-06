(ns clojure.core.typed.test.symbolic-closures
  (:require 
    typed.clj.checker.test-utils
    [typed.cljc.checker.type-rep :as r]
    [clojure.test :refer :all]))

(defmacro is-tc-e [& args]
  `(binding [r/enable-symbolic-closures? true]
     (typed.clj.checker.test-utils/is-tc-e ~@args)))

(defmacro is-tc-err [& args]
  `(binding [r/enable-symbolic-closures? true]
     (typed.clj.checker.test-utils/is-tc-err ~@args)))

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
  (is-tc-e (do (t/ann ^:no-check app (t/All [x y] [[x :-> y] x :-> y]))
               (def app #(%1 %2))
               (app #(inc %) 1))
           t/Int)
  (is-tc-err (do (t/ann ^:no-check app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (app #(inc %1 %2) 1)))
  (is-tc-err (do (t/ann ^:no-check app (t/All [x y] [[x :-> y] x :-> y]))
                 (def app #(%1 %2))
                 (app #(inc %1 %2) 1))
             t/Int)
  (is-tc-err (do (t/ann ^:no-check app (t/All [x y] [[x :-> y] x :-> y]))
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
           t/Int)
  #_ ;;FIXME
  (is-tc-e (let [f (fn* [x] x)]
             (map f [1]))
           (t/Seqable t/Int))
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
