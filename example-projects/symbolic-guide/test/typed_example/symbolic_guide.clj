(ns typed-example.symbolic-guide
  "A guide on symbolic execution in Typed Clojure.
  
  `preamble` explains the testing primitives we use to demonstrate typing
  checking behavior.
  
  `guide` is the guide"
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :refer [is-tc-e is-tc-err]]
            [typed.clojure :as t]))

(deftest preamble
  ;; This guide uses two (clojure.test) helpers:
  ;; - is-tc-e ("this expression type checks") and
  ;; - is-tc-err ("this expression contains a static type error")
  ;; The arguments are like `ann-form` (expression then expected type),
  ;; but the expected type is optional
  (is-tc-e 1) ;; 1 type checks with no expected type
  (is-tc-e 1 t/Int) ;; 1 type checks as Int
  (is-tc-err 1 t/Bool) ;; 1 fails to type check as Bool

  ;; Each expression is evaluated in its own namespace,
  ;; requiring [clojure.core.typed :refer :all :as t] and [clojure.core :as cc].
  (is-tc-e (fn [a :- t/Int] (cc/fn [] a))
           [t/Int :-> [:-> t/Int]])

  ;; To simulate a top-level expression, use a `do` form
  (is-tc-e (do (t/ann foo t/Int)
               (def foo 1)
               (inc foo))
           t/Int)
  )

(deftest guide
  ;; Symbolic execution is used in Typed Clojure when type information is
  ;; not readily available for function parameters. For example, here
  ;; the first argument lacks type information.
  (is-tc-e #(inc %))
  ;; It type checks because the type system determines the function body is not reachable,
  ;; and so it does not need to check the body.
  ;;
  ;; In fact, the body _is_ checked but with % at type t/Nothing (the bottom/impossible/unreachable type).
  (is-tc-e #(inc (t/ann-form % t/Nothing)))
  ;; This means you can have garbage in your function body, and it will type check.
  (is-tc-e #(identity (inc nil) % % %))
  ;; When the function body is reachable, a more realistic type will be used to check.
  ;; 
  ;; Here are some common ways this can happen:
  ;;
  ;; 1) an expected function type is provided.
  (is-tc-e   #(inc %) [t/Int  :-> t/Int])
  (is-tc-err #(inc %) [t/Bool :-> t/Int])
  ;; 2) a parameter is explictly annotated.
  (is-tc-e   (fn [a :- t/Int]  (inc a)))
  (is-tc-err (fn [a :- t/Bool] (inc a)))
  ;; 3) the function is invoked.
  (is-tc-e   (let [f #(inc %)] (f 1)))
  (is-tc-err (let [f #(inc %)] (f true)))
  ;; 4) the function is passed a higher-order function
  (is-tc-e   (map #(inc %) [1]))
  (is-tc-err (map #(inc %) [true]))
  ;; 5) the function is passed through several layers of higher-order functions
  (is-tc-e   (into [] (map #(inc %)) [1]))
  (is-tc-err (into [] (map #(inc %)) [true]))
  ;;
  ;; A special type called a "symbolic closure" is used to stand for the delayed checking
  ;; of a function. A symbolic closure is similar to a function intersection type, except
  ;; the intersection is built on-the-fly instead of provided by an t/IFn type annotation.
  ;;
  ;; The first expression below uses a function intersection, and the second uses a symbolic closure
  ;; to check the same calls.
  (is-tc-e (let [f (-> (fn [a] a)
                       ;; checks the function body twice, once for each function type
                       (t/ann-form (t/IFn [t/Int :-> t/Int]
                                          [t/Str :-> t/Str])))]
             ;; since 1 :- t/Int, this call is valid via the first function type
             (inc (f 1))
             ;; since "a" :- t/Str, this call is valid via the second function type
             (subs (f "a") 0)))
  (is-tc-e (let [;; check f's body with `a :- t/Nothing`.
                 ;; infers type `f :- (SymbolicClosure (fn [a] a))`
                 f (fn [a] a)]
             ;; check f's body with `a :- t/Int`
             (inc (f 1))
             ;; check the body with `a :- t/Str`
             (subs (f "a") 0)))
  ;; Symbolic closures are very flexible, and can be passed around with polymorphic functions
  ;; like any other type.
  (is-tc-e (let [f (identity #(do %))]
             (inc (f 1))
             (subs (f "a") 0)))
  ;; Some juggling:
  (is-tc-e (let [f (->> #(do %) (repeat 10) identity (map #(do %)) rand-nth)]
             (inc (f 1))
             (subs (f "a") 0)))
  ;; Transducers:
  (is-tc-e   (let [xf (map #(inc %))]
               (into [] xf [1])))
  (is-tc-e   (let [f #(inc %)]
               (into [] (map f) [1])))

  ;; Function-returning functions can themselves be checked symbolically if they do not
  ;; provide sufficient type information to an anonymous function.
  ;; 
  ;; Here, the anonymous function is not type checked.
  (is-tc-e   (map #(inc %)))
  (is-tc-e   (map #(inc (t/ann-form % t/Nothing)))) ;; proves the body is not checked
  ;; How can we be so confident the function is unreachable? The type of `map` can
  ;; be instantiated like so:
  ;;
  ;;    map : [[t/Nothing :-> t/Any] :-> (t/Transducer t/Nothing t/Any)]
  ;;
  ;; This tells us that `map` cannot call its function argument, since t/Nothing is an unreachable type.
  ;;
  ;; When an expected type is provided, we can no longer be so optimistic.
  ;; The anonymous function now must accept the same type as the "in" of (t/Transducer in out).
  (is-tc-e   (map #(inc %)) (t/Transducer t/Int t/Int))
  (is-tc-err (map #(inc %)) (t/Transducer t/Bool t/Int))

  ;; We're using clojure.core functions above, but there's nothing special about them.
  ;; Only their _types_ are used for inference---you can make your own higher-order functions
  ;; and they will infer exactly the same as the core functions.
  (is-tc-e   (fn [my-map :- (t/All [a c] [[a :-> c] :-> (t/Transducer a c)])]
               (into [] (my-map #(inc %)) [1])))
  (is-tc-err (fn [my-map :- (t/All [a c] [[a :-> c] :-> (t/Transducer a c)])]
               (into [] (my-map #(inc %)) [true])))

  ;; Error messages involving symbolic closures passed to polymorphic functions are problematic
  ;; since it is not obvious who exactly to blame for the error (the symbolic closure's body,
  ;; or the arguments to the polymorphic function?).
  ;;
  ;; If you are having trouble identifying why a symbolic closure fails to type check,
  ;; try annotating the function's arguments to localize the error message.
  (is-tc-err (map (fn [a]           (inc a)) [true])) ;; not much information is given.
  (is-tc-err (map (fn [a :- t/Bool] (inc a)) [true])) ;; the `inc` call is the problem.

  ;; The major tradeoff is that type checking is now (certainly) Turing complete. The rationale
  ;; is that this is preferable to writing annotations.
  ;;
  ;; If you get stackoverflow errors, try annotating some functions or simply wrapping
  ;; the problematic code with `t/tc-ignore`.
  ;;
  ;; For example, here's a Y-combinator encoding of an infinite loop---which the type
  ;; checker follows indefinitely.
  (is (thrown? StackOverflowError
               (is-tc-err #(let [Y (fn [f]
                                     ((fn [g] (fn [x] (f (g g) x)))
                                      (fn [g] (fn [x] (f (g g) x)))))]
                             (let [compute (Y (fn [f x] (+ 1 (f x))))]
                               (compute 1))))))

  ;; Note that symbolic closures are not created for thunks, since they can be checked immediately.
  (is-tc-err #(identity nil nil))
  )
