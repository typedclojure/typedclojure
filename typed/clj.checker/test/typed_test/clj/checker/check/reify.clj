(ns typed-test.clj.checker.check.reify
  (:require [clojure.test :refer [deftest is testing]]
            [typed.cljc.checker.filter-ops :as fops]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.object-rep :as orep]
            [typed.clj.checker.test-utils :refer :all]))

(deftest reify-test
  (is-tc-e (reify))
  (is-tc-e (reify)
           :expected-ret (r/ret (c/RClass-of Object)))
  (is-tc-e (reify)
           :expected-ret (r/ret (c/RClass-of Object)
                                (fops/-true-filter)))
  (is-tc-err (reify)
             :expected-ret (r/ret (c/RClass-of Object)
                                  (fops/-false-filter)))
  (is-tc-e (reify)
           :expected-ret (r/ret (c/RClass-of Object)
                                (fops/-true-filter)
                                orep/-empty))
  (is-tc-err (reify)
             :expected-ret (r/ret (c/RClass-of Object)
                                  (fops/-true-filter)
                                  (orep/-path nil 'a)))
  (is-tc-e ^:foo (reify))
  (is-tc-e (reify) Object)
  (is-tc-e (with-meta (reify) nil))
  (is-tc-err (reify) nil)
  (is-tc-err (reify) t/Int)
  (is-tc-e (reify Object
             (toString [_] "a")))
  (is-tc-e (reify java.io.Serializable
             clojure.lang.Sequential)
           (t/U java.io.Serializable clojure.lang.Sequential))
  (is-tc-e (reify java.io.Serializable
             clojure.lang.Sequential)
           (t/U java.io.Serializable (t/I clojure.lang.Sequential clojure.lang.IObj)))
  (is-tc-e (reify java.io.Serializable
             clojure.lang.Sequential)
           (t/U java.io.Serializable t/Str))
  (is-tc-e (reify ^{:typed.clojure/replace [:-> t/Str]}
             clojure.lang.IFn
             (invoke [_] "a")))
  (is-tc-err (reify ^{:typed.clojure/replace [t/Str :-> t/Str]}
               clojure.lang.IFn
               (invoke [_ a])))
  (is-tc-e (reify ^{:typed.clojure/replace [t/Str :-> t/Str]}
             clojure.lang.IFn
             (invoke [_ a] a)))
  (is-tc-e (reify ^{:typed.clojure/replace (t/IFn [:-> t/Str]
                                             [t/Str :-> t/Str])}
             clojure.lang.IFn
             (invoke [this] (this "a"))
             (invoke [_ a] a)))
  (is-tc-err (reify ^{:typed.clojure/replace (t/IFn [:-> t/Str]
                                               [t/Str :-> t/Str])}
               clojure.lang.IFn
               (invoke [_ a] a)))
  (is-tc-e (reify ^{:typed.clojure/replace [t/Str :-> t/Str]}
             clojure.lang.IFn
             (invoke [this] (this "a"))
             (invoke [_ a] a)))
  (is-tc-e (reify ^{:typed.clojure/replace [t/Str :-> t/Str]}
             clojure.lang.IFn
             (clojure.lang.IFn/invoke [this] (this "a"))
             (invoke [_ a] a)))
  (is-tc-e (reify ^{:typed.clojure/replace [:-> t/Str]}
             clojure.lang.IFn
             (invoke [_] "a"))
           [:-> t/Str])
  (is-tc-err (reify ^{:typed.clojure/replace [:-> t/Str]}
               clojure.lang.IFn
               (invoke [_] "a"))
             [:-> t/Int])
  (is-tc-err (reify ^{:typed.clojure/replace [:-> t/Str]}
               clojure.lang.IFn
               (invoke [_] nil)))
  #_ ;;TODO
  (is-tc-err (reify ^{:typed.clojure/replace [t/Str * :-> t/Str]}
               clojure.lang.IFn
               (invoke [_ s] nil)))
  ;; checks as clojure.lang.IFn, not as [:-> t/Str]
  ;; TODO improve error message: "AnyFunction is not a function type"
  (is-tc-err (reify clojure.lang.IFn
               (invoke [_] "a"))
             (t/I clojure.lang.IObj [:-> t/Str]))
  (is-tc-err (reify clojure.lang.IFn
               (invoke [_] nil))
             (t/I clojure.lang.IObj [:-> t/Str]))
  #_ ;;FIXME
  (is-tc-err (reify Object
               (toString [_] nil)))
  (is-tc-e (do (defprotocol Foo (foo [this a :- t/Int] :- t/Int))
               (reify Foo
                 (foo [_ a] a))))
  (is-tc-err (do (defprotocol Foo (foo [this a :- t/Int] :- t/Int))
                 (reify Foo
                   (foo [_ a] (t/ann-form a nil)))))
  (is-tc-err (do (defprotocol Foo (foo [this a :- t/Int] :- t/Int))
                 (reify Foo
                   (foo [_ a] nil))))
  (is-tc-e (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
               (ann-form (reify ^{:typed.clojure/replace (Foo t/Any)}
                           Foo
                           (foo [_ a] a))
                         (Foo t/Any))))
  ;; TODO infer ^{:typed.clojure/replace (Foo t/Int)}
  (is-tc-err (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
                 (ann-form (reify Foo
                             (foo [_ a] a))
                           (Foo t/Int))))
  ;; default covariant protocol parameters to upper bound
  (is-tc-e (do (defprotocol [[x :variance :covariant]] Foo (foo [this a :- x] :- x))
               (ann-form (reify Foo
                           (foo [_ a] a))
                         (Foo t/Any))))
  (is-tc-err (do (defprotocol [[x :variance :covariant]] Foo (foo [this a :- x] :- x))
                 (ann-form (reify Foo
                             (foo [_ a] a))
                           ;; not upper bound of x
                           (Foo t/Int))))
  (is-tc-e (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
               (ann-form (reify ^{:typed.clojure/replace (Foo t/Int)} Foo
                           (foo [_ a] a))
                         (Foo t/Int))))
  (is-tc-e (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
               (ann-form (reify ^{:typed.clojure/replace (Foo t/Int)}
                           Foo
                           (foo [_ a] a))
                         (t/U (Foo t/Int)
                              (Foo t/Bool)))))
  (is-tc-err (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
                 (ann-form (reify ^{:typed.clojure/replace (Foo t/Int)}
                             Foo
                             (foo [_ a] nil))
                           (Foo t/Int))))
  #_  ;;TODO union type for method
  (is-tc-err (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
                 (ann-form (reify
                             Foo
                             (foo [_ a] (when (boolean? a) a)))
                           (t/U (Foo t/Int)
                                (Foo t/Bool)))))
  (is-tc-err (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
                 (reify ^{:typed.clojure/replace (Foo t/Int)}
                   Foo
                   (foo [_ a] nil))))
  (is-tc-e (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
               (defn :forall [x]
                 ->foo
                 [x] :- (Foo x)
                 (reify ^{:typed.clojure/replace (Foo x)}
                   Foo
                   (foo [_ a] a)))
               (t/ann-form (->foo nil) (Foo nil))))
  (is-tc-err (do (defprotocol [[x :variance :invariant]] Foo (foo [this a :- x] :- x))
                 (defn :forall [x]
                   ->foo
                   [x] :- (Foo x)
                   (reify  ^{:typed.clojure/replace (Foo x)}
                     Foo
                     (foo [_ a] nil))))))
