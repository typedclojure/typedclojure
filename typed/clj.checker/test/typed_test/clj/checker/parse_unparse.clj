(ns typed-test.clj.checker.parse-unparse
  (:require [typed.clojure :as t]
            [clojure.test :refer :all]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.test-utils :refer :all]))

(deftest unparse-free-scoping-test
  (is-clj (= (second
               (prs/unparse-type
                 (prs/parse-type 
                   `(t/All ~'[a b] [t/Any t/Any :-> t/Any]))))
             '[a b]))
  (is-clj (= (rest (prs/unparse-type (prs/parse-type `(t/TFn ~'[[a :variance :covariant]] ~'a))))
             '([[a :variance :covariant]] a)))
  (is-clj (= (do
               '([a b] [a b :-> [a b :-> nil :filters {:then ff :else tt}]
                        :filters {:then tt :else ff}]))
             (->
               (tc-e
                 (fn :forall [a b]
                   [f :- a, coll :- b]
                   (fn
                     [x :- a
                      y :- b])))
               :t
               prs/unparse-type
               rest))))

(deftest bad-dots-Poly-test
  ;; no dots in variable
  (is (throws-tc-error?
        (prs/parse-clj '(clojure.core.typed/All [... a] [a -> a]))))
  (is (throws-tc-error?
        (prs/parse-clj '(clojure.core.typed/All [. a] [a -> a]))))
  (is (throws-tc-error?
        (prs/parse-clj '(clojure.core.typed/All [. a] [a -> a]))))
  ; no nil/true/false
  (is (throws-tc-error?
        (prs/parse-clj `(clojure.core.typed/All [~(symbol "nil")] [nil :-> nil]))))
  (is (throws-tc-error?
        (prs/parse-clj `(clojure.core.typed/All [~(symbol "true")] [nil :-> nil]))))
  (is (throws-tc-error?
        (prs/parse-clj `(clojure.core.typed/All [~(symbol "false")] [nil :-> nil]))))
  ; no ns qualified
  (is (throws-tc-error?
        (prs/parse-clj `(clojure.core.typed/All [a/b] [nil :-> nil]))))
  ; non-symbol
  (is (throws-tc-error?
        (prs/parse-clj `(clojure.core.typed/All [:a] [nil :-> nil]))))
  ; bad kw args
  (is (throws-tc-error?
        (prs/parse-clj `(clojure.core.typed/All [:a :b] [nil :-> nil]))))
  ; missing rest arg
  (is (throws-tc-error?
        (prs/parse-clj `[:* :-> nil])))
  ; missing plus arg
  (is (throws-tc-error?
        (prs/parse-clj `[:+ :-> nil])))
  ; missing dotted arg
  (is (throws-tc-error?
        (prs/parse-clj `[:.. :-> nil])))
  ; missing optional arg
  (is (throws-tc-error?
        (prs/parse-clj `[:? :-> nil])))
  (is (throws-tc-error?
        (prs/parse-clj `[<... :-> nil])))
  )

(deftest poly-named-test
  (is (= (prs/unparse-type
           (prs/parse-clj 
             '(typed.clojure/All [:named [a b]] [a -> b])))
         '(typed.clojure/All [:named [a b]] [a :-> b])))
  (is (= (prs/unparse-type
           (prs/parse-clj 
             '(typed.clojure/All [a ... :named [b c]]
                                 [c b a ... a -> b])))
         '(typed.clojure/All [a ... :named [b c]]
                             [c b a :.. a :-> b])))
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [:named [a b]]
                             [a -> b]))
               (def foo identity)
               (t/inst foo :named {a t/Num b t/Num}))
           [t/Num :-> t/Num])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [:named [a b]]
                             [a -> b]))
               (def foo identity)
               (t/inst foo :named {a t/Num}))
           [t/Num :-> t/Any])
  (is-tc-err (do (t/ann ^:no-check foo 
                        (t/All [:named [a b]]
                               [a -> b]))
                 (def foo identity)
                 (t/inst foo :named {a t/Num}))
             [t/Any :-> t/Num])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [:named [a b]]
                             [a -> b]))
               (def foo identity)
               (t/inst foo))
           [t/Any :-> t/Any])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [a ...]
                             [a ... a -> t/Any]))
               (cc/defn foo [& args])
               (t/inst foo t/Str t/Bool))
           [t/Str t/Bool :-> t/Any])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [a ... :named [b c]]
                             [c b a ... a -> b]))
               (cc/defn foo [& args] (second args))
               (t/inst foo t/Str t/Bool :named {c t/Num b t/Sym}))
           [t/Num t/Sym t/Str t/Bool :-> t/Sym]))


(deftest bad-All-test
  (is (throws-tc-error?
        (prs/parse-clj `(t/All nil [])))))

(deftest bad-HVec-test
  (is (throws-tc-error?
        (prs/parse-clj `(t/HVec :a)))))

(deftest Infer-test
  (is (r/wild? (prs/parse-clj `t/Infer))))

(deftest regex-test
  (is (= (prs/parse-clj `[(t/? t/Any) :-> t/Any])
         (prs/parse-clj `(t/IFn [:-> t/Any]
                                [t/Any :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/? t/Bool) t/Any (t/? t/Int) :-> t/Any])
         (prs/parse-clj 
           `(t/IFn [t/Any :-> t/Any]
                   [t/Any t/Int :-> t/Any]
                   [t/Bool t/Any :-> t/Any]
                   [t/Bool t/Any t/Int :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/* t/Bool) :-> t/Any])
         (prs/parse-clj `[t/Bool :* :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[(t/* t/Bool) (t/* t/Bool) :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[(t/+ t/Bool) (t/* t/Bool) :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[(t/* t/Bool) (t/+ t/Bool) :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[(t/+ t/Bool) (t/+ t/Bool) :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[(t/* t/Bool) t/Bool :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[(t/* t/Bool) (t/? t/Bool) :-> t/Any])))
  (is (throws-tc-error? (prs/parse-clj `[:-> (t/? t/Bool)])))
  (is (= (prs/parse-clj `[(t/? t/Int) (t/* t/Bool) :-> t/Any])
         (prs/parse-clj `(t/IFn [t/Bool :* :-> t/Any]
                                [t/Int t/Bool :* :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/? t/Int) (t/+ t/Bool) :-> t/Any])
         (prs/parse-clj `(t/IFn [t/Bool :+ :-> t/Any]
                                [t/Int t/Bool :+ :-> t/Any]))
         (prs/parse-clj `(t/IFn [t/Bool t/Bool :* :-> t/Any]
                                [t/Int t/Bool t/Bool :* :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/cat t/Int t/Bool) :-> t/Any])
         (prs/parse-clj `[t/Int t/Bool :-> t/Any])))
  (is (= (prs/parse-clj `[(t/cat t/Int t/Bool) (t/? t/Any) :-> t/Any])
         (prs/parse-clj `(t/IFn [t/Int t/Bool :-> t/Any]
                                [t/Int t/Bool t/Any :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/* (t/cat t/Int t/Bool)) :-> t/Any])
         (prs/parse-clj `[(t/HSequential [t/Int t/Bool] :repeat true) ~'<* :-> t/Any])))
  (is (= (prs/parse-clj `(t/All [x# y#] [[x# x# :-> t/Int] (t/* (t/cat x# y#)) :-> (t/Map x# y#)]))
         (prs/parse-clj `(t/All [x# y#] [[x# x# :-> t/Int] (t/HSequential [x# y#] :repeat true) ~'<* :-> (t/Map x# y#)]))))
  (is (= (prs/parse-clj `[(t/alt (t/cat (t/? java.io.Reader))
                                 (t/cat java.io.Reader t/Bool)
                                 (t/cat java.io.Reader t/Bool t/Any (t/? t/Bool)))
                          :-> t/Any])
         (prs/parse-clj `(t/IFn [:-> t/Any]
                                [java.io.Reader :-> t/Any]
                                [java.io.Reader t/Bool :-> t/Any]
                                [java.io.Reader t/Bool t/Any :-> t/Any]
                                [java.io.Reader t/Bool t/Any t/Bool :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/? t/Bool) (t/? t/Int) :-> t/Any])
         (prs/parse-clj `(t/IFn [:-> t/Any]
                                [t/Int :-> t/Any]
                                [t/Bool :-> t/Any]
                                [t/Bool t/Int :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/alt (t/? t/Bool) (t/? t/Int) (t/* t/Int)) :-> t/Any])
         (prs/parse-clj `(t/IFn [:-> t/Any]
                                [t/Bool :-> t/Any]
                                [t/Int :-> t/Any]
                                [t/Int :* :-> t/Any]))))
  (is (= (prs/parse-clj `(t/IFn [(t/? (t/cat (t/? t/AnyInteger) t/Num)) :-> (t/ASeq t/AnyInteger)]
                                [t/AnyInteger t/Num t/AnyInteger :-> (t/ASeq t/AnyInteger)]
                                [t/Num t/Num (t/? t/Num) :-> (t/ASeq t/Num)]))
         (prs/parse-clj `(t/IFn [:-> (t/ASeq t/AnyInteger)]
                                [t/Num :-> (t/ASeq t/AnyInteger)]
                                [t/AnyInteger t/Num :-> (t/ASeq t/AnyInteger)]
                                [t/AnyInteger t/Num t/AnyInteger :-> (t/ASeq t/AnyInteger)]
                                [t/Num t/Num :-> (t/ASeq t/Num)]
                                [t/Num t/Num t/Num :-> (t/ASeq t/Num)]))))
  (is (= (prs/parse-clj `(t/All [x#] [(t/Sorted x#) [t/Int t/Int :-> t/Bool] t/Int (t/? (t/cat t/Int t/Int t/Int)) :-> (t/Nilable (t/ASeq x#))]))
         (prs/parse-clj `(t/All [y#] (t/IFn [(t/Sorted y#) [t/Int t/Int :-> t/Bool] t/Int :-> (t/Nilable (t/ASeq y#))]
                                            [(t/Sorted y#) [t/Int t/Int :-> t/Bool] t/Int t/Int t/Int t/Int :-> (t/Nilable (t/ASeq y#))])))))
  (is (= (prs/parse-clj `[(t/alt (t/cat (t/U t/Keyword t/Sym t/Str))
                                 (t/cat t/Str t/Str))
                          :-> (t/Option t/Keyword)])
         (prs/parse-clj `(t/IFn [(t/U t/Keyword t/Sym t/Str) :-> (t/Option t/Keyword)]
                                [t/Str t/Str :-> (t/Option t/Keyword)]))))
  (is (= (prs/parse-clj `(t/All [a#] [t/Int (t/? t/Int) (t/? t/Int) (t/Seqable a#) :-> (t/ASeq (t/NonEmptyASeq a#))]))
         (prs/parse-clj `(t/All [a#] (t/IFn [t/Int (t/Seqable a#) :-> (t/ASeq (t/NonEmptyASeq a#))]
                                            [t/Int t/Int (t/Seqable a#) :-> (t/ASeq (t/NonEmptyASeq a#))]
                                            [t/Int t/Int t/Int (t/Seqable a#) :-> (t/ASeq (t/NonEmptyASeq a#))])))))
)
