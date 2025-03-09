(ns ^:typed.clojure typed-test.clj.checker.parse-unparse
  (:require [typed.clojure :as t]
            [clojure.test :refer :all]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.filter-rep :as fr]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.test-utils :refer :all]))

(def this-nsym (ns-name *ns*))

(deftest parse-type-test
  (is-clj (= (c/Poly-body* '(x) (prs/parse-type '(clojure.core.typed/All [x] x) (clj-opts)) (clj-opts))
             (r/make-F 'x)))
  (is-clj (= (c/Poly-body* '(x y) (prs/parse-type '(clojure.core.typed/All [x y] x) (clj-opts)) (clj-opts))
             (r/make-F 'x)))
  (is-clj (= (c/Poly-body* '(x y) (prs/parse-type '(clojure.core.typed/All [x y] y) (clj-opts)) (clj-opts))
             (r/make-F 'y)))
  (is-clj (= (c/Poly-body* '(a b c d e f g h i) (prs/parse-type '(clojure.core.typed/All [a b c d e f g h i] e) (clj-opts)) (clj-opts))
             (r/make-F 'e))))

(deftest polydots-unparse-test
  (is-clj (= '[a b :..]
             (second
               (prs/unparse-type
                 (prs/parse-type
                   '(clojure.core.typed/All [a b :..]) (clj-opts))
                 (clj-opts))))))

(deftest fn-type-parse-test
  (is (not= (fo/-FS fr/-bot fr/-bot)
            (fo/-FS fr/-top fr/-bot)))
  (is (not= (fo/-FS fr/-bot fr/-bot)
            (fo/-FS fr/-bot fr/-top)))
  (is (= (:then (fo/-FS fr/-bot fr/-bot))
         fr/-bot))
  (is (= (:else (fo/-FS fr/-bot fr/-bot))
         fr/-bot))
  (is (= (prs/parse-clj `[t/Any :-> t/Any])
         (prs/parse-clj `[t/Any :-> t/Any
                          :filters {:then ~'tt :else ~'tt}])))
  (is (-> (prs/parse-clj `[t/Any :-> t/Any
                           :filters {:then ~'ff :else ~'ff}])
          :types first :rng :fl
          (= (fo/-FS fr/-bot fr/-bot))))
  (is (-> (prs/parse-clj `[t/Any :-> t/Any
                           :filters {:then ~'ff :else ~'ff}])
          :types first :rng :fl :then fr/BotFilter?))
  (is (-> (prs/parse-clj `[t/Any :-> t/Any
                           :filters {:then ~'ff :else ~'ff}])
          :types first :rng :fl :else fr/BotFilter?)))

(deftest parse-type-fn-test
  (is-clj (= (prs/parse-type '[nil :* -> nil] (clj-opts))
             (r/make-FnIntersection (r/make-Function [] r/-nil :rest r/-nil))))
  (is-clj (= (prs/parse-type '(clojure.core.typed/All [x :..] [nil :.. x -> nil]) (clj-opts))
             (c/PolyDots* '(x) [r/dotted-no-bounds]
                          (r/make-FnIntersection (r/make-Function [] r/-nil :drest (r/DottedPretype1-maker r/-nil 'x)))
                          (clj-opts)))))

(defn unparse-in-opts []
  (prs/with-unparse-ns (clj-opts) this-nsym))
(defn parse-in-opts []
  (prs/with-parse-ns (clj-opts) this-nsym))

#_
(deftest parse-tfn-test
  (is-tc-e 1)
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]] (t/U t/Int t/Bool ~'a)) (clj-opts)) (unparse-in-opts))
         '(t/TFn [[a :variance :covariant]] (t/U t/Int t/Bool a))))
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]] (t/U t/Bool t/Int ~'a)) (clj-opts)) (unparse-in-opts))
         '(t/TFn [[a :variance :covariant]] (t/U t/Bool t/Int a))))
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]] (t/I t/Int t/Bool ~'a)) (clj-opts)) (unparse-in-opts))
         '(t/TFn [[a :variance :covariant]] (t/I t/Int t/Bool a))))
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]] (t/I t/Bool t/Int ~'a)) (clj-opts)) (unparse-in-opts))
         '(t/TFn [[a :variance :covariant]] (t/I t/Bool t/Int a))))
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]] (t/SequentialSeq ~'a)) (clj-opts)) (unparse-in-opts))
         '(t/TFn [[a :variance :covariant]] (t/SequentialSeq a))))
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]]
                                                   (t/I (t/SequentialSeq ~'a)
                                                        (Iterable ~'a)
                                                        (java.util.Collection ~'a)
                                                        (java.util.List ~'a)
                                                        clojure.lang.IObj)) (clj-opts))
                           (unparse-in-opts))
         '(t/TFn [[a :variance :covariant]] (t/I (t/SequentialSeq a) (Iterable a) (java.util.Collection a) (java.util.List a) IObj))))
  (is (= (prs/unparse-type (prs/parse-type `(t/TFn [[~'a :variance :covariant]] t/Type) (clj-opts)) (unparse-in-opts))
         '(t/TFn [[a :variance :invariant]] t/Type))))

(deftest unparse-free-scoping-test
  (is-clj (= (second
               (prs/unparse-type
                 (prs/parse-type 
                   `(t/All ~'[a b] [t/Any t/Any :-> t/Any]) (clj-opts)) (clj-opts)))
             '[a b]))
  (is-clj (= (rest (prs/unparse-type (prs/parse-type `(t/TFn ~'[[a :variance :covariant]] ~'a) (clj-opts)) (clj-opts)))
             '([[a :variance :covariant]] a)))
  (is-clj (= '([a b] [a b :-> [a b :-> nil]])
             (with-delayed-remove-ns
               (->
                 (tc-e
                   (fn :forall [a b]
                     [f :- a, coll :- b] :- [a b :-> nil]
                     (fn
                       [x :- a
                        y :- b]))
                   (t/All [a b] [a b :-> [a b :-> nil]]))
                 :t
                 (prs/unparse-type (clj-opts))
                 rest
                 )))))

(deftest bad-dots-Poly-test
  ;; no dots in variable
  (is (throws-tc-error?
        (prs/parse-clj '(clojure.core.typed/All [:.. a] [a -> a]))))
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
             '(typed.clojure/All [:named [a b]] [a -> b]))
           (clj-opts))
         '(typed.clojure/All [:named [a b]] [a :-> b])))
  (is (= (prs/unparse-type
           (prs/parse-clj 
             '(typed.clojure/All [a :.. :named [b c]]
                                 [c b a :.. a -> b])) (clj-opts))
         '(typed.clojure/All [a :.. :named [b c]]
                             [c b a :.. a :-> b]))))

(deftest bad-All-test
  (is (throws-tc-error?
        (prs/parse-clj `(t/All nil [])))))

(deftest bad-HVec-test
  (is (throws-tc-error?
        (prs/parse-clj `(t/HVec :a)))))

(deftest Infer-test
  (is (r/wild? (prs/parse-clj `t/Infer))))

(deftest expanded-regex-test
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
  (is (not= (prs/parse-clj `[(t/* t/Bool) :-> t/Any])
            (prs/parse-clj `[(t/HSequential [t/Bool] :repeat true) ~'<* :-> t/Any])))
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
  (is (= (prs/parse-clj `[(t/cat t/Int t/Bool) :* :-> t/Any])
         (prs/parse-clj `[(t/HSequential [t/Int t/Bool] :repeat true) ~'<* :-> t/Any])))
  (is (= (prs/parse-clj `(t/All [x# y#] [[x# x# :-> t/Int] (t/* (t/cat x# y#)) :-> (t/Map x# y#)]))
         (prs/parse-clj `(t/All [x# y#] [[x# x# :-> t/Int] (t/HSequential [x# y#] :repeat true) ~'<* :-> (t/Map x# y#)]))))
  (is (= (prs/parse-clj `(t/All [x# y#] [[x# x# :-> t/Int] (t/cat x# y#) :* :-> (t/Map x# y#)]))
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
  (is (= (prs/parse-clj `(t/All [m# k# v# c# :..] [m# k# v# (t/cat c# c#) :.. c# :-> (t/Assoc m# k# v# c# :.. c#)]))
         (prs/parse-clj `(t/All [m# k# v# c# :..] [m# k# v# (t/HSequential [c# c#] :repeat true) <... c# :-> (t/Assoc m# k# v# c# :.. c#)]))))
  (is (= (prs/parse-clj `(t/All [m# k# v# c# :..] [nil k# v# (t/cat c# c#) :.. c# :-> (t/Assoc nil k# v# c# :.. c#)]))
         (prs/parse-clj `(t/All [m# k# v# c# :..] [nil k# v# (t/HSequential [c# c#] :repeat true) <... c# :-> (t/Assoc nil k# v# c# :.. c#)]))))
  (is (= (prs/parse-clj `(t/All [m# k# v#] [nil k# v# (t/* (t/cat k# v#)) :-> (t/Map k# v#)]))
         (prs/parse-clj `(t/All [m# k# v#] [nil k# v# (t/* (t/cat k# v#)) :-> (t/Map k# v#)]))))
  (is (= (prs/parse-clj `(t/All [m# k# v#] [nil k# v# (t/* (t/cat k# v#)) :-> (t/Map k# v#)]))
         (prs/parse-clj `(t/All [m# k# v#] [nil k# v# (t/HSequential [k# v#] :repeat true) ~'<* :-> (t/Map k# v#)]))))
  (is (= (prs/parse-clj `(t/All [a# y#] [[(t/* a#) :-> y#] (t/* a#) :-> [(t/* a#) :-> y#]]))
         (prs/parse-clj `(t/All [a# y#] [[a# :* :-> y#] a# :* :-> [a# :* :-> y#]]))))
  (is (-> (prs/parse-clj `[(t/+ [t/Any :-> t/Any]) :-> [t/Any :* :-> t/Any]])
          :types
          first
          :rest
          boolean))
)

(deftest unexpanded-regex-test
  (is-tc-e :load-checker)
  (clj
    (let [t (prs/parse-clj `[(t/* t/Int) (t/* t/Bool) :-> t/Any])]
      (is (-> t :types count (= 1)) t)
      (is (-> t :types first :kind (= :regex)) t)
      (is (-> t :types first :regex :kind (= :cat)) t)
      (is (= 2 (-> t :types first :regex :types count)) t)))
  (clj
    (let [t (prs/parse-clj `[(t/* t/Int) t/Bool :* :-> t/Any])]
      (is (-> t :types count (= 1)) t)
      (is (-> t :types first :kind (= :regex)) t)
      (is (-> t :types first :regex :kind (= :cat)) t)
      (is (= 2 (-> t :types first :regex :types count)) t)))
  (clj
    (let [t (prs/parse-clj `[(t/* t/Int) (t/* t/Bool) :-> t/Any])]
      (is (-> t :types count (= 1)) t)
      (is (-> t :types first :kind (= :regex)) t)
      (is (-> t :types first :regex :kind (= :cat)) t)
      (is (= 2 (-> t :types first :regex :types count)) t)))
  (is (= (prs/parse-clj `[t/Int :*    t/Bool :* :-> t/Any])
         (prs/parse-clj `[t/Int :*    (t/* t/Bool) :-> t/Any])
         (prs/parse-clj `[(t/* t/Int) t/Bool :* :-> t/Any])
         (prs/parse-clj `[(t/* t/Int) (t/* t/Bool) :-> t/Any])))
  (is (= `[t/Int :* t/Bool :* :-> t/Any]
         (prs/unparse-type (prs/parse-clj `[(t/* t/Int) t/Bool :* :-> t/Any]) (clj-opts))))
  (is (= `[t/Bool :* t/Bool :-> t/Any]
         (prs/unparse-type (prs/parse-clj `[(t/* t/Bool) t/Bool :-> t/Any]) (clj-opts))))
  (is (= `[t/Int t/Int :* t/Bool t/Bool :* :-> t/Any]
         (prs/unparse-type (prs/parse-clj `[(t/+ t/Int) (t/+ t/Bool) :-> t/Any]) (clj-opts))))
  (is (= `[t/Int t/Int :* t/Bool :* :-> t/Any]
         (prs/unparse-type (prs/parse-clj `[(t/+ t/Int) (t/* t/Bool) :-> t/Any]) (clj-opts))))
  (is (= `[t/Int :* t/Bool t/Bool :* :-> t/Any]
         (prs/unparse-type (prs/parse-clj `[(t/* t/Int) (t/+ t/Bool) :-> t/Any]) (clj-opts))))
  (is (= `(t/IFn [t/Bool :* :-> t/Any]
                 [t/Bool :* t/Bool :-> t/Any])
         (prs/unparse-type (prs/parse-clj `[(t/* t/Bool) (t/? t/Bool) :-> t/Any]) (clj-opts))))
  (is (= (prs/parse-clj `(t/All [y# :..] [y# :.. y# :-> t/Any]))
         (prs/parse-clj `(t/All [y# :..] [(t/cat y# :.. y#) :-> t/Any]))))
  (is (= (r/PolyDots-body-unsafe* (prs/parse-clj `(t/All [y# :..] [y# :.. y# :-> t/Any])))
         (r/PolyDots-body-unsafe* (prs/parse-clj `(t/All [y# :..] [(t/cat y# :.. y#) :-> t/Any])))))
  #_;TODO
  (is (subtype? (r/PolyDots-body-unsafe* (prs/parse-clj `(t/All [y# :..] [y# :.. y#, t/Int :-> t/Any])))
                (r/PolyDots-body-unsafe* (prs/parse-clj `(t/All [y# :..] [(t/cat y# :.. y#) t/Int :-> t/Any])))))
  #_;TODO
  (is (subtype? (prs/parse-clj `(t/All [y# :..] [y# :.. y#, t/Int :-> t/Any]))
                (prs/parse-clj `(t/All [y# :..] [(t/cat y# :.. y#) t/Int :-> t/Any]))))
  (is (= (prs/parse-clj `[(t/cat t/Int :+ t/Bool :+) :-> t/Any])
         (prs/parse-clj `[t/Int :+ t/Bool :+ :-> t/Any])
         (prs/parse-clj `[t/Int t/Int :* t/Bool :+ :-> t/Any])
         (prs/parse-clj `[t/Int :+ t/Bool t/Bool :* :-> t/Any])
         (prs/parse-clj `[(t/+ t/Int) t/Bool :+ :-> t/Any])
         (prs/parse-clj `[(t/+ t/Int) (t/+ t/Bool) :-> t/Any])))
)

(deftest parse-Match-test
  (is-tc-e :load-checker)
  (is-clj (= '(t/Match nil
                       nil :-> nil)
             (prs/unparse-type
               (prs/parse-clj `(t/Match nil
                                        nil :-> nil))
               (unparse-in-opts))))
  (is-clj (= '(t/Match nil
                       [S] (t/Seqable S) :-> S)
             (prs/unparse-type
               (prs/parse-clj `(t/Match nil
                                        [~'S] (t/Seqable ~'S) :-> ~'S))
               (unparse-in-opts))))
  (is-clj (= '(t/Match (t/Seqable t/Int)
                       [T] (t/NilableNonEmptySeq T) :-> T
                       [S] (t/Seqable S) :-> S)
             (prs/unparse-type
               (prs/parse-clj `(t/Match (t/Seqable t/Int)
                                        [~'T] (t/NilableNonEmptySeq ~'T) :-> ~'T
                                        [~'S] (t/Seqable ~'S) :-> ~'S))
               (unparse-in-opts)))))

(deftest parse-Instance-test
  (is-clj (= '(t/Instance Object)
             (let [opts (assoc (unparse-in-opts) :typed.clj.checker.parse-unparse/parse-type-in-ns this-nsym)]
               (prs/unparse-type
                 (prs/parse-clj `(t/Instance Object) opts)
                 opts)))))

(t/ann-protocol [[x :variance :invariant]]
                InvariantProtocol)
(defprotocol InvariantProtocol)

(deftest parse-Satisfies-test
  (is-clj (= 'typed.cljc.checker.impl-protocols/TCType
             (prs/unparse-type
               (prs/parse-clj `(t/Satisfies typed.cljc.checker.impl-protocols/TCType))
               (unparse-in-opts))))
  (is-clj (= 'typed.cljc.checker.impl-protocols/TCType
             (prs/unparse-type
               (prs/parse-clj `(t/Satisfies ~'TCType)
                              (assoc (clj-opts) ::prs/parse-type-in-ns 'typed.cljc.checker.impl-protocols))
               (unparse-in-opts))))
  (is-clj (= '(t/Satisfies InvariantProtocol)
             (prs/unparse-type
               (prs/parse-clj `(t/Satisfies InvariantProtocol))
               (unparse-in-opts)))))
