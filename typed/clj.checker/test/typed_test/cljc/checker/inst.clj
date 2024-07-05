(ns typed-test.cljc.checker.inst
  (:require 
    ; this loads the type system, must go first
    [typed.clj.checker.test-utils :refer :all]
    [typed.clojure :as t]
    [clojure.test :refer [deftest is testing]]
    [typed.clj.checker.parse-unparse :as prs]
    [typed.cljc.checker.inst :as inst]))

(deftest trans-dots-test
  (is-tc-e 1)
  (is (= (inst/manual-inst (prs/parse-clj `(t/All [x# b# :..] [x# :.. b# :-> x#]))
                           (mapv (comp prs/parse-clj prs/allow-regex)
                                 `(Integer (t/cat Double Float)))
                           {}
                           (clj-opts))
         (prs/parse-clj `[Integer Integer :-> Integer])))
  (is (= (inst/manual-inst (prs/parse-clj `(t/All [x# b# :..] [b# :.. b# :-> x#]))
                           (mapv (comp prs/parse-clj prs/allow-regex) `(Integer (t/cat Double Float)))
                           {}
                           (clj-opts))
         (prs/parse-clj `[Double Float :-> Integer])))
  ;map type
  (is (= (inst/manual-inst (prs/parse-clj `(t/All [c# a# b# :..]
                                              [[a# b# :.. b# :-> c#] (t/Seqable a#) (t/Seqable b#) :.. b# :-> (t/Seqable c#)]))
                           (mapv (comp prs/parse-clj prs/allow-regex) `(Integer Double (t/cat Float)))
                           {}
                           (clj-opts))
         (prs/parse-clj `[[Double Float :-> Integer] (t/Seqable Double) (t/Seqable Float) :-> (t/Seqable Integer)])))
  (is (= (inst/manual-inst (prs/parse-clj `(t/All [x# b# :..]
                                              ['[x# b#] :.. b# :-> '['[x# b#] :.. b#]]))
                           (mapv (comp prs/parse-clj prs/allow-regex) `(Integer (t/cat Double Float)))
                           {}
                           (clj-opts))
         (prs/parse-clj `['[Integer Double] '[Integer Float] 
                          :-> '['[Integer Double] '[Integer Float]]])))
  ;TODO t/HSequential
  (is (= (inst/manual-inst (prs/parse-clj `(t/All [x# b# :..]
                                              [x# :.. b# :-> (t/HSequential [x# :.. b#])]))
                           (mapv (comp prs/parse-clj prs/allow-regex) `(Integer (t/cat Double Float)))
                           {}
                           (clj-opts))
         (prs/parse-clj `(t/IFn [Integer Integer :-> (t/HSequential [Integer Integer])]))))
  ; completeness check
  (is (t/check-ns-clj 'clojure.core.typed.test.trans-dots)))

(deftest poly-inst-scoping-test
  (is-tc-e (fn [a] (inst identity foo))
           (t/All [foo] [t/Any -> t/Any]))
  (is-tc-e
    (fn [f coll]
      (fn
        [x :- a
         y :- (t/Seqable b)]))
    (t/All [a b] [t/Any t/Any -> t/Any]))
  (is-tc-err
    (fn :forall [x y]
      [f coll]
      (fn
        [x :- a
         y :- (t/Seqable b)]))
    (t/All [a b] [t/Any t/Any -> t/Any]))
  (is-tc-e
    (fn :forall [x y]
      [f coll]
      (fn
        [x :- x
         y :- (t/Seqable y)]))
    (t/All [a b] [t/Any t/Any -> t/Any]))
  ;; backwards compat with trailing single dotted var
  (is-tc-err (fn [a] (inst a))
             [(t/All [x b :..] [b :.. b -> t/Any]) -> t/Any])
  (is-tc-e (fn [a] (inst a t/Int t/Int))
           [(t/All [b :..] [b :.. b -> t/Any]) -> t/Any])
  ;; preferred syntax
  (is-tc-e (fn [a] (inst a (t/cat)))
           [(t/All [b :..] [b :.. b -> t/Any]) -> t/Any])
  (is-tc-e (fn [a] (inst a t/Any (t/cat)))
           [(t/All [x b :..] [b :.. b -> t/Any]) -> t/Any])
  (is-tc-e (fn [a] (inst a (t/cat t/Int t/Int)))
           [(t/All [b :..] [b :.. b -> t/Any]) -> t/Any])
)

(deftest poly-named-test
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
                      (t/All [a :..] [a :.. a :-> t/Any]))
               (cc/defn foo [& args])
               (t/inst foo t/Str t/Bool))
           [t/Str t/Bool :-> t/Any])
  (is-tc-e (do (t/ann ^:no-check foo 
                      (t/All [a :.. :named [b c]]
                             [c b a :.. a :-> b]))
               (cc/defn foo [& args] (second args))
               (t/inst foo t/Str t/Bool :named {c t/Num b t/Sym}))
           [t/Num t/Sym t/Str t/Bool :-> t/Sym]))
