(ns typed-test.cljc.checker.check.cache
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.clojure :as t]
            [typed.clj.runtime.env :as env-clj]
            [typed.cljc.checker.check.cache :as cache]
            [clojure.test :refer :all]))

(def expected-cache
 '{"(ns typed-test.cljc.checker.check.cache-test-ns1\n  (:require [typed.clojure :as t]))" {:typed.cljc.checker.check.cache/types {:clojure.core.typed.current-impl/unanalyzed-special {clojure.core/ns typed.clj.ext.clojure.core__ns/-unanalyzed-special__ns},
                                                                                                                                   :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure},
                                                                                            :typed.cljc.checker.check.cache/vars {ns clojure.core/ns},
                                                                                            :typed.cljc.checker.check.cache/errors false,
                                                                                            :typed.cljc.checker.check.cache/interop {},
                                                                                            :typed.cljc.checker.check.cache/type-syms {}},
   "(t/ann foo t/Int)" {:typed.cljc.checker.check.cache/types {:clojure.core.typed.current-impl/unanalyzed-special {clojure.core.typed/ann typed.clj.ext.clojure.core.typed__ann/defuspecial__ann},
                                                               :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure},
                        :typed.cljc.checker.check.cache/vars {t/ann typed.clojure/ann},
                        :typed.cljc.checker.check.cache/errors false,
                        :typed.cljc.checker.check.cache/interop {},
                        :typed.cljc.checker.check.cache/type-syms {}},
   "(def foo 1)" {:typed.cljc.checker.check.cache/types {:clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure,
                                                         :clojure.core.typed.current-impl/current-var-annotations {typed-test.cljc.checker.check.cache-test-ns1/foo typed.clojure/Int},
                                                         :clojure.core.typed.current-impl/current-name-env {typed.clojure/Int typed.clojure/AnyInteger,
                                                                                                            typed.clojure/AnyInteger (typed.clojure/U
                                                                                                                                      java.lang.Short
                                                                                                                                      java.lang.Byte
                                                                                                                                      java.math.BigInteger
                                                                                                                                      java.lang.Integer
                                                                                                                                      clojure.lang.BigInt
                                                                                                                                      java.lang.Long),
                                                                                                            java.lang.Short {},
                                                                                                            java.lang.Byte {},
                                                                                                            java.math.BigInteger {},
                                                                                                            java.lang.Integer {},
                                                                                                            clojure.lang.BigInt {},
                                                                                                            java.lang.Long {}},
                                                         :clojure.core.typed.current-impl/current-rclass-env {java.lang.Comparable (typed.clojure/TFn [[a :variance :invariant]] (java.lang.Comparable a))}},
                  :typed.cljc.checker.check.cache/vars {},
                  :typed.cljc.checker.check.cache/errors false,
                  :typed.cljc.checker.check.cache/interop {},
                  :typed.cljc.checker.check.cache/type-syms {typed-test.cljc.checker.check.cache-test-ns1 {t/Int typed.clojure/Int},
                                                             typed.ann.clojure {t/AnyInteger typed.clojure/AnyInteger,
                                                                                t/U typed.clojure/U,
                                                                                Integer java.lang.Integer,
                                                                                Long java.lang.Long,
                                                                                BigInteger java.math.BigInteger,
                                                                                Short java.lang.Short,
                                                                                Byte java.lang.Byte}}},
   "(t/ann bar [(t/Seqable t/Num) :-> (t/Seq t/Bool)])" {:typed.cljc.checker.check.cache/types {:clojure.core.typed.current-impl/unanalyzed-special {clojure.core.typed/ann typed.clj.ext.clojure.core.typed__ann/defuspecial__ann},
                                                                                                :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure},
                                                         :typed.cljc.checker.check.cache/vars {t/ann typed.clojure/ann},
                                                         :typed.cljc.checker.check.cache/errors false,
                                                         :typed.cljc.checker.check.cache/interop {},
                                                         :typed.cljc.checker.check.cache/type-syms {}},
   "(defn bar [n]\n  (map zero? n))" {:typed.cljc.checker.check.cache/types {:clojure.core.typed.current-impl/unanalyzed-special {clojure.core/defn typed.clj.ext.clojure.core__defn/defuspecial__defn,
                                                                                                                                  clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
                                                                             :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure,
                                                                             :clojure.core.typed.current-impl/current-var-annotations {typed-test.cljc.checker.check.cache-test-ns1/bar [(typed.clojure/Seqable
                                                                                                                                                                                          typed.clojure/Num)
                                                                                                                                                                                         :->
                                                                                                                                                                                         (typed.clojure/Seq
                                                                                                                                                                                          typed.clojure/Bool)],
                                                                                                                                       clojure.core/map (typed.clojure/All
                                                                                                                                                         [c a b :..]
                                                                                                                                                         (typed.clojure/IFn
                                                                                                                                                          [[a :-> c] :-> (typed.clojure/Transducer a c)]
                                                                                                                                                          [[a b :.. b :-> c]
                                                                                                                                                           (typed.clojure/NonEmptySeqable a)
                                                                                                                                                           (typed.clojure/NonEmptySeqable b)
                                                                                                                                                           :..
                                                                                                                                                           b
                                                                                                                                                           :->
                                                                                                                                                           (typed.clojure/NonEmptyASeq c)]
                                                                                                                                                          [[a b :.. b :-> c]
                                                                                                                                                           (typed.clojure/Seqable a)
                                                                                                                                                           (typed.clojure/Seqable b)
                                                                                                                                                           :..
                                                                                                                                                           b
                                                                                                                                                           :->
                                                                                                                                                           (typed.clojure/ASeq c)])),
                                                                                                                                       clojure.core/zero? [typed.clojure/Num
                                                                                                                                                           :->
                                                                                                                                                           typed.clojure/Bool
                                                                                                                                                           :filters
                                                                                                                                                           {:then (is (typed.clojure/Val 0) 0),
                                                                                                                                                            :else (! (typed.clojure/Val 0) 0)}]},
                                                                             :clojure.core.typed.current-impl/current-used-vars #{},
                                                                             :clojure.core.typed.current-impl/current-name-env {clojure.lang.ISeq {},
                                                                                                                                typed.clojure/Bool java.lang.Boolean,
                                                                                                                                typed.clojure/Seqable (typed.clojure/TFn
                                                                                                                                                       [[x :variance :covariant]]
                                                                                                                                                       (typed.clojure/Nilable
                                                                                                                                                        (clojure.lang.Seqable
                                                                                                                                                         (typed.clojure/NilableNonEmptySeq x)))),
                                                                                                                                java.util.Collection {},
                                                                                                                                typed.clojure/Num java.lang.Number,
                                                                                                                                java.lang.Boolean {},
                                                                                                                                typed.clojure/Option (typed.clojure/TFn
                                                                                                                                                      [[x :variance :covariant]]
                                                                                                                                                      (typed.clojure/U nil x)),
                                                                                                                                clojure.lang.IPersistentCollection {},
                                                                                                                                clojure.lang.Fn {},
                                                                                                                                typed.clojure/Fn clojure.lang.Fn,
                                                                                                                                typed.clojure/Nilable typed.clojure/Option,
                                                                                                                                typed.clojure/NonEmptyASeq (typed.clojure/TFn
                                                                                                                                                            [[x :variance :covariant]]
                                                                                                                                                            (typed.clojure/I
                                                                                                                                                             clojure.lang.Sequential
                                                                                                                                                             clojure.lang.IObj
                                                                                                                                                             (clojure.lang.ISeq x)
                                                                                                                                                             (java.util.List x)
                                                                                                                                                             (typed.clojure/CountRange 1))),
                                                                                                                                typed.clojure/Seq (typed.clojure/TFn
                                                                                                                                                   [[x :variance :covariant]]
                                                                                                                                                   (clojure.lang.ISeq x)),
                                                                                                                                typed.clojure/NonEmptySeqable (typed.clojure/TFn
                                                                                                                                                               [[x :variance :covariant]]
                                                                                                                                                               (typed.clojure/I
                                                                                                                                                                (clojure.lang.Seqable
                                                                                                                                                                 (typed.clojure/NilableNonEmptySeq x))
                                                                                                                                                                (typed.clojure/CountRange 1))),
                                                                                                                                java.util.SequencedCollection {},
                                                                                                                                clojure.lang.Seqable {},
                                                                                                                                java.lang.Number {},
                                                                                                                                typed.clojure/NilableNonEmptySeq (typed.clojure/TFn
                                                                                                                                                                  [[x :variance :covariant]]
                                                                                                                                                                  (typed.clojure/Nilable
                                                                                                                                                                   (typed.clojure/NonEmptySeq x))),
                                                                                                                                typed.clojure/ASeq (typed.clojure/TFn
                                                                                                                                                    [[x :variance :covariant]]
                                                                                                                                                    (typed.clojure/I
                                                                                                                                                     (clojure.lang.ISeq x)
                                                                                                                                                     (java.util.List x)
                                                                                                                                                     clojure.lang.Sequential
                                                                                                                                                     clojure.lang.IObj)),
                                                                                                                                java.lang.Iterable {}},
                                                                             :clojure.core.typed.current-impl/current-rclass-env {clojure.lang.Seqable (typed.clojure/TFn
                                                                                                                                                        [[a
                                                                                                                                                          :variance
                                                                                                                                                          :covariant
                                                                                                                                                          :<
                                                                                                                                                          (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
                                                                                                                                                        (clojure.lang.Seqable a)),
                                                                                                                                  java.lang.Comparable (typed.clojure/TFn
                                                                                                                                                        [[a :variance :invariant]]
                                                                                                                                                        (java.lang.Comparable a)),
                                                                                                                                  java.util.SequencedCollection (typed.clojure/TFn
                                                                                                                                                                 [[a :variance :invariant]]
                                                                                                                                                                 (java.util.SequencedCollection a)),
                                                                                                                                  java.util.Collection (typed.clojure/TFn
                                                                                                                                                        [[a :variance :covariant]]
                                                                                                                                                        (java.util.Collection a)),
                                                                                                                                  java.lang.Iterable (typed.clojure/TFn
                                                                                                                                                      [[a :variance :covariant]]
                                                                                                                                                      (java.lang.Iterable a)),
                                                                                                                                  clojure.lang.IPersistentCollection (typed.clojure/TFn
                                                                                                                                                                      [[a :variance :covariant]]
                                                                                                                                                                      (clojure.lang.IPersistentCollection a)),
                                                                                                                                  clojure.lang.ISeq (typed.clojure/TFn
                                                                                                                                                     [[a :variance :covariant]]
                                                                                                                                                     (clojure.lang.ISeq a))}},
                                      :typed.cljc.checker.check.cache/vars {defn clojure.core/defn, map clojure.core/map, zero? clojure.core/zero?},
                                      :typed.cljc.checker.check.cache/errors false,
                                      :typed.cljc.checker.check.cache/interop {},
                                      :typed.cljc.checker.check.cache/type-syms {typed-test.cljc.checker.check.cache-test-ns1 {t/Seqable typed.clojure/Seqable,
                                                                                                                               t/Num typed.clojure/Num,
                                                                                                                               t/Seq typed.clojure/Seq,
                                                                                                                               t/Bool typed.clojure/Bool},
                                                                                 typed.ann.clojure {t/All typed.clojure/All,
                                                                                                    t/Seq typed.clojure/Seq,
                                                                                                    t/NonEmptyASeq typed.clojure/NonEmptyASeq,
                                                                                                    t/NonEmptySeq typed.clojure/NonEmptySeq,
                                                                                                    t/NonEmptyCount typed.clojure/NonEmptyCount,
                                                                                                    t/NonEmptySeqable typed.clojure/NonEmptySeqable,
                                                                                                    t/Nilable typed.clojure/Nilable,
                                                                                                    t/Seqable typed.clojure/Seqable,
                                                                                                    t/Sequential typed.clojure/Sequential,
                                                                                                    t/SequentialSeq typed.clojure/SequentialSeq,
                                                                                                    Iterable java.lang.Iterable,
                                                                                                    t/Transducer typed.clojure/Transducer,
                                                                                                    t/ASeq typed.clojure/ASeq,
                                                                                                    t/IFn typed.clojure/IFn,
                                                                                                    t/Option typed.clojure/Option,
                                                                                                    t/CountRange typed.clojure/CountRange,
                                                                                                    t/NilableNonEmptySeq typed.clojure/NilableNonEmptySeq,
                                                                                                    Number java.lang.Number,
                                                                                                    t/U typed.clojure/U,
                                                                                                    t/TFn typed.clojure/TFn,
                                                                                                    t/I typed.clojure/I},
                                                                                 typed.ann.clojure.jvm {t/NilableNonEmptySeq typed.clojure/NilableNonEmptySeq,
                                                                                                        t/Any typed.clojure/Any,
                                                                                                        IPersistentCollection clojure.lang.IPersistentCollection,
                                                                                                        Seqable clojure.lang.Seqable,
                                                                                                        Iterable java.lang.Iterable,
                                                                                                        Collection java.util.Collection}}}})

(def expected-ns-cache
 '{:typed.cljc.checker.check.cache/types {:clojure.core.typed.current-impl/unanalyzed-special {clojure.core/defn typed.clj.ext.clojure.core__defn/defuspecial__defn,
                                                                                               clojure.core/fn typed.clj.ext.clojure.core__fn/defuspecial__fn},
                                          :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure,
                                          :clojure.core.typed.current-impl/current-var-annotations {typed-test.cljc.checker.check.cache-test-ns1/bar [(typed.clojure/Seqable typed.clojure/Num)
                                                                                                                                                      :->
                                                                                                                                                      (typed.clojure/Seq typed.clojure/Bool)],
                                                                                                    clojure.core/map (typed.clojure/All
                                                                                                                      [c a b :..]
                                                                                                                      (typed.clojure/IFn
                                                                                                                       [[a :-> c] :-> (typed.clojure/Transducer a c)]
                                                                                                                       [[a b :.. b :-> c]
                                                                                                                        (typed.clojure/NonEmptySeqable a)
                                                                                                                        (typed.clojure/NonEmptySeqable b)
                                                                                                                        :..
                                                                                                                        b
                                                                                                                        :->
                                                                                                                        (typed.clojure/NonEmptyASeq c)]
                                                                                                                       [[a b :.. b :-> c]
                                                                                                                        (typed.clojure/Seqable a)
                                                                                                                        (typed.clojure/Seqable b)
                                                                                                                        :..
                                                                                                                        b
                                                                                                                        :->
                                                                                                                        (typed.clojure/ASeq c)])),
                                                                                                    clojure.core/zero? [typed.clojure/Num
                                                                                                                        :->
                                                                                                                        typed.clojure/Bool
                                                                                                                        :filters
                                                                                                                        {:then (is (typed.clojure/Val 0) 0), :else (! (typed.clojure/Val 0) 0)}]},
                                          :clojure.core.typed.current-impl/current-name-env {clojure.lang.ISeq {},
                                                                                             typed.clojure/Bool java.lang.Boolean,
                                                                                             typed.clojure/Seqable (typed.clojure/TFn
                                                                                                                    [[x :variance :covariant]]
                                                                                                                    (typed.clojure/Nilable (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x)))),
                                                                                             java.util.Collection {},
                                                                                             typed.clojure/Num java.lang.Number,
                                                                                             java.lang.Boolean {},
                                                                                             typed.clojure/Option (typed.clojure/TFn [[x :variance :covariant]] (typed.clojure/U nil x)),
                                                                                             clojure.lang.IPersistentCollection {},
                                                                                             clojure.lang.Fn {},
                                                                                             typed.clojure/Fn clojure.lang.Fn,
                                                                                             typed.clojure/Nilable typed.clojure/Option,
                                                                                             typed.clojure/NonEmptyASeq (typed.clojure/TFn
                                                                                                                         [[x :variance :covariant]]
                                                                                                                         (typed.clojure/I
                                                                                                                          clojure.lang.Sequential
                                                                                                                          clojure.lang.IObj
                                                                                                                          (clojure.lang.ISeq x)
                                                                                                                          (java.util.List x)
                                                                                                                          (typed.clojure/CountRange 1))),
                                                                                             typed.clojure/Seq (typed.clojure/TFn [[x :variance :covariant]] (clojure.lang.ISeq x)),
                                                                                             typed.clojure/NonEmptySeqable (typed.clojure/TFn
                                                                                                                            [[x :variance :covariant]]
                                                                                                                            (typed.clojure/I
                                                                                                                             (clojure.lang.Seqable (typed.clojure/NilableNonEmptySeq x))
                                                                                                                             (typed.clojure/CountRange 1))),
                                                                                             java.util.SequencedCollection {},
                                                                                             clojure.lang.Seqable {},
                                                                                             java.lang.Number {},
                                                                                             typed.clojure/NilableNonEmptySeq (typed.clojure/TFn
                                                                                                                               [[x :variance :covariant]]
                                                                                                                               (typed.clojure/Nilable (typed.clojure/NonEmptySeq x))),
                                                                                             typed.clojure/ASeq (typed.clojure/TFn
                                                                                                                 [[x :variance :covariant]]
                                                                                                                 (typed.clojure/I
                                                                                                                  (clojure.lang.ISeq x)
                                                                                                                  (java.util.List x)
                                                                                                                  clojure.lang.Sequential
                                                                                                                  clojure.lang.IObj)),
                                                                                             java.lang.Iterable {}},
                                          :clojure.core.typed.current-impl/current-rclass-env {clojure.lang.Seqable (typed.clojure/TFn
                                                                                                                     [[a :variance :covariant :< (typed.clojure/NilableNonEmptySeq typed.clojure/Any)]]
                                                                                                                     (clojure.lang.Seqable a)),
                                                                                               java.lang.Comparable (typed.clojure/TFn [[a :variance :invariant]] (java.lang.Comparable a)),
                                                                                               java.util.SequencedCollection (typed.clojure/TFn [[a :variance :invariant]] (java.util.SequencedCollection a)),
                                                                                               java.util.Collection (typed.clojure/TFn [[a :variance :covariant]] (java.util.Collection a)),
                                                                                               java.lang.Iterable (typed.clojure/TFn [[a :variance :covariant]] (java.lang.Iterable a)),
                                                                                               clojure.lang.IPersistentCollection (typed.clojure/TFn
                                                                                                                                   [[a :variance :covariant]]
                                                                                                                                   (clojure.lang.IPersistentCollection a)),
                                                                                               clojure.lang.ISeq (typed.clojure/TFn [[a :variance :covariant]] (clojure.lang.ISeq a))},
                                          :clojure.core.typed.current-impl/current-used-vars #{}},
   :typed.cljc.checker.check.cache/vars {ns clojure.core/ns, t/ann typed.clojure/ann, defn clojure.core/defn, map clojure.core/map, zero? clojure.core/zero?},
   :typed.cljc.checker.check.cache/interop {},
   :typed.cljc.checker.check.cache/type-syms {typed-test.cljc.checker.check.cache-test-ns1 {t/Seqable typed.clojure/Seqable, t/Num typed.clojure/Num, t/Seq typed.clojure/Seq, t/Bool typed.clojure/Bool},
                                              typed.ann.clojure {t/All typed.clojure/All,
                                                                 t/Seq typed.clojure/Seq,
                                                                 t/NonEmptyASeq typed.clojure/NonEmptyASeq,
                                                                 t/NonEmptySeq typed.clojure/NonEmptySeq,
                                                                 t/NonEmptyCount typed.clojure/NonEmptyCount,
                                                                 t/NonEmptySeqable typed.clojure/NonEmptySeqable,
                                                                 t/Nilable typed.clojure/Nilable,
                                                                 t/Seqable typed.clojure/Seqable,
                                                                 t/Sequential typed.clojure/Sequential,
                                                                 t/SequentialSeq typed.clojure/SequentialSeq,
                                                                 Iterable java.lang.Iterable,
                                                                 t/Transducer typed.clojure/Transducer,
                                                                 t/ASeq typed.clojure/ASeq,
                                                                 t/IFn typed.clojure/IFn,
                                                                 t/Option typed.clojure/Option,
                                                                 t/CountRange typed.clojure/CountRange,
                                                                 t/NilableNonEmptySeq typed.clojure/NilableNonEmptySeq,
                                                                 Number java.lang.Number,
                                                                 t/U typed.clojure/U,
                                                                 t/TFn typed.clojure/TFn,
                                                                 t/I typed.clojure/I},
                                              typed.ann.clojure.jvm {t/NilableNonEmptySeq typed.clojure/NilableNonEmptySeq,
                                                                     t/Any typed.clojure/Any,
                                                                     IPersistentCollection clojure.lang.IPersistentCollection,
                                                                     Seqable clojure.lang.Seqable,
                                                                     Iterable java.lang.Iterable,
                                                                     Collection java.util.Collection}},
   :slurped "(ns typed-test.cljc.checker.check.cache-test-ns1\n  (:require [typed.clojure :as t]))\n\n(t/ann foo t/Int)\n(def foo 1)\n\n(t/ann bar [(t/Seqable t/Num) :-> (t/Seq t/Bool)])\n(defn bar [n]\n  (map zero? n))\n"})


#_ ;;FIXME non-deterministic I/U printing order
(deftest test-ns1
  (is (t/check-ns-clj 'typed-test.cljc.checker.check.cache-test-ns1))
  (is (= expected-cache
         (get-in @env-clj/clj-checker-atom [::cache/check-form-cache 'typed-test.cljc.checker.check.cache-test-ns1
                                            "(ns typed-test.cljc.checker.check.cache-test-ns1\n  (:require [typed.clojure :as t]))"])))
  (is (= expected-ns-cache
         (get-in @env-clj/clj-checker-atom [::cache/check-ns-cache 'typed-test.cljc.checker.check.cache-test-ns1]))))
