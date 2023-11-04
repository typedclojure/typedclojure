(ns typed-test.cljc.checker.cs-gen
  (:require [clojure.data :as data]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.cs-gen :as sut]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.clj.checker.parse-unparse :refer [parse-type]]
            [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]))

(deftest solve-test
  (is-tc-e 1) ;;load type system
  (is-clj (nil? (sut/solve (r/ret (parse-type `t/Num))
                           (parse-type `(t/All [x#] [t/Int :-> t/Int])))))
  (is-clj (nil? (sut/solve (r/ret (parse-type `t/Num))
                           (parse-type `(t/All [x#] [t/Int :-> t/Int])))))
  (is-clj (both-subtype?
            (:t (sut/solve (r/ret (parse-type `[t/Int :-> t/Int]))
                           (parse-type `(t/All [x#] [[t/Int :-> x#] :-> [t/Int :-> x#]]))))
            (parse-type `[t/Int :-> t/Int])))
  (is-clj (both-subtype?
            (:t (sut/solve (r/ret (parse-type `[t/Int :-> t/Int]))
                           (parse-type `(t/All [x#] [[(t/Val 1) :-> x#] :-> [(t/Val 1) :-> x#]]))))
            (parse-type `[(t/Val 1) :-> t/Int])))
  (is-clj (both-subtype?
            (:t (sut/solve (r/ret (parse-type `[t/Int :-> [t/Bool :-> t/Int]]))
                           (parse-type `(t/All [x#] [[t/Int :-> [x# :-> t/Int]] :-> [t/Int :-> [x# :-> t/Int]]]))))
            (parse-type `[t/Int :-> [t/Bool :-> t/Int]])))
  (is-clj (both-subtype?
            (:t (sut/solve (r/ret (parse-type `[t/Int :-> [t/Int :-> t/Bool]]))
                           (parse-type `(t/All [x#] [[t/Int :-> [t/Int :-> x#]] :-> [t/Int :-> [t/Int :-> x#]]]))))
            (parse-type `[t/Int :-> [t/Int :-> t/Bool]])))
  (is-clj (both-subtype?
            (:t (sut/solve (r/ret (parse-type `[t/Bool :-> [t/Int :-> t/Int]]))
                           (parse-type `(t/All [x#] [[x# :-> [t/Int :-> t/Int]] :-> [x# :-> [t/Int :-> t/Int]]]))))
            (parse-type `[t/Bool :-> [t/Int :-> t/Int]])))
  )

(deftest wild->tv-test
  (is-tc-e true) ;load type system
  (clj (let [{:keys [t tvs]} (sut/wild->tv (parse-type `t/Int))]
         (is (= (parse-type `t/Int) t))
         (is (empty? tvs))))
  (clj (let [{:keys [t tvs]} (sut/wild->tv r/-wild)]
         (is (r/F? t))
         (is (= 1 (count tvs)))))
  (clj (let [{:keys [t tvs]} (sut/wild->tv (parse-type `[t/Int :-> [t/Infer :-> t/Int]]))]
         (when (is (= 1 (count tvs)))
           (is (= (parse-type `[t/Int :-> [t/Bool :-> t/Int]])
                  (subst/subst-all
                    {(first tvs) (crep/t-subst-maker (parse-type `t/Bool) r/no-bounds)}
                    t)))
           (is (= (parse-type `[t/Int :-> [t/Num :-> t/Int]])
                  (subst/subst-all
                    {(first tvs) (crep/t-subst-maker (parse-type `t/Num) r/no-bounds)}
                    t)))))))

(deftest eliminate-wild-test
  (is-tc-e true) ;load type system
  (clj (let [t (parse-type `(t/All [x#] [x# :-> x#]))]
         (is (identical? t (sut/eliminate-wild (parse-type `(t/All [x#] [x# :-> x#])) t)))))
  (is-clj (both-subtype? (parse-type `t/Bool)
                         (sut/eliminate-wild (parse-type `t/Bool) r/-wild)))
  (is-clj (both-subtype? (parse-type `t/Int)
                         (sut/eliminate-wild (parse-type `t/Int) r/-wild)))
  (is-clj (not (both-subtype? (parse-type `t/Bool)
                              (sut/eliminate-wild (parse-type `t/Int) r/-wild))))
  (is-clj (both-subtype? (parse-type `t/Num)
                         (sut/eliminate-wild (parse-type `t/Int)
                                             (parse-type `t/Num))))
  (is-clj (nil? (sut/eliminate-wild (parse-type `t/Num)
                                    (parse-type `t/Int))))
  (is-clj (nil? (sut/eliminate-wild (parse-type `[t/Bool :-> t/Int])
                                    (parse-type `[t/Infer :-> t/Num]))))
  (is-clj (nil? (sut/eliminate-wild (parse-type `[t/Bool :-> t/Num])
                                    (parse-type `[t/Infer :-> t/Int]))))
  (is-clj (nil? (sut/eliminate-wild (parse-type `[t/Bool :-> t/Int])
                                    (parse-type `[t/Infer :-> t/Infer]))))
  (is-clj (both-subtype? (parse-type `(t/Transducer t/Bool t/Int))
                         (sut/eliminate-wild (parse-type `(t/Transducer t/Bool t/Int))
                                             (parse-type `(t/Transducer t/Bool t/Infer)))))
  (is-clj (nil? (sut/eliminate-wild (parse-type `(t/Transducer t/Bool t/Int))
                                    (parse-type `(t/Transducer t/Infer t/Int)))))
  (is-clj (nil? (sut/eliminate-wild (parse-type `(t/Transducer t/Bool t/Int))
                                    (parse-type `(t/Transducer t/Infer t/Infer))))))

(deftest prep-symbolic-closure-expected2-type-test
  (is-tc-e true) ;load type system
  (clj (let [res (sut/prep-symbolic-closure-expected-type2
                   {'x (crep/t-subst-maker (r/-val 1) r/no-bounds)}
                   (r/make-FnIntersection
                     (r/make-Function [(r/make-F 'x)] (r/make-F 'x))))]
         (is (= (r/make-FnIntersection (r/make-Function [(r/-val 1)] r/-wild))
                res))))
  (clj (let [res (sut/prep-symbolic-closure-expected-type2
                   {'x (crep/t-subst-maker (r/-val 1) r/no-bounds)}
                   (c/Poly* ['y] [r/no-bounds]
                            (r/make-FnIntersection (r/make-Function [(r/make-F 'x) (r/F-maker 'y)] (r/make-F 'x)))))]
         (is (= (c/Poly* ['y] [r/no-bounds]
                         (r/make-FnIntersection (r/make-Function [(r/-val 1) (r/F-maker 'y)] r/-wild)))
                res))))
  (clj (let [res (sut/prep-symbolic-closure-expected-type2
                   {'x (crep/t-subst-maker (r/-val 1) r/no-bounds)}
                   (c/PolyDots* ['y 'z] [r/no-bounds r/dotted-no-bounds]
                                (r/make-FnIntersection (r/make-Function [(r/make-F 'x) (r/F-maker 'y)] (r/make-F 'x)
                                                                        :drest (r/DottedPretype1-maker (r/F-maker 'z) 'z)))))]
         (is (= (c/PolyDots* ['y 'z] [r/no-bounds r/dotted-no-bounds]
                             (r/make-FnIntersection (r/make-Function [(r/-val 1) (r/F-maker 'y)] r/-wild
                                                                     :drest (r/DottedPretype1-maker (r/F-maker 'z) 'z))))
                res))))
  (clj (let [subst {'a (crep/t-subst-maker r/-nothing r/no-bounds)
                    'c (crep/t-subst-maker r/-nothing r/no-bounds)}
             t (r/make-FnIntersection (r/make-Function [(r/make-F 'a)] (r/make-F 'c)))
             res (sut/prep-symbolic-closure-expected-type2 subst t)]
         (is (= (sut/prep-symbolic-closure-expected-type subst t)
                res))
         (is (= (r/make-FnIntersection (r/make-Function [r/-nothing] r/-wild))
                res))))
  #_ ;;TODO
  (clj (let [subst {'x (crep/t-subst-maker r/-nothing r/no-bounds)
                    'y (crep/t-subst-maker (r/-val 1) r/no-bounds)}
             t (r/TApp-maker (r/Name-maker `t/Transducer) [(r/make-F 'y) (r/make-F 'x)])
             res (sut/prep-symbolic-closure-expected-type2 subst t)
             expected-t (r/TApp-maker (r/Name-maker `t/Transducer) [(r/-val 1) r/-wild])]
         (is (both-subtype? (sut/prep-symbolic-closure-expected-type subst t)
                            expected-t))
         (is (both-subtype? (c/fully-resolve-type expected-t) res))
         #_;;TODO
         (is (= (r/make-FnIntersection (r/make-Function [r/-nothing] r/-wild))
                res))))

;  "substitution-without-symb" {x66185 typed.clojure/Nothing y66186 (typed.clojure/Val 1) a66187 typed.clojure/Nothing}
;"dom-t" (typed.clojure/Transducer y66186 x66185)
;"rng-var-hash" {y66186 :contravariant, r66350 :invariant} (typed.clojure/Reducer y66186 r66350)
;"variance" x66185 nil
;"variance" y66186 :contravariant
;"variance" a66187 nil
;"prep-symbolic-closure-expected-type" (typed.clojure/Transducer (typed.clojure/Val 1) typed.clojure/Any)

)

(deftest separate-F-test
  (is-tc-e true) ;load type system
  (clj (let [t (r/make-Function [(r/make-F 'x)] (r/make-F 'x))
             {t' :separated-t :keys [remap]} (sut/separate-F t {:fv #{'x}})
             xfvs (get-in remap [:fv 'x])]
         (is (= {:fv {'x xfvs}} remap) remap)
         (is (vector? xfvs))
         (is (= 2 (count xfvs)))
         (let [[x1 x2] xfvs
               expected-t (r/make-Function [(r/make-F x1)] (r/make-F x2))]
           (is (= expected-t t')))))
  (binding [vs/*verbose-types* true]
    (clj (let [t (r/make-Function [(r/make-F 'x) (r/make-F 'y)] (r/make-F 'x)
                                  :drest (r/DottedPretype1-maker (r/make-FnIntersection
                                                                   (r/make-Function [(r/make-F 'x) (r/make-F 'y)] (r/make-F 'y)
                                                                                    :drest 
                                                                                    (r/DottedPretype1-maker (r/make-FnIntersection (r/make-Function [(r/make-F 'x)] (r/make-F 'y)))
                                                                                                            'z))
                                                                   (r/make-Function [(r/make-F 'x) (r/make-F 'y)] (r/make-F 'y)
                                                                                    :drest 
                                                                                    (r/DottedPretype1-maker (r/make-FnIntersection (r/make-Function [(r/make-F 'x)] (r/make-F 'y)))
                                                                                                            'z)))
                                                                 'y))
               {t' :separated-t :keys [remap]} (sut/separate-F t {:fv #{'x}
                                                                  :idx #{'y 'z}})
               ;_ (prn t)
               ;_ (prn t')
               ;_ (clojure.pprint/pprint remap)
               {{[x1 x2] 'x} :fv
                {[y1] 'y [z1 z2] 'z} :idx} remap
               {{{[y1_x1 y1_x2] 'x
                  [y1_y1 y1_y2 y1_y3 y1_y4] 'y} [y1]
                 {[z1_x1] 'x
                  [z1_y1] 'y} [y1 z1]
                 {[z2_x1] 'x
                  [z2_y1] 'y} [y1 z2]} :idx-context} remap]
           (is (= {:fv {'x [x1 x2]}
                   :idx {'y [y1]
                         'z [z1 z2]}
                   :idx-context {[y1] {'x [y1_x1 y1_x2]
                                       'y [y1_y1 y1_y2 y1_y3 y1_y4]}
                                 [y1 z1] {'x [z1_x1]
                                          'y [z1_y1]}
                                 [y1 z2] {'x [z2_x1]
                                          'y [z2_y1]}}}
                  remap))
           (let [expected-t (r/make-Function [(r/make-F x1) (r/make-F 'y)] (r/make-F x2)
                                             :drest (r/DottedPretype1-maker (r/make-FnIntersection
                                                                              (r/make-Function [(r/make-F y1_x1) (r/make-F y1_y1)] (r/make-F y1_y2)
                                                                                               :drest 
                                                                                               (r/DottedPretype1-maker (r/make-FnIntersection (r/make-Function [(r/make-F z1_x1)] 
                                                                                                                                                               (r/make-F z1_y1)))
                                                                                                                       z1))
                                                                              (r/make-Function [(r/make-F y1_x2) (r/make-F y1_y3)] (r/make-F y1_y4)
                                                                                               :drest 
                                                                                               (r/DottedPretype1-maker (r/make-FnIntersection (r/make-Function [(r/make-F z2_x1)]
                                                                                                                                                               (r/make-F z2_y1)))
                                                                                                                       z2)))
                                                                            y1))]
             (is (= expected-t t'))))))
  (clj (let [{:keys [separated-t remap]} (sut/separate-F
                                           (r/make-Function [] r/-any :drest (r/DottedPretype1-maker (r/-hvec [(r/make-F 'z) (r/make-F 'z)]) 'z))
                                           {:idx #{'z}})
             {{[z] 'z} :idx} remap
             {{{[z1 z2] 'z} [z]} :idx-context} remap
             expected-t (r/make-Function [] r/-any :drest (r/DottedPretype1-maker (r/-hvec [(r/make-F z1) (r/make-F z2)]) z))]
         (is (= expected-t separated-t)))))

(deftest subst-non-covariant-test
  (clj (let [d (gensym 'd)
             a (gensym 'a)
             r (gensym 'r)
             t (r/make-Function [(r/make-F a)] (r/make-F r) :drest (r/DottedPretype1-maker (r/make-F d) d))
             subst {a (crep/t-subst-maker (r/-val 0) r/no-bounds)
                    d (crep/i-subst-maker [(r/-val 1) (r/-val 2)])}]
         (is (= (r/make-Function [(r/-val 0) (r/-val 1) (r/-val 2)]
                                 (r/make-F r))
                (sut/subst-non-covariant subst t)))))
  (clj (let [d (gensym 'd)
             a (gensym 'a)
             r (gensym 'r)
             t (r/make-Function [(r/make-F a)] (r/make-F r) :drest (r/DottedPretype1-maker (r/-hvec [(r/make-F d) (r/make-F d)]) d))
             subst {a (crep/t-subst-maker (r/-val 0) r/no-bounds)
                    d (crep/i-subst-maker [(r/-val 1) (r/-val 2)])}]
         (is (= (r/make-Function [(r/-val 0)
                                  (r/-hvec [(r/-val 1) (r/-val 1)])
                                  (r/-hvec [(r/-val 2) (r/-val 2)])]
                                 (r/make-F r))
                (sut/subst-non-covariant subst t))))))

(deftest infer-GetType-test
  (is-tc-e (fn [get' :- (t/All [m k] [m k :-> (t/Get m k)])]
             (let [res (get' {:a 1} :a)]
               (t/ann-form res '1))))
  (is-tc-err (fn [get' :- (t/All [m k] [m k :-> (t/Get m k)])]
               (let [res (get' {:a 1} :a)]
                 (t/ann-form res '2))))
  (is-tc-e (fn [get' :- (t/All [m k] [m k :-> (t/Get m k)])]
             (t/ann-form (get' {:a 1} :a) '1)))
  (is-tc-err (fn [get' :- (t/All [m k] [m k :-> (t/Get m k)])]
               (t/ann-form (get' {:a 1} :a) '2)))
  (is-tc-e (fn [get' :- (t/All [m k] [m k :-> '[(t/Get m k)]])]
             (let [res (get' {:a 1} :a)]
               (t/ann-form res '['1]))))
  (is-tc-e (fn [get' :- (t/All [m k] [m k :-> '[(t/Get m k)]])]
             (t/ann-form (get' {:a 1} :a) '['1])))
  (is-tc-err (fn [get' :- (t/All [m k] [m k :-> '[(t/Get m k)]])]
               (t/ann-form (get' {:a 1} :a) '['2])))

  (is-tc-e (fn [update' :- (t/All [m k v] [m k [(t/Get m k) :-> v] :-> v])
                f :- ['1 :-> '1]]
             (update' {:a 1} :a f)))
  (is-tc-e (fn [update' :- (t/All [m k v] [m k [(t/Get m k) :-> v] :-> v])
                f :- ['1 :-> '1]]
             (t/ann-form (update' {:a 1} :a f) '1)))
  (is-tc-err (fn [update' :- (t/All [m k v] [m k [(t/Get m k) :-> v] :-> v])
                  f :- ['2 :-> '2]]
               (t/ann-form (update' {:a 1} :a f) '1)))
  (is-tc-err (fn [update' :- (t/All [m k v] [m k [(t/Get m k) :-> v] :-> v])
                  f :- ['2 :-> '1]]
               (t/ann-form (update' {:a 1} :a f) '1)))
  (is-tc-err (fn [update' :- (t/All [m k v] [m k [(t/Get m k) :-> v] :-> v])
                  f :- ['1 :-> '2]]
               (t/ann-form (update' {:a 1} :a f) '1)))
  #_;TODO
  (is-tc-e (fn [update'' :- (t/All [m k v] [m k [[m k :-> (t/Get m k)] :-> v] :-> v])
                f :- [['{:a '1} '1 :-> '1] :-> '1]]
             (t/ann-form (update'' {:a 1} :a f) '1)))
  (is-tc-err (fn [update'' :- (t/All [m k v] [m k [[m k :-> (t/Get m k)] :-> v] :-> v])
                  f :- [['{:a '1} '1 :-> '1] :-> '1]]
               (t/ann-form (update'' {:a 2} :a f) '1)))
  #_;TODO
  (is-tc-e (fn [update'' :- (t/All [m k v] [[k :-> m] k [[m k :-> (t/Get m k)] :-> v] :-> v])
                f1 :- ['1 :-> '{:a '1}]
                f2 :- [['{:a '1} '1 :-> '1] :-> '1]]
             (t/ann-form (update'' f1 :a f2) '1)))
  (is-tc-e (fn [f :- ['1 :-> '1]]
             (update {:a 1} :a f)))
  (is-tc-e (fn [f :- ['1 :-> '2]]
             (update {:a 1} :a f)))
  (is-tc-err (fn [f :- ['2 :-> '1]]
               (update {:a 1} :a f)))
  (is-tc-err (fn [f :- ['2 :-> '2]]
               (update {:a 1} :a f)))
  (is-tc-e (fn [f :- ['2 :-> '2]]
             (update {:a 2} :a f)))
  (is-tc-e (fn [f :- ['1 :-> '1]]
             (let [res (update {:a 1} :a f)]
               (t/ann-form res '{:a '1}))))
  (is-tc-e (fn [f :- ['1 :-> '1]]
             (t/ann-form (update {:a 1} :a f) '{:a '1})))
  (is-tc-err (fn [f :- ['2 :-> '2]]
               (t/ann-form (update {:a 1} :a f) '{:a '1})))
  (is-tc-err (fn [f :- ['2 :-> '1]]
               (t/ann-form (update {:a 1} :a f) '{:a '1})))
  (is-tc-err (fn [f :- ['1 :-> '2]]
               (t/ann-form (update {:a 1} :a f) '{:a '1})))
  (is-tc-err (update {:a 1} + 1))
  (is-tc-err (update {:a 1} + 1) '{:a t/Bool})
  (is-tc-e (update {:a 1} :a identity) '{:a t/Int})
  (is-tc-err (update {:a 1} :a identity 1))
  (is-tc-e (update {:a 1} :a inc))
  (is-tc-e (update {:a 1} :a inc) '{:a t/Int})
  (is-tc-e (update {:a 1} :a inc) (t/HMap {:a t/Int} :complete? true))
  (is-tc-err (update {:a 1} :a inc) '{:a t/Bool})
  (is-tc-err (update {:a true} :a inc) '{:a t/Int})
  (is-tc-e (fn [v :- (t/Get (t/HMap :mandatory {:a t/Int} :complete? true) (t/Val :a))]
             :- t/Int
             (inc v)))
  (is-tc-e (update {:a 1} :a + 1))
  (is-tc-e (update {:a 1} :a + 1) '{:a t/Int})
  (is-tc-e (update {:a 1} :a + 1 2))
  (is-tc-err (update {:a 1} :a + 1 2 nil))
  (is-tc-e (update {:a 1} :a + 1 2) '{:a t/Int})
  (is-tc-e (fn [f :- [t/Int t/Kw t/Bool :-> t/Kw]]
             (update {:a 1} :a f :f true)))
  (is-tc-e (fn [f :- [t/Int t/Kw t/Bool :-> t/Kw]]
             :- '{:a t/Kw}
             (update {:a 1} :a f :f true)))
  (is-tc-err (fn [f :- [t/Int t/Kw t/Bool :-> t/Kw]]
               (update {:a 1} :a f true :f)))
  (is-tc-err (fn [f :- [t/Int t/Kw t/Bool :-> t/Kw]]
               :- '{:a t/Kw}
               (update {:a 1} :a f true :f))))

(deftest csgen-intersect
  (is-tc-e (do 
             (defprotocol 
               [[x :variance :covariant]]
               ICollection')
             (defprotocol 
               [[x :variance :covariant]]
               ISeq')
             (defprotocol 
               [[x :variance :covariant]]
               ISeqable')
             (defprotocol 
               [[x :variance :covariant]]
               IList')
             (defalias
               NEColl'
               (t/TFn [[x :variance :covariant]]
                    (ICollection' x)))
             (defalias
               NEASeq'
               (t/TFn [[x :variance :covariant]]
                    (t/I
                      (ICollection' x)
                      (ISeqable' x)
                      (ISeq' x)
                      (IList' x)
                      #_(CountRange 1))))
             (fn [seq' :- (t/All [x] [(NEColl' x) -> (NEASeq' x)])
                  a :- (NEColl' t/Int)] 
               :- (NEASeq' Number)
               (seq' a)))))

(deftest cs-gen-DataType-Protocol-test
  (is-tc-e (do (defprotocol [[foo :variance :covariant]]
                 Foo)
               (t/ann-datatype [[bar :variance :covariant]]
                               FooD []
                               :extends
                               [(Foo bar)])
               (deftype FooD []
                 Foo)
               (fn [upcast :- (t/All [x] [(Foo x) :-> x])
                    dt :- (FooD t/Int)]
                 :- t/Int
                 (upcast dt))))
  (is-tc-err (do (defprotocol [[foo :variance :covariant]]
                   Foo)
                 (t/ann-datatype [[bar :variance :covariant]]
                                 FooD []
                                 :extends
                                 [(Foo bar)])
                 (deftype FooD []
                   Foo)
                 (fn [upcast :- (t/All [x] [(Foo x) :-> x])
                      dt :- (FooD t/Int)]
                   :- t/Bool ;;bad return
                   (upcast dt))))
  (is-tc-e (do (t/defprotocol
                 [[foo :variance :covariant]]
                 Foo
                 (bar- [this] :- foo))

               (t/ann-datatype FooD [t :- t/Symbol]
                               :extends
                               [(Foo t/Symbol)])

               (deftype FooD [t]
                 Foo
                 (bar- [this] t))

               (t/ann-form (bar- (->FooD 'a))
                           t/Symbol))))

(deftest cs-gen-RClass-Protocol-test
  (is-tc-e (do (defprotocol [[a :variance :covariant]] Prot)
               (definterface Inter)
               (override-classes Inter [[[a :variance :covariant]]
                                        :unchecked-ancestors
                                        [(Prot a)]])
               (fn [upcast :- (t/All [x] [(Prot x) :-> x])
                    inter :- (Inter t/Int)]
                 :- t/Int
                 (upcast inter)))
           :requires [[typed.clojure.jvm :refer [override-classes]]])
  (is-tc-err (do (defprotocol [[a :variance :covariant]] Prot)
                 (definterface Inter)
                 (override-classes Inter [[[a :variance :covariant]]
                                          :unchecked-ancestors
                                          [(Prot a)]])
                 (fn [upcast :- (t/All [x] [(Prot x) :-> x])
                      inter :- (Inter t/Int)]
                   :- t/Bool ;; bad return
                   (upcast inter)))
             :requires [[typed.clojure.jvm :refer [override-classes]]]))

(deftest cs-gen-no-best-inst-test
  (is-tc-e (fn [f :- (t/All [x] [:-> [x :-> x]])]
             (let [e (f)]
               (clojure.core.typed/print-env "a"))))
  ;; TODO return symbolic closure from (f)
  #_
  (is-tc-e (do (fn [f :- (t/All [x] [:-> [x :-> x]])]
             (let [e (f)]
               (e 1)))))
  ;;FIXME should error, no best instantiation
  #_
  (is-tc-err (do (defprotocol [[x :variance :invariant]]
                   Inv)
                 (fn [f :- (t/All [x] [:-> (Inv x)])]
                   (f))))
  ;;FIXME should error, no best instantiation
  #_
  (is-tc-err (do (defprotocol [[x :variance :invariant]]
                   Inv)
                 (fn [f :- (t/All [x] [:-> (Inv x)])]
                   (let [inv (f)]
                     (t/ann-form inv (Inv t/Nothing))))))
  (is-tc-err (do (defprotocol [[x :variance :invariant]]
                   Inv)
                 (fn [f :- (t/All [x] [:-> (Inv x)])]
                   (let [inv (f)]
                     (t/ann-form inv (Inv t/Any))))))
  (is-tc-e (do (defprotocol [[x :variance :invariant]]
                 Inv)
               (fn [f :- (t/All [x] [:-> (Inv x)])]
                 (t/ann-form (f) (Inv t/Any))
                 (t/ann-form (f) (Inv t/Nothing))
                 (t/ann-form (f) (Inv t/Int))
                 (t/ann-form (f) (Inv t/Bool)))))
  (is-tc-e (fn [f :- (t/All [x] [:-> (t/Atom x)])]
             (t/ann-form @(f) t/Nothing)))
  (is-tc-e (fn [f :- (t/All [x] [:-> [x :-> (t/Atom x)]])
                a :- t/Nothing]
             (t/ann-form @((f) a) t/Nothing)))
  ;; TODO return symbolic closure from (f)
  #_
  (is-tc-e (fn [f :- (t/All [x] [:-> [x :-> (t/Atom x)]])
                a :- t/Int]
             (t/ann-form @((f) a) t/Int)))
  (is-tc-err (do (defprotocol [[x :variance :invariant]]
                   Atom)
                 (fn [swap! :- (t/All [x] [(Atom x) [x :-> x] :-> x])
                      a :- (Atom (t/Nilable t/Int))
                      inc :- [t/Int :-> t/Int]]
                   (swap! a inc))))
  (is-tc-e (do (defprotocol [[x :variance :invariant]]
                 Inv)
               (defprotocol [[x :variance :covariant]]
                 Cov)
               (fn [f :- (t/All [x] [(Inv x) (Cov x) :-> x])
                    inv :- (Inv (t/Nilable t/Int))
                    ;; can upcast to (Cov (t/Nilable t/Int))
                    cov :- (Cov t/Int)]
                 (f inv cov))))
  (is-tc-err (do (defprotocol [[x :variance :invariant]]
                   Inv)
                 (defprotocol [[x :variance :contravariant]]
                   Contra)
                 (fn [f :- (t/All [x] [(Inv x) (Contra x) :-> x])
                      inv :- (Inv (t/Nilable t/Int))
                      ;; cannot upcast to (Contra (t/Nilable t/Int))
                      contra :- (Contra t/Int)]
                   (f inv contra))))
  (is-tc-err (do (defprotocol [[x :variance :contravariant]]
                   Contra)
                 (fn [contra :- (Contra t/Int)]
                   :- (Contra (t/Nilable t/Int))
                   contra)))
  (is-tc-err (do (defprotocol [[x :variance :contravariant]]
                   Inv)
                 (fn [inv :- (Inv t/Int)]
                   :- (Inv (t/Nilable t/Int))
                   inv)))
  (is-tc-e (do (defprotocol [[x :variance :covariant]]
                 Cov)
               (fn [cov :- (Cov t/Int)]
                 :- (Cov (t/Nilable t/Int))
                 cov)))
  (is-tc-e (do (defprotocol [[x :variance :contravariant]]
                 Contra)
               (fn [f :- (t/All [x] [(Contra x) (Contra x) :-> x])
                    contra1 :- (Contra (t/Nilable t/Int))
                    contra2 :- (Contra t/Int)]
                 (let [a (f contra1 contra2)]
                   (ann-form a (Contra t/Int))))))
  (is-tc-e (do (defprotocol [[x :variance :contravariant]]
                 Contra)
               (fn [f :- (t/All [x] [(Contra x) (Contra x) :-> x])
                    contra1 :- (Contra (t/Nilable t/Int))
                    contra2 :- (Contra t/Int)]
                 :- t/Int
                 (f contra1 contra2))))
  ;;FIXME
  #_
  (is-tc-err (do (defprotocol [[x :variance :contravariant]]
                   Contra)
                 (fn [f :- (t/All [x] [(Contra x) (Contra x) :-> x])
                      contra1 :- (Contra (t/Nilable t/Int))
                      contra2 :- (Contra t/Int)]
                   (f contra1 contra2))))
  (is-tc-e (do (defprotocol [[x :variance :covariant]]
                 Cov)
               (fn [f :- (t/All [x] [(Cov x) (Cov x) :-> x])
                    cov1 :- (Cov (t/Nilable t/Int))
                    cov2 :- (Cov t/Int)]
                 (f cov1 cov2))))
  (is-tc-err (do (defprotocol [[x :variance :covariant]]
                   Cov)
                 (fn [f :- (t/All [x] [(Cov x) (Cov x) :-> x])
                      cov1 :- (Cov (t/Nilable t/Int))
                      cov2 :- (Cov t/Int)]
                   :- t/Int ;; should be nilable
                   (f cov1 cov2))))
  (is-tc-e (do (defprotocol [[x :variance :covariant]]
                 Cov)
               (fn [f :- (t/All [x] [(Cov x) (Cov x) :-> x])
                    cov1 :- (Cov (t/Nilable t/Int))
                    cov2 :- (Cov t/Int)]
                 :- (t/Nilable t/Int)
                 (f cov1 cov2))))
  )

(deftest cs-gen-Instance-RClass-test
  (is-tc-e (fn [a :- t/AnyAtom
                f :- (t/All [x] [(t/Deref x) :-> (t/Deref x)])]
             (f a)))
  (is-tc-err (fn [a :- t/AnyAtom
                  f :- (t/All [x] [(t/Deref x) :-> (t/Deref x)])]
               :- t/Nothing
               (f a))))
