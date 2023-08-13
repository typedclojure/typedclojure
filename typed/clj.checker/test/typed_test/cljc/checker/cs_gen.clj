(ns typed-test.cljc.checker.cs-gen
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.cs-gen :as sut]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.clj.checker.parse-unparse :refer [parse-type]]
            [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]))

(deftest solve-test
  (is-tc-e 1) ;;load type system
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

(deftest infer->tv-test
  (clj (let [{:keys [t tvs]} (sut/infer->tv (parse-type `t/Int))]
         (is (= (parse-type `t/Int) t))
         (is (empty? tvs))))
  (clj (let [{:keys [t tvs]} (sut/infer->tv r/-infer-any)]
         (is (r/F? t))
         (is (= 1 (count tvs)))))
  (clj (let [{:keys [t tvs]} (sut/infer->tv (parse-type `[t/Int :-> [^:clojure.core.typed/infer t/Any :-> t/Int]]))]
         (is (= 1 (count tvs)))
         (is (= (parse-type `[t/Int :-> [t/Bool :-> t/Int]])
                (subst/subst-all
                  {(first tvs) (crep/t-subst-maker (parse-type `t/Bool) r/no-bounds)}
                  t)))
         (is (= (parse-type `[t/Int :-> [t/Num :-> t/Int]])
                (subst/subst-all
                  {(first tvs) (crep/t-subst-maker (parse-type `t/Num) r/no-bounds)}
                  t))))))

(deftest eliminate-infer-any-test
  (is-clj (both-subtype? (parse-type `t/Bool)
                         (sut/eliminate-infer-any (parse-type `t/Bool) r/-infer-any)))
  (is-clj (both-subtype? (parse-type `t/Int)
                         (sut/eliminate-infer-any (parse-type `t/Int) r/-infer-any)))
  (is-clj (not (both-subtype? (parse-type `t/Bool)
                              (sut/eliminate-infer-any (parse-type `t/Int) r/-infer-any))))
  (is-clj (both-subtype? (parse-type `t/Num)
                         (sut/eliminate-infer-any (parse-type `t/Int)
                                                  (parse-type `t/Num))))
  ;; this breaks an assumption of the function that s <: t.
  (is-clj (= (parse-type `t/Int)
             (sut/eliminate-infer-any (parse-type `t/Num)
                                      (parse-type `t/Int))))
  (is-clj (subtype? (parse-type `[t/Bool :-> t/Int])
                    (sut/eliminate-infer-any (parse-type `[t/Bool :-> t/Int])
                                             (parse-type `[^:clojure.core.typed/infer t/Any :-> t/Num]))))
  (is-clj (subtype? (parse-type `[t/Bool :-> t/Num])
                    (sut/eliminate-infer-any (parse-type `[t/Bool :-> t/Int])
                                             (parse-type `[^:clojure.core.typed/infer t/Any :-> t/Num]))))
  ;; this breaks an assumption of the function that s <: t.
  (is-clj (= (parse-type `[^:clojure.core.typed/infer t/Any :-> t/Int])
             (sut/eliminate-infer-any (parse-type `[t/Bool :-> t/Num])
                                      (parse-type `[^:clojure.core.typed/infer t/Any :-> t/Int]))))
  (is-clj (subtype? (parse-type `[t/Bool :-> t/Int])
                    (sut/eliminate-infer-any (parse-type `[t/Bool :-> t/Int])
                                             (parse-type `[^:clojure.core.typed/infer t/Any :-> ^:clojure.core.typed/infer t/Any]))))
  (is-clj (not (subtype? (parse-type `[t/Bool :-> t/Bool])
                         (sut/eliminate-infer-any (parse-type `[t/Bool :-> t/Int])
                                                  (parse-type `[^:clojure.core.typed/infer t/Any :-> ^:clojure.core.typed/infer t/Any])))))
  (is-clj (not (subtype? (parse-type `[t/Int :-> t/Int])
                         (sut/eliminate-infer-any (parse-type `[t/Bool :-> t/Int])
                                                  (parse-type `[^:clojure.core.typed/infer t/Any :-> ^:clojure.core.typed/infer t/Any])))))
  (is-clj (both-subtype? (parse-type `(t/Transducer t/Bool t/Int))
                         (sut/eliminate-infer-any (parse-type `(t/Transducer t/Bool t/Int))
                                                  (parse-type `(t/Transducer ^:clojure.core.typed/infer t/Any ^:clojure.core.typed/infer t/Any)))))
  (is-clj (not (both-subtype? (parse-type `(t/Transducer t/Bool t/Bool))
                              (sut/eliminate-infer-any (parse-type `(t/Transducer t/Bool t/Int))
                                                       (parse-type `(t/Transducer ^:clojure.core.typed/infer t/Any ^:clojure.core.typed/infer t/Any))))))
  (is-clj (not (both-subtype? (parse-type `(t/Transducer t/Int t/Int))
                              (sut/eliminate-infer-any (parse-type `(t/Transducer t/Bool t/Int))
                                                       (parse-type `(t/Transducer ^:clojure.core.typed/infer t/Any ^:clojure.core.typed/infer t/Any))))))
  (is-clj (not (both-subtype? (parse-type `(t/Transducer t/Int t/Bool))
                              (sut/eliminate-infer-any (parse-type `(t/Transducer t/Bool t/Int))
                                                       (parse-type `(t/Transducer ^:clojure.core.typed/infer t/Any ^:clojure.core.typed/infer t/Any))))))
  )
