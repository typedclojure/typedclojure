(ns typed-test.clj.spec.impl
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :as t]
            [typed.clj.spec.impl :as impl]
            [clojure.test :refer :all]))

(deftest prewalk-test
  (is (= 2
         (impl/prewalk (fn [config form]
                         {:config config
                          :form (inc form)})
                       {} 1)))
  (is (= [2 [42 4]]
         (impl/prewalk (fn [config form]
                         {:config config
                          :form (if (integer? form)
                                  (if (= 2 form)
                                    42
                                    (inc form))
                                  form)})
                       {}
                       [1 [2 3]]))))

(deftest subst-tv-test
  (is (= 1
         (impl/subst-tv `(t/tv :x)
                        {:x 1}
                        impl/spec2-version-info-base)))
  (is (= `(vector 1)
         (impl/subst-tv `(vector (t/tv :x))
                        {:x 1}
                        impl/spec2-version-info-base)))
  (is (= `(s/fspec :args (s/cat :xs integer?)
                   :ret integer?)
         (impl/subst-tv `(s/fspec :args (s/cat :xs (t/tv :x))
                                  :ret (t/tv :x))
                        {:x `integer?}
                        impl/spec2-version-info-base)))
  (is (= `(s/fspec :args (s/cat :xs (s/cat :x0 integer?))
                   :ret (s/every integer?
                                 :count 0))
         (impl/subst-tv `(s/fspec :args (s/cat :xs (t/fold-binders (t/tv :x) :x))
                                  :ret (s/every integer?
                                                :count (t/fold-binders (t/tv :N) :x
                                                                       :wrap #(apply min %))))
                        `{:x [{:N 0 :x integer?}]}
                        impl/spec2-version-info-base)))
  ;substitute tvs in expansion of tv
  (is (= (impl/subst-tv `(t/tv :x
                               :wrap
                               (fn [x#]
                                 `(t/tv :y)))
                        `{:x 1 :y 2}
                        impl/spec2-version-info-base)
         ; (tv :x :wrap ...) => (tv :y) => 2
         `2))
  ;don't traverse inside tv if not substituting
  (is (= (impl/subst-tv `(t/tv :x
                               :wrap
                               (fn [~'x]
                                 `[~'x (t/tv :y)]))
                        `{:y 2}
                        impl/spec2-version-info-base)
         `(t/tv :x
                :wrap
                (fn [~'x]
                  `[~'x (t/tv :y)])))))
