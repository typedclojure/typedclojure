(ns typed-test.clj.spec.memoize
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [clojure.test :refer :all]))

;clojure.core/memoize (All [x y ...]
;                            [[y ... y -> x] -> [y ... y -> x]])

(s/def ::memoize
  (all :binder
       (binder
         :x (tvar-spec)
         :y (tvar-spec :kind (s/* (binder
                                    :y (tvar-spec)))))
       :body
       (s/fspec :args (s/cat :fn
                             (s/fspec :args (dotted-pretype (tvar :y) :y)
                                      :ret (tvar :x)))
                :ret (s/fspec :args (dotted-pretype (tvar :y) :y)
                              :ret (tvar :x)))))

(deftest memoize-test
  (is (s/valid? ::memoize memoize))
  ; a slightly less thorough memoize function..
  (is (s/valid? ::memoize identity)))
