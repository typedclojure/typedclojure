(ns ^:typed.clojure typed-test.clj.spec.memoize
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.protocols :as protocols
             :refer [Spec conform* unform* explain* gen* with-gen* describe*
                     Schema keyspecs*
                     Select]]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [clojure.test.check.generators :as tcg]
            [typed.clj.spec :refer :all :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

;clojure.core/memoize (All [x y :..]
;                            [[y :.. y -> x] -> [y :.. y -> x]])

(s/def ::memoize
  (all :binder
       (binder
         :x (bind-tv)
         :y (bind-tv :kind (s/* (binder
                                    :y (bind-tv)))))
       :body
       (s/fspec :args (s/cat :fn
                             (s/fspec :args (fold-binders (tv :y) :y)
                                      :ret (tv :x)))
                :ret (s/fspec :args (fold-binders (tv :y) :y)
                              :ret (tv :x)))))

(deftest memoize-test
  (tu/is-valid ::memoize memoize)
  (tu/is-invalid ::memoize +)
  ; a slightly less thorough memoize function..
  (tu/is-valid ::memoize identity))

(comment
  (s/explain ::memoize memoize)
  )
