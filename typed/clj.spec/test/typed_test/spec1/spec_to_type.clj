(ns ^:typed.clojure typed-test.spec1.spec-to-type
  (:require [clojure.test :refer [deftest is]]
            [typed.spec1.spec-to-type :as sut]
            [clojure.spec.alpha :as s]
            [typed.clojure :as t]))

(deftest spec->type-test
  (is (= `t/AnyInteger
         (sut/spec->type (s/spec int?) {})))
  (is (= `[t/AnyInteger :-> t/AnyInteger]
         (sut/spec->type (s/fspec :args (s/cat :x int?) :ret int?) {}))))
