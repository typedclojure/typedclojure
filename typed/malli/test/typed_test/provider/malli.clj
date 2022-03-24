(ns typed-test.provider.malli
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [typed.provider.malli :as sut]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as uv]
            [clojure.core.typed.current-impl :as impl]))

(defmacro is-Malli= [expected t]
  `(impl/with-clojure-impl
     (binding [uv/*verbose-types* true]
       (is (= ~expected
            (prs/unparse-type (sut/Malli ~t)))))))

(deftest Malli-test
  (is-Malli= `t/AnyInteger '(Malli :int))
  (is-Malli= `(t/Seqable t/AnyInteger) '(Malli [:* :int])))

(deftest var-provider-test
  (is (t/check-ns-clj 'typed-test.provider.malli__test1))
  (is (err/top-level-error-thrown? (t/check-ns-clj 'typed-test.provider.malli__test-fail1))))
