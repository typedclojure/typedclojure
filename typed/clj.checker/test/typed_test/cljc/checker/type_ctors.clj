(ns typed-test.cljc.checker.type-ctors
  (:require [clojure.test :refer [deftest is]]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]))

(deftest RClass-of-with-unknown-params-test
  (is-clj (r/Instance?
            (c/RClass-of-with-unknown-params
              `java.lang.Comparable)))
  (is-clj (r/RClass?
            (c/RClass-of-with-unknown-params
              `java.lang.Object))))

(deftest RClass-supers*-test
  (is-tc-e :load)
  (is-clj (= #{(c/RClass-of `Object)}
             (c/RClass-supers* (r/Instance-maker `Object))))
  (is-clj (= (cond-> #{(c/RClass-of Number)
                       (c/RClass-of Object)
                       (c/RClass-of 'java.io.Serializable)
                       (r/Instance-maker `java.lang.Comparable)}
               (try (Class/forName "java.lang.constant.ConstantDesc")
                    true
                    (catch ClassNotFoundException _))
               (into [(c/RClass-of 'java.lang.constant.ConstantDesc)
                      (c/RClass-of 'java.lang.constant.Constable)]))
             (c/RClass-supers* (r/Instance-maker `Integer))))
  )
