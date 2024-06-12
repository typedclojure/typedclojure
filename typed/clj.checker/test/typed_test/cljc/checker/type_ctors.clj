(ns typed-test.cljc.checker.type-ctors
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]))

(deftest RClass-of-with-unknown-params-test
  (is-clj (r/Instance?
            (c/RClass-of-with-unknown-params
              `java.lang.Comparable
              (clj-opts))))
  (is-clj (r/RClass?
            (c/RClass-of-with-unknown-params
              `java.lang.Object
              (clj-opts)))))

(deftest RClass-supers*-test
  (is-tc-e :load)
  (is-clj (= #{(c/RClass-of `Object (clj-opts))}
             (c/RClass-supers* (r/Instance-maker `Object) (clj-opts))))
  (is-clj (= (cond-> #{(c/RClass-of Number (clj-opts))
                       (c/RClass-of Object (clj-opts))
                       (c/RClass-of 'java.io.Serializable (clj-opts))
                       (r/Instance-maker `java.lang.Comparable)}
               (try (Class/forName "java.lang.constant.ConstantDesc")
                    true
                    (catch ClassNotFoundException _))
               (into [(c/RClass-of 'java.lang.constant.ConstantDesc (clj-opts))
                      (c/RClass-of 'java.lang.constant.Constable (clj-opts))]))
             (c/RClass-supers* (r/Instance-maker `Integer) (clj-opts))))
  )

(deftest In-test
  (is-clj (not (subtype?
                 (c/-name `t/Nilable (c/-name `t/Int))
                 (c/In [(c/-name `t/Nilable (c/-name `t/Int))
                        (c/-name `t/Int)]
                       (clj-opts)))))
  (is-clj (subtype?
            (c/In [(c/-name `t/Nilable (c/-name `t/Int))
                   (c/-name `t/Int)]
                  (clj-opts))
            (c/-name `t/Nilable (c/-name `t/Int))))
  (is-clj (both-subtype?
            (c/-name `t/Int)
            (c/In [(c/-name `t/Nilable (c/-name `t/Int))
                   (c/-name `t/Int)]
                  (clj-opts)))))
