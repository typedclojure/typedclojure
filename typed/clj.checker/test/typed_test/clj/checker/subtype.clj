(ns typed-test.clj.checker.subtype
  (:require [typed.clojure :as t]
            [typed.clj.checker.subtype :as sut]
            [typed.cljc.checker.type-ctors :as c]
            [typed.clj.checker.test-utils :refer [is-clj]]
            [clojure.test :refer [deftest is]]))

(deftest subtypes-varargs?-test
  (is-clj (sut/subtypes-varargs? [(c/-name `t/Int)]
                                 [(c/-name `t/Num)]
                                 nil
                                 nil))
  (is-clj (not (sut/subtypes-varargs? []
                                      [(c/-name `t/Num)]
                                      nil
                                      nil)))
  (is-clj (not (sut/subtypes-varargs? [(c/-name `t/Num)]
                                      [(c/-name `t/Int)]
                                      nil
                                      nil)))
  (is-clj (sut/subtypes-varargs? [(c/-name `t/Int)]
                                 []
                                 (c/-name `t/Num)
                                 nil))
  (is-clj (not (sut/subtypes-varargs? [(c/-name `t/Num)]
                                      []
                                      (c/-name `t/Int)
                                      nil)))
  (is-clj (sut/subtypes-varargs? [(c/-name `t/Num) (c/-name `t/Int)]
                                 [(c/-name `t/Num)]
                                 (c/-name `t/Int)
                                 nil))
  )
