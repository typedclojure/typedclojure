(ns ^:typed.clojure typed-test.cljc.checker.check-below
  (:require [typed.clj.checker.test-utils :refer :all]
            [typed.cljc.checker.check-below :as sut]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :refer [parse-type]]
            [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]))

(deftest check-below-test
  (is-tc-e 1) ;;load type system
  (is-tc-err (-> 1
                 (t/ann-form t/Infer)
                 (t/ann-form t/Int)))
  (is-tc-e (-> 1
               (t/ann-form t/Int)
               (t/ann-form t/Infer)))
  (is-clj (thrown? Error (sut/check-below r/-wild (parse-type `t/Int (clj-opts)) (clj-opts))))
  (is-clj (both-subtype?
            (sut/check-below (parse-type `t/Int (clj-opts)) r/-wild (clj-opts))
            (parse-type `t/Int (clj-opts))))
  (is-clj (both-subtype?
            (sut/check-below (parse-type `t/Int (clj-opts)) r/-any (clj-opts))
            (parse-type `t/Any (clj-opts))))
  (is-clj (both-subtype? (sut/check-below (parse-type `[:-> t/Int] (clj-opts)) (parse-type `[:-> t/Infer] (clj-opts)) (clj-opts))
                         (parse-type `[:-> t/Int] (clj-opts))))
  (is-clj (both-subtype? (sut/check-below (parse-type `[[t/Int :-> t/Int] :-> t/Int] (clj-opts))
                                          (parse-type `[[t/Infer :-> (t/Val 1)] :-> t/Infer] (clj-opts))
                                          (clj-opts))
                         (parse-type `[[t/Int :-> (t/Val 1)] :-> t/Int] (clj-opts)))))
