(ns typed-test.spec.clojure.core
  (:require [typed.spec.clojure.core :as api]
            [clojure.alpha.spec :as s]
            [clojure.alpha.spec.gen :as gen]
            [clojure.alpha.spec.test :as stest]
            [typed.clj.spec :as t]
            [typed.clj.spec.test-utils :as tu]
            [clojure.test :refer :all]))

(deftest reduced-spec-test
  (doseq [v (gen/sample
              (s/gen (api/reduced-spec integer?)))]
    (assert (reduced? v) v)
    (is (integer? @v) v))
  (tu/is-valid (api/reduced-spec integer?) (reduced 1))
  (tu/is-invalid (api/reduced-spec symbol?) (reduced 1))
  (tu/is-valid (s/or :reduced (api/reduced-spec integer?)
                     :integer integer?)
               (reduced 1))
  (tu/is-invalid (s/or :reduced (api/reduced-spec integer?)
                       :integer integer?)
                 (gensym))
  (tu/is-invalid (api/reduced-spec integer?)
                 (gensym)))
