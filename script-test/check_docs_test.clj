(ns check-docs-test
  (:require [check-docs :as sut]
            [clojure.test :refer [deftest is]]))

(deftest check-api-typedclojure-org-test
  (is (= :ok
         (sut/check-api-typedclojure-org
           {:curl-get (fn [url]
                        {:status 200})})))
  (is (thrown? AssertionError
               (sut/check-api-typedclojure-org
                 {:curl-get (fn [url]
                              {:status 403})}))))
