(ns typed-example.zero-deps-test
  (:require [typed.clojure :as t]
            [clojure.test :refer [deftest is]]))

(deftest zero-deps-test
  (is (t/check-ns-clj 'typed-example.zero-deps))
  #_ ;;FIXME
  (is (t/check-ns-cljs 'typed-example.zero-deps))
  )
