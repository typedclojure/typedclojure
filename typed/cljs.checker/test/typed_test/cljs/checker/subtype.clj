(ns typed-test.cljs.checker.subtype
  (:require [clojure.test :refer [deftest]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljs.checker.test-utils :refer [is-cljs subtype?]]))

(deftest subtype-prims-cljs-test
  (is-cljs (subtype? (r/-val 1) (prs/parse-cljs 'typed.clojure/JSnumber))))
