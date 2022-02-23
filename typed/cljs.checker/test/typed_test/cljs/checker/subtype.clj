(ns typed-test.cljs.checker.subtype
  (:require [clojure.test :refer [deftest]]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljs.checker.test-utils :refer [is-cljs]]))

(deftest subtype-prims-cljs-test
  (is-cljs (sub/subtype? (r/-val 1) (prs/parse-cljs 'typed.clojure/JSnumber))))
