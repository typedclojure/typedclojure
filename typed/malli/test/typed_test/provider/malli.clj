(ns typed-test.provider.malli
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.core.typed.util-vars :as uv]
            [clojure.core.typed.current-impl :as impl]))

;; these tests implicitly load typed.clj{s}.provider.malli 
(deftest var-provider-test
  (is (t/check-ns-clj 'typed-test.provider.malli__test1))
  (is (t/check-ns-cljs* 'typed-test.provider.malli__test1))
  (is (err/top-level-error-thrown? (t/check-ns-clj 'typed-test.provider.malli__test-fail1)))
  (is (err/top-level-error-thrown? (t/check-ns-cljs* 'typed-test.provider.malli__test-fail1)))
  (is (t/check-ns-clj 'typed-test.provider.malli__test-fail-just-in-cljs1))
  ;;FIXME malli schemas from clj are being provided to the cljs checker
  #_
  (is (err/top-level-error-thrown? (t/check-ns-cljs* 'typed-test.provider.malli__test-fail-just-in-cljs1)))
  )
