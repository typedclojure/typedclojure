(ns typed-test.clojure
  (:require [clojure.test :refer [deftest is]]
            [typed.clojure :as t]
            [clojure.core.typed.errors :as err]))

(deftest check-ns-*-test
  (is (t/check-ns-clj 'typed-test.clojure.succeed-clj-fail-cljs))
  ;; TODO test that this should fail when run in cljs
  (is (t/check-ns 'typed-test.clojure.succeed-clj-fail-cljs))
  (is (err/top-level-type-error-thrown? (t/check-ns-cljs 'typed-test.clojure.succeed-clj-fail-cljs)))
  (is (err/top-level-type-error-thrown? (t/check-ns-clj 'typed-test.clojure.succeed-cljs-fail-clj)))
  ;; TODO test that this should succeed when run in cljs
  (is (err/top-level-type-error-thrown? (t/check-ns 'typed-test.clojure.succeed-cljs-fail-clj)))
  (is (err/top-level-type-error-thrown? (t/check-ns 'typed-test.clojure.succeed-cljs-fail-clj)))
  (is (t/check-ns-cljs 'typed-test.clojure.succeed-cljs-fail-clj)))

(deftest check-ns-even-if-not-dependent-on-tc-test
  (is (err/top-level-type-error-thrown? (t/check-ns-clj 'typed-test.clojure.does-not-depend-on-tc)))
  (is (err/top-level-type-error-thrown? (t/check-ns-cljs 'typed-test.clojure.does-not-depend-on-tc))))
