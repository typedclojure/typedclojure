(ns ^:typed.clojure typed-test.clojure
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [clojure.edn :as edn]))

(deftest check-ns-*-test
  (some-> (find-ns 'typed-test.clojure.succeed-cljs-fail-clj) ns-name remove-ns)
  (some-> (find-ns 'typed-test.clojure.succeed-clj-fail-cljs) ns-name remove-ns)
  (is (t/check-ns-clj 'typed-test.clojure.succeed-clj-fail-cljs))
  (is (t/cns 'typed-test.clojure.succeed-clj-fail-cljs))
  (is (err/top-level-error-thrown? (t/check-ns-clj 'typed-test.clojure.succeed-cljs-fail-clj)))
  (is (err/top-level-error-thrown? (eval `(t/cns 'typed-test.clojure.succeed-cljs-fail-clj)))))

(deftest check-ns-even-if-not-dependent-on-tc-test
  (is (err/top-level-error-thrown? (t/check-ns-clj 'typed-test.clojure.does-not-depend-on-tc))))

(deftest check-ns-ignore-test
  (is (t/check-ns-clj 'typed-test.clojure.tc-ignore-ns)))

(deftest cf-*-test
  (some-> (find-ns 'typed-test.clojure.cf-prep) ns-name remove-ns)
  ;; setup
  (is (t/check-ns-clj 'typed-test.clojure.cf-prep))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf ~'int->clj-bool->cljs)))
         't/Int))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf-clj ~'int->clj-bool->cljs)))
         't/Int)))
