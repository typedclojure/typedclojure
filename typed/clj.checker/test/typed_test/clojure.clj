(ns typed-test.clojure
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]
            [clojure.core.typed.errors :as err]
            [clojure.edn :as edn]
            [typed.cljs.checker.test-utils :refer [cljs-eval]]))

(defn prep-temp-cljs-ns-form []
  `(~'ns ~(gensym "temp.ns")
     ;; FIXME ns doesn't seem to like macros that expand to macros in other namespaces?
     ;; eg., typed.clojure/ann-form => clojure.core.typed/ann-form.
     ;; (but only from a CLJS repl)
     ;; or perhaps it's just that ana-api/resolve-sym does not register the latter expansion?
     ;; we should be able to remove the cljs.core.typed require here.
     ~'(:require [cljs.core.typed :include-macros true]
                 [typed.clojure :as t :include-macros true])))

(deftest check-ns-*-test
  (is (t/check-ns-clj 'typed-test.clojure.succeed-clj-fail-cljs))
  ;; TODO test that this should fail when run in cljs
  (is (t/cns 'typed-test.clojure.succeed-clj-fail-cljs))
  (is (err/top-level-type-error-thrown? (t/check-ns-cljs 'typed-test.clojure.succeed-clj-fail-cljs)))
  (is (err/top-level-type-error-thrown? (t/check-ns-clj 'typed-test.clojure.succeed-cljs-fail-clj)))
  (testing "t/cns fail in cljs"
    (let [rs (cljs-eval 20 15000
                        [(prep-temp-cljs-ns-form)
                         '(t/cns 'typed-test.clojure.succeed-clj-fail-cljs)])]
      (is (= :top-level-error
             (get-in (edn/read-string {:readers {'error identity}} (:val (peek rs)))
                     [:data :type-error]))
          rs)))
  (is (err/top-level-type-error-thrown? (eval `(t/cns 'typed-test.clojure.succeed-cljs-fail-clj))))
  (is (err/top-level-type-error-thrown? (eval `(t/cns 'typed-test.clojure.succeed-cljs-fail-clj))))
  (is (t/check-ns-cljs 'typed-test.clojure.succeed-cljs-fail-clj))
  (testing "t/cns succeed in cljs"
    (let [rs (cljs-eval 20 15000
                        [(prep-temp-cljs-ns-form)
                         '(t/cns 'typed-test.clojure.succeed-cljs-fail-clj)])]
      (is (= :ok (edn/read-string (:val (peek rs))))
          rs))))

(deftest check-ns-even-if-not-dependent-on-tc-test
  (is (err/top-level-type-error-thrown? (t/check-ns-clj 'typed-test.clojure.does-not-depend-on-tc)))
  (is (err/top-level-type-error-thrown? (t/check-ns-cljs 'typed-test.clojure.does-not-depend-on-tc))))

(deftest cf-*-test
  ;; setup
  (is (t/check-ns-clj 'typed-test.clojure.cf-prep))
  (is (t/check-ns-cljs 'typed-test.clojure.cf-prep))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf ~'int->clj-bool->cljs)))
         't/Int))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf-clj ~'int->clj-bool->cljs)))
         't/Int))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf-cljs ~'int->clj-bool->cljs)))
         ;;FIXME should be t/Bool
         'typed.clojure/Bool))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf-cljs ~'int->clj-bool->cljs)))
         ;;FIXME should be t/Bool
         'typed.clojure/Bool))
  (is (= (eval `(binding [*ns* (the-ns 'typed-test.clojure.cf-prep)]
                  (t/cf-cljs ~'(t/ann-form true t/Bool))))
         ;;FIXME should be t/Bool
         'typed.clojure/Bool))
  (testing "t/cf in cljs"
    (let [rs (cljs-eval 5 15000
                        ['(require '[typed.clojure :as t])
                         '(t/cns 'typed-test.clojure.cf-prep)
                         '(in-ns 'typed-test.clojure.cf-prep)
                         `(t/cf ~'int->clj-bool->cljs)])]
      (is (= `t/Bool ;;FIXME should be t/Bool
             (edn/read-string (:val (peek rs))))
          rs)))
  (testing "t/cf in cljs"
    (let [rs (cljs-eval 5 15000
                        ['(require '[typed.clojure :as t])
                         '(t/cns 'typed-test.clojure.cf-prep)
                         '(in-ns 'typed-test.clojure.cf-prep)
                         `(t/cf-cljs ~'int->clj-bool->cljs)])]
      (is (= `t/Bool ;;FIXME should be t/Bool
             (edn/read-string (:val (peek rs))))
          rs)))
  (testing "t/cf-clj in cljs"
    (let [rs (cljs-eval 5 15000
                        ['(require '[typed.clojure :as t])
                         '(in-ns 'typed-test.clojure.cf-prep)
                         `(t/cf-clj ~'int->clj-bool->cljs)])]
      (is (= 't/Int
             (edn/read-string (:val (peek rs))))
          rs))))
