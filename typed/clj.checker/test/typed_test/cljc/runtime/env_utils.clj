;; tests live in clj.checker for is-tc-e etc
(ns typed-test.cljc.runtime.env-utils
  (:require [clojure.test :refer [deftest is testing]]
            [typed.clojure :as t]
            [typed.cljc.checker.var-env :as var-env]
            [typed.clj.checker.test-utils :refer [is-clj is-tc-e is-tc-err clj-opts]]))

(deftest forget-types-after-ns-remove-test
  (some-> (find-ns 'forget-types-after-ns-remove-test) ns-name remove-ns)
  (is-clj (nil? (var-env/lookup-Var-nofail 'forget-types-after-ns-remove-test/foo clj-opts)))
  (is-tc-e (do (ns forget-types-after-ns-remove-test)
               (typed.clojure/ann forget-types-after-ns-remove-test/foo ':foo)))
  (is-clj (some? (var-env/lookup-Var-nofail 'forget-types-after-ns-remove-test/foo clj-opts)))
  (is (some-> (find-ns 'forget-types-after-ns-remove-test) ns-name remove-ns))
  (is-clj (nil? (var-env/lookup-Var-nofail 'forget-types-after-ns-remove-test/foo clj-opts))))

(deftest forget-types-in-defining-ns-after-ns-remove-test
  (some-> (find-ns 'forget-types-after-ns-remove-test) ns-name remove-ns)
  (is-clj (nil? (var-env/lookup-Var-nofail 'forget-types-after-ns-remove-test/foo clj-opts)))
  (is-tc-e (do (ns forget-types-after-ns-remove-test)
               (typed.clojure/tc-ignore
                 (defn register-ann! []
                   (typed.clojure/ann forget-types-after-ns-remove-test/foo ':foo)))
               (ns registering-ns)
               (typed.clojure/tc-ignore
                 ;; at runtime, *ns* will be 'registering-ns.
                 (forget-types-after-ns-remove-test/register-ann!))))
  (is-clj (some? (var-env/lookup-Var-nofail 'forget-types-after-ns-remove-test/foo clj-opts)))
  (is (some-> (find-ns 'forget-types-after-ns-remove-test) ns-name remove-ns))
  (is-clj (nil? (var-env/lookup-Var-nofail 'forget-types-after-ns-remove-test/foo clj-opts))))
