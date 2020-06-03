(ns typed-test.cljc.analyzer
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t]
               :default (throw (ex-info "Unknown impl" {})))
            [typed.cljc.analyzer :as ana]))

(prn "common test")

(t/deftest fake-test
  (t/is (= 1 1)))
