;; TODO for now, just tests that typed.cljc.analyzer is loadable
(ns typed-test.cljc.analyzer
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t]
               :default (throw (ex-info "Unknown impl" {})))
            [typed.cljc.analyzer :as ana]))

