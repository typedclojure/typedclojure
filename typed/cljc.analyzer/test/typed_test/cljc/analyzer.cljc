;; TODO for now, just tests that typed.cljc.analyzer is loadable
(ns ^:typed.clojure typed-test.cljc.analyzer
  (:require #?(:clj [clojure.test :as t]
               :cljs [cljs.test :as t]
               :cljr [clojure.test :as t]
               :default (throw (ex-info "Unknown impl" {})))
            [typed.cljc.analyzer :as ana]))

