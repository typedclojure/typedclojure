;; TODO for now, just tests that typed.cljc.analyzer is loadable
(ns ^:typed.clojure typed-test.cljc.analyzer
  (:require #?(:bb [clojure.test :as t]
               :cljs [cljs.test :as t]
               :cljr [clojure.test :as t]
               :default [clojure.test :as t])
            [typed.cljc.analyzer :as ana]))

