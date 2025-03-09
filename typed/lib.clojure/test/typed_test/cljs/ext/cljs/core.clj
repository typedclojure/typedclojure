(ns ^:typed.clojure ^:no-doc typed-test.cljs.ext.cljs.core
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.template :refer [do-template]]
            [typed.clojure :as t]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljs.checker.test-utils :refer [is-tc-e is-tc-err]]))

(deftest implements?-test
  (is-tc-e (do (defprotocol A)
               (cljs.core/implements? A 2)))
  (is-tc-err (do (defprotocol A)
                 (cljs.core/implements? A (typed.clojure/ann-form 1 nil))))
  (is-tc-e (do (defprotocol A)
               (cljs.core/implements? A 1))
           typed.clojure/Bool)
  (is-tc-err (do (defprotocol A)
                 (cljs.core/implements? A 1))
             nil))
