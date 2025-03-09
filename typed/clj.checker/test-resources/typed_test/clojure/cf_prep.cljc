(ns ^:typed.clojure typed-test.clojure.cf-prep
  (:require [typed.clojure :as t]))

(t/ann int->clj-bool->cljs #?(:clj t/Int :cljs t/Bool))
(def int->clj-bool->cljs #?(:clj 1 :cljs true))
