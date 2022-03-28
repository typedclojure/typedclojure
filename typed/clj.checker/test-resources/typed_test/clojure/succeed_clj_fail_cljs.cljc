(ns typed-test.clojure.succeed-clj-fail-cljs
  (:require [typed.clojure :as t]))

(t/ann-form #?(:clj 1 :cljs :a) t/Int)
