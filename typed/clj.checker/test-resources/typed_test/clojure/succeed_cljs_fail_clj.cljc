(ns ^:typed.clojure typed-test.clojure.succeed-cljs-fail-clj
  (:require [typed.clojure :as t]))

(t/ann-form #?(:clj :a :cljs 1) t/Int)
