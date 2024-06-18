(ns typed-test.cljc.checker.check.cache-test-ns1
  {:typed.clojure {:experimental #{:cache}}}
  (:require [typed.clojure :as t]))

(t/ann foo t/Int)
(def foo 1)

(t/ann bar [(t/Seqable t/Num) :-> (t/Seq t/Bool)])
(defn bar [n]
  (map zero? n))
