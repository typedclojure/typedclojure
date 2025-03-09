(ns ^:typed.clojure clojure.core.typed.test.binding
  (:require [typed.clojure :as t]))

(t/def ^:dynamic *foo* :- t/Bool, true)
(binding [*foo* false])
