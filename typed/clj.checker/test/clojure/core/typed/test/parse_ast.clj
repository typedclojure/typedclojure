(ns ^:typed.clojure clojure.core.typed.test.parse-ast
  (:require [typed.clojure :as t]))

(t/ann-record Top [])
(defrecord Top [])

(t/defalias A Top)
