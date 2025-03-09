(ns ^:typed.clojure clojure.core.typed.test.protocol-in-another-ns)

(defprotocol AnotherNs
  (baz [this]))
