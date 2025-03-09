(ns ^:typed.clojure typed.clj.spec.protocols)

(defprotocol ITypeFn
  (apply-typefn* [this arg-map]))

(defprotocol IPoly
  (inst* [this arg-map]))
