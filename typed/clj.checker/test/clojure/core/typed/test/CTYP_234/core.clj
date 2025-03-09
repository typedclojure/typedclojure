(ns ^:typed.clojure clojure.core.typed.test.CTYP-234.core
  (:require [typed.clojure :as t]
            [clojure.core.typed.test.CTYP-234.dep :as other]))

(t/ann foo [other/MyType -> t/AnyInteger])
(defn foo
  [x]
  (:bar x))
