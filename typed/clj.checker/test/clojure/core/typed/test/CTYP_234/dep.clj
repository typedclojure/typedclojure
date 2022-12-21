(ns clojure.core.typed.test.CTYP-234.dep
  (:require [typed.clojure :as t]))

(t/defalias MyType
  (t/HMap :mandatory {:foo String
                      :bar t/AnyInteger}))
