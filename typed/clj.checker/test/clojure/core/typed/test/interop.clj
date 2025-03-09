(ns ^:typed.clojure clojure.core.typed.test.interop
  (:import (java.io File))
  (:require [clojure.core.typed :as t]))

(t/ann f File)
(def f (File. "a"))

(t/ann prt (t/U nil t/Str))
(def prt (.getParent ^File f))

(t/non-nil-return java.io.File/getName :all)
(t/ann nme String)
(def nme (.getName ^File f))

(t/ann Class->symbol [Class -> t/Sym])
(defn Class->symbol [cls]
  (symbol (.getName ^Class cls)))
(defn Class->symbol [cls]
  (let [^Class cls cls]
    (symbol (.getName cls))))
(def Class->symbol
  (fn [^Class cls]
    (symbol (.getName cls))))
(defn Class->symbol [^Class cls]
  (symbol (.getName cls)))
