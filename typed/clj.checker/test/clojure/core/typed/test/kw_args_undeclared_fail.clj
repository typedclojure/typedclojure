(ns ^:typed.clojure clojure.core.typed.test.kw-args-undeclared-fail
  (:require [typed.clojure :as t :refer [ann]]))

;; invoking a kw-fn with undeclared keywords is an error
(ann undeclared-kw-invoke-test [& :optional {:foo t/Any} -> nil])
(defn undeclared-kw-invoke-test [& a])

(undeclared-kw-invoke-test :blah 'a)
