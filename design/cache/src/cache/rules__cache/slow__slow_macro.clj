(ns cache.rules__cache.slow__slow-macro
  (:require [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.unanalyzed :refer [defuspecial]]))

(defuspecial defuspecial__slow-macro
  [expr expected]
  (println "Checking slow-macro...")
  (Thread/sleep 1000)
  (println "Checked slow-macro.")
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret r/-any)
                       expected)))
