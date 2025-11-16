(ns bb-test-runner
  (:require
   [clojure.test :as t]
   [typed.fnl.analyzer-test]))

(defn run-tests [& _args]
  (let [{:keys [fail error]}
        (t/run-tests
         'typed.fnl.analyzer-test)]
    (when (or (pos? fail)
              (pos? error))
      (System/exit 1))))
