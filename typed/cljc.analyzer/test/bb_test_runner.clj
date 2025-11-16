(ns bb-test-runner
  (:require
   [clojure.test :as t]
   [typed-test.cljc.analyzer]))

(defn run-tests [& _args]
  (let [{:keys [fail error]}
        (t/run-tests
         'typed-test.cljc.analyzer)]
    (when (or (pos? fail)
              (pos? error))
      (System/exit 1))))
