(ns bb-test-runner
  (:require
   [clojure.test :as t]
   ;;TODO more
   [typed-test.clj.analyzer]))

(defn run-tests [& _args]
  (let [{:keys [fail error]}
        (t/run-tests
         'typed-test.clj.analyzer)]
    (when (or (pos? fail)
              (pos? error))
      (System/exit 1))))
