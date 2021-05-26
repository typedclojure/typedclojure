(ns gen-doc
  (:require [codox.main :as codox]))

(defn -main [& args]
  (codox/generate-docs
    {:language :clojure
     :source-uri "https://github.com/typedclojure/typedclojure/blob/{git-commit}/typed.clj.spec/{filepath}#L{line}"
     :output-path "target/codox"})
  (shutdown-agents)
  (System/exit 0))
