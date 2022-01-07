(ns typed.dev.repl
  (:require [clojure.main :as m]))

(defn -main [& args]
  (m/repl
    :init (fn []
            (apply require m/repl-requires)
            (doto 'typed.dev
              require
              in-ns))))
