(ns typed.dev.kaocha-nrepl
  (:require [nrepl.cmdline :as nrepl]
            [kaocha.api :as kaocha-api]
            [kaocha.runner :as kaocha]
            [kaocha.watch :as watch]
            [clojure.string :as str]
            [clojure.java.io :as io])
  (:import [java.nio.file Path Paths]))

(def ^Path classpath-root-path (.toAbsolutePath (.toPath (io/file ""))))

(def typed-load? #_false true)

(def ^:private ^:dynamic *in-load* false)
(def ^:private ^:dynamic *kaocha-watch-try-run* nil)

(defn -main [& args]
  (future (nrepl/-main
            #_"
            Note:
              introducing other middleware makes vim-fireplace choose
              fipp for pprint, which doesn't play well with the delicately
              defined classes in type-rep."
            "--middleware" "[cider.nrepl/wrap-complete,cider.nrepl/wrap-info]"))
  (alter-var-root #'watch/try-run
                  (fn [try-run]
                    (fn [& args]
                      (binding [*kaocha-watch-try-run* (atom {})]
                        (doto (apply try-run args)
                          #_(-> keys vec (prn 'TRY_RUN)))))))
  ;;TODO create pre-hook plugin that dissociates ::testable/load-error if it is a ::fake-error
  (apply kaocha/-main args))

(comment
  (.startsWith (.toAbsolutePath (Paths/get (.toURI (io/resource "clojure/core/typed.clj"))))
               classpath-root-path )
  (do classpath-root-path)
  )
