(ns typed.dev.kaocha-nrepl
  (:require [nrepl.cmdline :as nrepl]
            [kaocha.api :as kaocha-api]
            [kaocha.runner :as kaocha]
            [kaocha.watch :as watch]
            [clojure.string :as str]
            [clojure.core.typed.load1 :as load1]
            [clojure.java.io :as io])
  (:import [java.nio.file Path Paths]))

(def ^Path classpath-root-path (.toAbsolutePath (.toPath (io/file ""))))

(def typed-load? false #_true)

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
  (alter-var-root #'load
                  (fn [load]
                    (fn [& args]
                      (doseq [base-resource-path args]
                        ;(prn `main base-resource-path)
                        (or (when-not *in-load*
                              (binding [*in-load* true]
                                (when (and @(resolve `typed-load?)
                                           kaocha-api/*active?*
                                           *kaocha-watch-try-run*)
                                  (when (str/starts-with? base-resource-path "/")
                                    (when-some [[^java.net.URL resource filename] (load1/base-resource-path->resource (subs base-resource-path 1))]
                                      ;(prn `main resource filename (.toURI resource))
                                      (when-some [path (try (some-> (.toURI resource) Paths/get .toAbsolutePath)
                                                            (catch java.nio.file.FileSystemNotFoundException _))]
                                        ;(prn `main :path path)
                                        (when (.startsWith path classpath-root-path)
                                          ;(prn `main 'starts-with classpath-root-path)
                                          (load1/load-typed-file
                                            (subs base-resource-path 1)
                                            nil
                                            ;; just print the first type error in the first file
                                            {:skip-check-form? (fn [e]
                                                                 (boolean (::printed-type-error @*kaocha-watch-try-run*)))
                                             :ex-handler (fn [e]
                                                           (when-not (::printed-type-error
                                                                       (first (swap-vals! *kaocha-watch-try-run*
                                                                                          assoc ::printed-type-error true)))
                                                             ;(println "PRINTING!!!")
                                                             (println (ex-message e))
                                                             (throw (ex-info "Type error" {::fake-error true}))))})
                                          true)))))))
                            (load base-resource-path))))))
  ;;TODO create pre-hook plugin that dissociates ::testable/load-error if it is a ::fake-error
  (apply kaocha/-main args))

(comment
  (.startsWith (.toAbsolutePath (Paths/get (.toURI (io/resource "clojure/core/typed.clj"))))
               classpath-root-path )
  (do classpath-root-path)
  )
