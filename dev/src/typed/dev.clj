(ns typed.dev
  {:clojure.tools.namespace.repl/unload false})

(defonce watchers (atom []))

(defn watch []
  (let [w ((requiring-resolve 'kaocha.watch/run)
           (assoc ((requiring-resolve 'kaocha.config/load-config))
                  :skip-meta :typed/skip-from-repo-root))]
    (swap! watchers conj w)
    w))

(defn stop-watchers []
  (mapv (fn [[_fut cancel-fn]]
          (cancel-fn))
        (first (reset-vals! watchers []))))

(defonce tracker (atom nil))

;;https://github.com/metosin/bat-test/blob/847b6bcaed754cfeadcc1b186e9c0eb162bc308c/src/metosin/bat_test/impl.clj
(defn load-only-loaded-ns
  [tracker]
  (update tracker :clojure.tools.namespace.track/load
          #(doall (filter (fn [nss]
                            (find-ns nss))
                          %))))

;; only refresh loaded namespaces
;;https://github.com/metosin/bat-test/blob/847b6bcaed754cfeadcc1b186e9c0eb162bc308c/src/metosin/bat_test/impl.clj
;;FIXME :clojure.tools.namespace.repl/unload is broken so this ns keeps losing its tracker
(defn refresh []
  (let [watch-directories (filter
                            (fn [^String p]
                              ;; TODO generalize pattern, perhaps via deps.edn alias convention
                              ;; don't reload test resources
                              (not= "test-resources" (.getName (java.io.File. p))))
                            (into ((requiring-resolve 'typed.dev.merge-deps/src-paths))
                                  ((requiring-resolve 'typed.dev.merge-deps/test-paths))))
        _ (swap! tracker (fn [tracker]
                           (print (format "Scan directories: %s\n" (pr-str watch-directories)))
                           ((requiring-resolve 'clojure.tools.namespace.dir/scan-dirs)
                            (or tracker ((requiring-resolve 'clojure.tools.namespace.track/tracker)))
                            watch-directories)))
        changed-ns (:clojure.tools.namespace.track/load @tracker)]
    (swap! tracker load-only-loaded-ns)
    ;(print (format "Unload: %s\n" (pr-str (:clojure.tools.namespace.track/unload @tracker))))
    (print (format "Load: %s\n" (pr-str (:clojure.tools.namespace.track/load @tracker))))
    (swap! tracker (requiring-resolve 'clojure.tools.namespace.reload/track-reload))
    (try
      (when (:clojure.tools.namespace.reload/error @tracker)
        (print (format "Error reloading: %s\n" (name (:clojure.tools.namespace.reload/error-ns @tracker))))
        (throw (:clojure.tools.namespace.reload/error @tracker)))
      (catch java.io.FileNotFoundException e
        (print "Reseting tracker due to file not found exception, all namespaces will be reloaded next time.\n")
        (reset! tracker ((requiring-resolve 'clojure.tools.namespace.track/tracker)))
        (throw e)))))

;; https://github.com/nrepl/piggieback#usage
(defn cljs-repl []
  ((requiring-resolve 'cider.piggieback/cljs-repl)
   ((requiring-resolve 'cljs.repl.node/repl-env))))
