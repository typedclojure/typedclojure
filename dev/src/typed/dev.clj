(ns typed.dev
  {:clojure.tools.namespace.repl/unload false}
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.repl :refer :all]
            [typed.clojure :as t]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))

(defn detect-typed-ns [env form]
  (when (.contains (name (ns-name *ns*)) "typed")
    (println (ns-name *ns*) " contains call to clojure.core/assert: " (binding [*print-meta* true] (pr-str form)))))

(defmacro printing-assert
  ([x]
   (detect-typed-ns &env &form)
   (when *assert*
     `(when-not ~x
        (throw (new AssertionError (str "Assert failed: " (pr-str '~x)))))))
  ([x message]
   (detect-typed-ns &env &form)
   (when *assert*
     `(when-not ~x
        (throw (new AssertionError (str "Assert failed: " ~message "\n" (pr-str '~x))))))))

(alter-var-root #'assert (fn [_] @#'printing-assert))

(defonce watchers (atom []))

(defn rin-ns [sym]
  (require sym)
  (in-ns sym))

(defn watch
  ([] (watch :checker))
  ([profile]
   (let [w ((requiring-resolve 'kaocha.watch/run)
            ((requiring-resolve 'kaocha.config/load-config)
             "tests.edn"
             (cond-> {}
               profile (assoc :profile profile))))]
     (when profile
       (println (str "Watching test profile " profile " in tests.edn")))
     (swap! watchers conj w)
     w)))

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
;;hack in finally clause to work around
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
        (throw e))
      (finally
        (require 'typed.dev)))))

;; https://github.com/nrepl/piggieback#usage
(defn cljs-repl []
  ((requiring-resolve 'cljs.repl/repl)
   ((requiring-resolve 'cljs.repl.node/repl-env)))
  ;; TODO follow up on this bizzare behavior
  ;; 1. start a repl with the following code
  ;; 2. (require 'typed.ann.clojure :reload)
  ;; 3. notice that no side effects happened.
  ;;    - go to a clj REPL and eval: (keys @(clojure.core.typed.current-impl/cljs-checker))
  ;;      - no var/name env!
  ;; 4. to "fix" this, change the ns form in typed.ann.clojure from
  ;;      [typed.clojure :as t]
  ;;    to
  ;;      [typed.clojure :as t :include-macros true]
  ;; 5. side effects should be back, as if the macros were never being called previously...
  #_
  ((requiring-resolve 'cider.piggieback/cljs-repl)
   ((requiring-resolve 'cljs.repl.node/repl-env))))

(defmacro d [& body]
  (require 'clj-java-decompiler.core)
  `(clj-java-decompiler.core/decompile ~@body))
