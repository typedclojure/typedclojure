;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clojure.main
  (:require [clojure.edn :as edn]
            [clojure.tools.namespace.repl :as repl]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.track :as track]
            [nextjournal.beholder :as-alias beholder]
            [clojure.core.typed.errors :as err]
            [typed.clojure :as t]))

(defn- exec1 [{:keys [dirs focus platform] :or {platform :clj}}]
  (case platform
    :clj (if focus
           (t/check-ns-clj focus)
           (t/check-dir-clj dirs))
    :cljs (if focus
           (t/check-ns-cljs focus)
           (t/check-dir-cljs dirs))))

(defn- print-error [e]
  (if (some-> (ex-data e) err/top-level-error?)
    (print (.getMessage e))
    (print e))
  (flush))

(defn- watch [{:keys [dirs platform refresh refresh-dirs watch-dirs] :or {platform :clj} :as m}]
  (let [refresh (or refresh refresh-dirs)
        watch-dirs (concat watch-dirs dirs refresh-dirs)
        refresh-dirs (or refresh-dirs dirs)
        dirs (cond-> dirs
               (string? dirs) vector)
        _ (assert (seq dirs) "Must provide directories to scan")
        _ (when refresh
            (alter-var-root #'repl/refresh-dirs (fn [old]
                                                  (or (not-empty old)
                                                      refresh-dirs))))
        rescan (atom (promise))
        do-check #(try (exec1 m)
                       (catch Throwable e
                         (println "[watch] Caught error")
                         (print-error e)
                         (println)
                         nil))]
    (apply (requiring-resolve `beholder/watch)
           (fn [{:keys [type path]}]
             (when (contains? #{:modify :create} type)
               (deliver @rescan true)))
           watch-dirs)
    (loop []
      (do-check)
      (reset! rescan (promise))
      @@rescan
      (when refresh
        (let [res (repl/refresh)]
        (when-not (= :ok res)
          (println "[watch] refresh failed")
          (println res))))
      (recur))))

(defn exec [m]
  (if (:watch m)
    (watch m)
    (exec1 m)))

(defn -main [& args]
  (try (exec (apply hash-map (map edn/read-string args)))
       (System/exit 0)
       (catch Throwable e
         (print-error e)
         (System/exit 1))))

(comment
  (exec {:dirs "src"})
  (-main ":dirs" "[\"typed\"]")
  )
