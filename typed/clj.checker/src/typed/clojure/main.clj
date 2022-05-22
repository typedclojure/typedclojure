;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clojure.main
  (:require [clojure.core.typed.errors :as-alias err]
            [clojure.edn :as edn]
            [clojure.tools.namespace.repl :as-alias repl]
            [nextjournal.beholder :as-alias beholder]
            [babashka.process :as-alias process]
            [typed.clojure :as t]))

(defn- dynavar [sym]
  (doto (requiring-resolve sym)
    (assert (str "Unresolable: " (pr-str sym)))))

(defn- exec1 [{:keys [dirs focus platform] :or {platform :clj}}]
  (let [platforms (cond-> platform
                    (keyword? platform) vector)]
    (assert (seq platforms) (str "Must provide at least one platform: " (pr-str platform)))
    (doseq [platform platforms]
      (case platform
        :clj (if focus
               (t/check-ns-clj focus)
               (t/check-dir-clj dirs))
        :cljs (if focus
                (t/check-ns-cljs focus)
                (t/check-dir-cljs dirs))
        (throw (ex-info (str "Unknown platform: " (pr-str platform)) {}))))))

(defn- print-error [e]
  (if (some-> (ex-data e) ((dynavar `err/top-level-error?)))
    (print (.getMessage e))
    (print e))
  (flush))

(defn- watch [{:keys [dirs refresh refresh-dirs watch-dirs] :as m}]
  (let [refresh (or refresh refresh-dirs)
        watch-dirs (concat watch-dirs dirs refresh-dirs)
        refresh-dirs (or refresh-dirs dirs)
        dirs (cond-> dirs
               (string? dirs) vector)
        _ (assert (seq dirs) "Must provide directories to scan")
        _ (when refresh
            (alter-var-root (dynavar `repl/refresh-dirs)
                            (fn [old]
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
        (let [res ((dynavar `repl/refresh))]
          (when-not (= :ok res)
            (println "[watch] refresh failed")
            (println res))))
      (recur))))

(defn exec
  "Type check namespaces. Plural options may be provided as a vector.
  
  :dirs  string(s) naming directories to find namespaces to type check
  :focus   symbol(s) naming namespaces to type check (overrides :dirs) (default: nil)
  :platform   platform(s) to check: :clj{s}  (default: :clj)
  :refresh   if true (or if :refresh-dirs option is provided) refresh with tools.namespace before rechecking (default: nil)
  :refresh-dirs   string(s) naming directories to refresh if repl/refresh-dirs is empty. (default: use :dirs)
  :watch      if true, recheck on changes to :watch-dirs.
  :watch-dirs   string(s) naming extra directories to watch to trigger rechecking. (default: use :dirs + :refresh-dirs)
  :split  a pair [this-split num-splits]. Evenly and deterministically split checkable namespaces into num-splits segments, then
          check this-split segment (zero-based). 
          eg., [0 1]  ;; check everything
               [0 2]  ;; check the first half of all namespaces
               [1 2]  ;; check the second half of all namespaces
               [2 5]  ;; check the 3rd split of 5 splits
          (default: [0 1])
  :shell-command   shell command to parallelize via :parallel option (default: \"bin/typed\")
  :parallel   evenly parallelize :exec-command to check namespaces using GNU Parallel.
              In combination with :split, assumes all splits have same parallelism."
  [{:keys [parallel shell-command] :or {shell-command "bin/typed"} :as m}]
  (cond
    parallel (let [_ (assert (pos-int? parallel) (str ":parallel must be a positive integer, given: " (pr-str)))
                   [this-split num-splits] (or (:split m) [0 1])]
               (require 'babashka.process.pprint)
               ;;end workaround
               (run! (dynavar `process/check)
                     ((dynavar `process/pipeline)
                      ((dynavar `process/pb)
                       (vec (cons "echo" (map (fn [parallel-split]
                                                (pr-str (-> m
                                                            (dissoc :parallel)
                                                            (assoc :split [(+ this-split parallel-split)
                                                                           (* num-splits parallel)]))))
                                              (range parallel)))))
                      ((dynavar `process/pb)
                       ["parallel" "--halt" "now,fail=1" (str shell-command " {}")]
                       ;; hmm this isn't working. 
                       {:out :inherit
                        :err :inherit}))))
    (:watch m) (watch m)
    :else (exec1 m)))

(defn -main
  "Same args as exec."
  [& args]
  (let [{:as m} (map edn/read-string args)]
    (try (exec m)
         (System/exit 0)
         (catch Throwable e
           (print-error e)
           (System/exit 1)))))

(comment
  (exec {:dirs "src"})
  (-main ":dirs" "[\"typed\"]")
  )
