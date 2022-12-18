;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clojure.main
  (:require [clojure.core.typed.errors :as err]
            [clojure.edn :as edn]
            [clojure.tools.namespace.repl :as-alias repl]
            [nextjournal.beholder :as-alias beholder]
            [babashka.process :as-alias process]
            [typed.clojure :as t]
            [typed.cljc.dir :as tdir]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.ns-deps-utils :as ns-depsu]))

(defn- dynavar [sym]
  (doto (requiring-resolve sym)
    (assert (str "Unresolable: " (pr-str sym)))))

; Algorithm By Mark Dickinson https://stackoverflow.com/a/2660138
(defn- partition-fairly
  "Partition coll into n chunks such that each chunk's
  count is within one of eachother. Puts its larger chunks first.
  Returns a vector of chunks (vectors)."
  [n coll]
  {:pre [(vector? coll)
         (integer? n)]
   :post [(or (empty? %)
              (let [fc (count (first %))]
                (every? #{fc (dec fc)} (map count %))))
          (= coll (apply concat %))]}
  ;TODO make lazier (use partition with overlapping steps to iterate
  ; over `indices`)
  (let [q (quot (count coll) n)
        r (rem (count coll) n)
        indices (mapv #(+ (* q %)
                          (min % r))
                      (range (inc n)))]
    (mapv #(subvec coll
                   (indices %)
                   (indices (inc %)))
          (range n))))

(defn- nses-for-this-split [[this-split num-splits] nses]
  (nth (partition-fairly num-splits nses) this-split))

(defn- print-error [^Throwable e]
  (if (some-> (ex-data e) err/top-level-error?)
    (print (.getMessage e))
    (print e))
  (flush))

(defn- exec1 [{:keys [split dirs focus platform watch] :or {platform :clj split [0 1]}}]
  (let [focus (cond-> focus
                (symbol? focus) vector)
        platforms (sort (cond-> platform
                          (keyword? platform) vector))
        _ (assert (seq platforms) (str "Must provide at least one platform: " (pr-str platform)))
        plan (mapcat (fn [platform]
                       (map (fn [nsym]
                              {:platform platform 
                               :nsym nsym})
                            (nses-for-this-split
                              split
                              (or focus
                                  (:nses
                                    (impl/with-impl (case platform
                                                      :clj :clojure
                                                      :cljs :cljs)
                                      (tdir/check-dir-plan dirs)))))))
                  platforms)]
    (assert (seq plan) "No namespaces to check")
    (reduce (fn [acc {:keys [platform nsym] :as info}]
              {:pre [(map? info)
                     (keyword? platform)
                     (simple-symbol? nsym)
                     (= :ok (:result acc))]}
              (let [chk #((case platform
                            :clj t/check-ns-clj
                            :cljs t/check-ns-cljs)
                          nsym)
                    res (try (chk)
                             (assoc info :result :ok)
                             (catch Throwable e
                               (assoc info :result :fail :ex e)))
                    acc (-> acc
                            (update :checks (fnil conj []) res)
                            (cond-> (not= :ok (:result res)) (into res)))]
                (cond-> acc
                  (not= :ok (:result acc)) reduced)))
            {:result :ok} plan)))

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
        watcher (apply (dynavar `beholder/watch)
                       (fn [{:keys [type path]}]
                         (when (contains? #{:modify :create} type)
                           (deliver @rescan true)))
                       watch-dirs)]
    (try (loop [last-result (exec1 m)]
           (when (= :fail (:result last-result))
             (println "[watch] Caught error")
             (print-error (:ex last-result))
             (println))
           (reset! rescan (promise))
           @@rescan
           ;; TODO easy special case: don't refresh if the scan-dirs says only the currently focussed (ie., failing) namespace changed
           (when refresh
             (let [res ((dynavar `repl/refresh-all))] ;; refresh-all to undo check-ns making namespaces stale
               (when-not (= :ok res)
                 (println "[watch] refresh failed")
                 (println res))))
           (recur (exec1 (cond-> m
                           (and (= :fail (:status last-result))
                                ;; don't focus if deleted
                                (ns-depsu/should-check-ns? (:nsym last-result)))
                           (assoc :focus (:nsym last-result)
                                  :platform (:platform last-result))))))
         (finally ((dynavar `beholder/stop) watcher)))))

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
    :else (let [{:keys [result ex]} (exec1 m)]
            (if (= :fail result)
              (throw ex)
              result))))

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
