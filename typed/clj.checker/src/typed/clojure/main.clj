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
            [typed.clojure :as t]))

(defn watch [{:keys [dirs platform refresh refresh-dirs] :or {platform :clj}}]
  (let [refresh (or refresh refresh-dirs)
        dirs (cond-> dirs
               (string? dirs) vector)
        _ (assert (seq dirs) "Must provide directories to scan")
        _ (when refresh
            (when (empty? repl/refresh-dirs)
              (alter-var-root #'repl/refresh-dirs (constantly (or refresh-dirs dirs)))))
        rescan (atom (promise))
        do-check #(try (case platform
                         :clj (t/check-dir-clj dirs)
                         :cljs (t/check-dir-cljs dirs))
                       (catch Throwable e
                         (println "[watch] Caught error")
                         nil))]
    (apply (requiring-resolve `beholder/watch)
           (fn [{:keys [type path]}]
             (when (contains? #{:modify :create} type)
               (deliver @rescan true)))
           dirs)
    (loop []
      (do-check)
      @@rescan
      (reset! rescan (promise))
      (when refresh
        (let [res (repl/refresh)]
        (when-not (= :ok res)
          (println "[watch] refresh failed")
          (println res))))
      (recur))))

(defn exec [{:keys [dirs platform] :or {platform :clj} :as m}]
  (if (:watch m)
    (watch m)
    (case platform
      :clj (t/check-dir-clj dirs)
      :cljs (t/check-dir-cljs dirs))))

(defn -main [& args]
  (try (exec (apply hash-map (map edn/read-string args)))
       (System/exit 0)
       (catch Throwable e
         (System/exit 1))))

(comment
  (exec {:dirs "src"})
  (-main ":dirs" "[\"typed\"]")
  )
