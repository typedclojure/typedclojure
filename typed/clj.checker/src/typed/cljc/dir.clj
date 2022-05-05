;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.dir
  (:require [typed.clojure :as t]
            [typed.cljc.checker.ns-deps-utils :as ndepsu]
            [clojure.core.typed.current-impl :as impl]
            [clojure.tools.namespace.dependency :as dep]
            [clojure.tools.namespace.dir :as dir]
            [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.track :as track]))

(defn check-dir* [dirs]
  (let [dirs (cond-> dirs
               (string? dirs) vector)
        _ (assert (seq dirs) "Must provide one or more directories")
        {::track/keys [deps] ::dir/keys [files] ::file/keys [filemap] :as tracker} (dir/scan-dirs (track/tracker) dirs)
        _ (assert (seq files) (str "No files found in " (pr-str dirs)))
        nses (into [] (filter (set (vals filemap)))
                   (dep/topo-sort deps))]
    (t/check-ns-clj nses)))

(defn check-dir-clj [dirs]
  (impl/with-clojure-impl
    (check-dir* dirs)))

(comment
  (check-dir-clj "typed/clj.checker/src")
  )
