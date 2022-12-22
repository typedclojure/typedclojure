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
            [clojure.tools.namespace.find :as find]
            [clojure.tools.namespace.track :as track]
            [typed.cljc.checker.ns-deps-utils :as ns-deps-u]))

(defn check-dir-plan
  ([dirs] (check-dir-plan (track/tracker) dirs))
  ([tracker dirs]
   {:post [(:tracker %)
           (vector? (:nses %))]}
   (let [dirs (cond-> dirs
                (string? dirs) vector)
         _ (assert (seq dirs) "Must provide one or more directories")
         {::track/keys [deps] ::dir/keys [files] ::file/keys [filemap] :as tracker} (-> (or tracker (track/tracker))
                                                                                        (dir/scan-dirs 
                                                                                          dirs
                                                                                          {:platform (impl/impl-case
                                                                                                       :clojure find/clj
                                                                                                       :cljs find/cljs)}))
         _ (assert (seq files) (str "No files found in " (pr-str dirs)))
         nses (into [] (filter (every-pred (set (vals filemap))
                                           ns-deps-u/should-check-ns?))
                    (dep/topo-sort deps))]
     {:tracker tracker
      :nses nses})))

(defn check-dir* [dirs]
  (let [{:keys [nses]} (check-dir-plan dirs)]
    (println "Type checking namespaces:" nses)
    ((impl/impl-case
       :clojure t/check-ns-clj
       :cljs t/check-ns-cljs)
     nses)))

(defn check-dir-clj [dirs]
  (impl/with-clojure-impl
    (check-dir* dirs)))

(defn check-dir-cljs [dirs]
  (impl/with-cljs-impl
    (check-dir* dirs)))

(comment
  (impl/with-clojure-impl
    (check-dir-plan "typed/clj.checker/src"))
  (check-dir-clj "typed/clj.checker/src")
  )
