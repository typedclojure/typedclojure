;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check-ns-common
  (:refer-clojure :exclude [requiring-resolve delay])
  (:require [clojure.core.cache :as cache]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]
            [typed.clj.checker.check :as chk-clj]
            [typed.clj.checker.file-mapping :as file-map]
            [typed.clj.checker.reset-caches :as reset-caches]
            [typed.cljc.checker.lex-env :as lex-env]
            [typed.cljc.checker.ns-deps-utils :as ns-deps-u]
            [typed.cljc.checker.reset-env :as reset-env]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.runtime.env :as env]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-utils-platform-specific :as plat-con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io])
  (:import (clojure.lang ExceptionInfo)))

;; TODO move into callers of check-ns-info
(def *register-clj-anns (delay (configs/register-clj-config-anns)))
(def *register-cljs-anns (delay (configs/register-cljs-config-anns)))

;; returns a map with keys
;; - :delayed errors    a vector of ExceptionInfo instances representing type errors
;;
;; Optional
;; - :file-mapping      a map from namespace symbols to vectors of AST nodes
;;                      Added if true :file-mapping keyword is passed as an option
(defn check-ns-info
  [impl ns-or-syms {:keys [trace file-mapping check-config max-parallelism] :as opt} opts]
  (assert (not (:opts opt)))
  (assert opts)
  (let [start (. System (nanoTime))
        threadpool vs/*check-threadpool*
        shutdown-threadpool? (not threadpool)
        ^java.util.concurrent.ExecutorService
        max-parallelism (or (when (= :available-processors max-parallelism)
                              (.. Runtime getRuntime availableProcessors))
                            max-parallelism
                            (when-some [max-parallelism (System/getProperty "typed.clojure.max-parallelism")]
                              (if (= ":available-processors")
                                (.. Runtime getRuntime availableProcessors)
                                (Integer/parseInt max-parallelism)))
                            (.. Runtime getRuntime availableProcessors))
        _ (when max-parallelism (assert (pos? max-parallelism) max-parallelism))
        threadpool (or threadpool
                       (some-> max-parallelism java.util.concurrent.Executors/newWorkStealingPool))]
    (try
      (reset-caches/reset-caches)
      (let [nsym-coll (mapv #(if (symbol? %)
                               ; namespace might not exist yet, so ns-name is not appropriate
                               ; to convert to symbol
                               %
                               (ns-name %))
                            (if ((some-fn symbol? plat-con/namespace?)
                                 ns-or-syms)
                              [ns-or-syms]
                              ns-or-syms))]
        (assert (seq nsym-coll) "Nothing to check")
        (impl/with-impl impl
          (binding [vs/*delayed-errors* (err/-init-delayed-errors)
                    vs/*already-checked* (atom #{})
                    vs/*trace-checker* trace
                    ; we only use this if we have exactly one namespace passed
                    vs/*checked-asts* (when (#{impl/clojure} impl)
                                        (when (= 1 (count nsym-coll))
                                          (atom {})))
                    vs/*lexical-env* (lex-env/init-lexical-env)
                    ;; nested check-ns inside check-form switches off check-form
                    vs/*in-check-form* false
                    vs/*check-threadpool* threadpool
                    vs/*check-config* check-config]
            (let [terminal-error (atom nil)]
              ;(reset-env/reset-envs!)
              ;(reset-caches)
              ;; handle terminal type error
              (try
                ;-------------------------
                ; Collection phase
                ;-------------------------
                (impl/impl-case opts
                  :clojure @*register-clj-anns
                  :cljs @*register-cljs-anns)
                (case (:check-ns-load check-config)
                  :require-before-check (impl/impl-case opts
                                          :clojure (locking clojure.lang.RT/REQUIRE_LOCK
                                                     (apply require nsym-coll))
                                          :cljs (err/nyi-error ":check-ns-load :require-before-check in CLJS" opts))
                  (nil :never) nil)
                ;-------------------------
                ; Check phase
                ;-------------------------
                (let [opts (impl/impl-case opts
                             :clojure ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                             :cljs ((requiring-resolve 'typed.cljs.runtime.env/cljs-opts)))
                      check-ns (impl/impl-case opts
                                 :clojure chk-clj/check-ns-and-deps
                                 :cljs    (requiring-resolve 'typed.cljs.checker.check/check-ns-and-deps))
                      check-ns #(check-ns % opts)]
                  (if (= 1 (count nsym-coll))
                    (check-ns (nth nsym-coll 0))
                    (let [check-ns (bound-fn*
                                     #(binding [vs/*delayed-errors* (err/-init-delayed-errors)]
                                        (try (check-ns %)
                                             {:delayed @vs/*delayed-errors*}
                                             (catch ExceptionInfo e
                                               (if (-> e ex-data :type-error)
                                                 {:thrown e}
                                                 (throw e))))))
                          results (if-not threadpool
                                    (mapv check-ns nsym-coll)
                                    (mapv (fn [^java.util.concurrent.Future future]
                                            (try (.get future)
                                                 (catch java.util.concurrent.ExecutionException e
                                                   (throw (or (.getCause e) e)))))
                                          (.invokeAll threadpool (map (fn [nsym]
                                                                        #(check-ns nsym))
                                                                      nsym-coll))))
                          delayed (swap! vs/*delayed-errors* into (mapcat :delayed) results)
                          terminals (some->> (some :thrown results)
                                             (reset! terminal-error))])))
                (catch ExceptionInfo e
                  (if (-> e ex-data :type-error)
                    (reset! terminal-error e)
                    (throw e))))
              (into
                {:delayed-errors (vec (concat (some-> vs/*delayed-errors* deref)
                                              (when-let [e @terminal-error]
                                                [e])))}
                (when (= impl/clojure impl)
                  (when (and file-mapping
                             (= 1 (count nsym-coll)))
                    {:file-mapping (apply merge
                                          (map #(impl/with-impl impl
                                                  (file-map/ast->file-mapping % opts))
                                               (get (some-> vs/*checked-asts* deref) (first nsym-coll))))})))))))
      (finally
        (when shutdown-threadpool?
          (some-> threadpool .shutdown))))))

(defn check-ns [impl ns-or-syms opt opts]
  (let [{:keys [delayed-errors]} (check-ns-info impl ns-or-syms opt opts)]
    (impl/with-impl impl
      (if-let [errors (seq delayed-errors)]
        (err/print-errors! errors)
        :ok))))
