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
            [clojure.string :as str]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]
            [typed.clj.checker.check :as chk-clj]
            [typed.clj.checker.file-mapping :as file-map]
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
  (when trace
    (assert (= "true" (System/getProperty "typed.cljc.checker.utils.trace"))
            "To enable tracing set system property -Dtyped.cljc.checker.utils.trace=true on startup"))
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
                       (when (some-> max-parallelism (> 1))
                         (java.util.concurrent.Executors/newWorkStealingPool max-parallelism)))]
    (try
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
        (assert (not (::vs/delayed-errors opts)))
        (impl/with-impl impl
          (binding [;; nested check-ns inside check-form switches off check-form
                    vs/*in-check-form* false
                    vs/*check-threadpool* threadpool]
            (let [delayed-errors (err/-init-delayed-errors)
                  opts (-> opts
                           (assoc ::vs/check-config check-config)
                           (assoc ::vs/lexical-env (lex-env/init-lexical-env))
                           (assoc ::vs/already-checked (atom #{}))
                           (assoc ::vs/delayed-errors delayed-errors)
                           (assoc ::vs/trace trace))
                  terminal-error (atom nil)]
              ;(reset-env/reset-envs!)
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
                (let [check-ns (impl/impl-case opts
                                 :clojure chk-clj/check-ns-and-deps
                                 :cljs    (requiring-resolve 'typed.cljs.checker.check/check-ns-and-deps))]
                  (if (= 1 (count nsym-coll))
                    (check-ns (nth nsym-coll 0) opts)
                    (let [check-ns (bound-fn*
                                     #(let [delayed-errors (err/-init-delayed-errors)
                                            ex (volatile! nil)
                                            chk (fn [] (try (check-ns % (assoc opts ::vs/delayed-errors delayed-errors))
                                                            (catch Throwable e (vreset! ex e))))
                                            out (if threadpool
                                                  (with-out-str (chk))
                                                  (do (chk) nil))]
                                        (-> (if-let [ex @ex]
                                              (if (-> ex ex-data :type-error)
                                                {:errors (conj @delayed-errors ex)}
                                                {:ex ex})
                                              {:errors @delayed-errors})
                                            (assoc :out out))))
                          results (if-not threadpool
                                    (mapv check-ns nsym-coll)
                                    (mapv (fn [^java.util.concurrent.Future future]
                                            (try (.get future)
                                                 (catch java.util.concurrent.ExecutionException e
                                                   (throw (or (.getCause e) e)))))
                                          (.invokeAll threadpool (map (fn [nsym]
                                                                        #(check-ns nsym))
                                                                      nsym-coll))))
                          _ (swap! delayed-errors into (mapcat (fn [{:keys [ex errors out]}]
                                                                 (some-> out str/trim not-empty println)
                                                                 (some-> ex throw)
                                                                 errors))
                                   results)])))
                (catch ExceptionInfo e
                  (if (-> e ex-data :type-error)
                    (reset! terminal-error e)
                    (throw e))))
              {:delayed-errors (vec (concat @delayed-errors
                                            (when-let [e @terminal-error]
                                              [e])))}))))
      (finally
        (when shutdown-threadpool?
          (some-> threadpool .shutdown))))))

(defn check-ns [impl ns-or-syms opt opts]
  (let [{:keys [delayed-errors]} (check-ns-info impl ns-or-syms opt opts)]
    (impl/with-impl impl
      (if-let [errors (seq delayed-errors)]
        (err/print-errors! errors)
        :ok))))
