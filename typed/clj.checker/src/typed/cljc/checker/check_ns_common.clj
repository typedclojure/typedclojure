;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check-ns-common
  (:require [clojure.core.cache :as cache]
            [clojure.core.typed :as t]
            [typed.clj.checker.check :as chk-clj]
            [typed.clj.checker.file-mapping :as file-map]
            [typed.clj.checker.reset-caches :as reset-caches]
            [typed.cljc.checker.lex-env :as lex-env]
            [typed.cljc.checker.ns-deps-utils :as ns-deps-u]
            [typed.cljc.checker.reset-env :as reset-env]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.var-env :as var-env]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.java.io :as io])
  (:import (clojure.lang ExceptionInfo)))

(defn cljs-reader [nsym]
  (let [f ((requiring-resolve 'cljs.util/ns->relpath) nsym)
        res (if (re-find #"^file://" f) (java.net.URL. f) (io/resource f))]
    (assert res (str "Can't find " f " in classpath"))
    (io/reader res)))

;; returns a map with keys
;; - :delayed errors    a vector of ExceptionInfo instances representing type errors
;;
;; Optional
;; - :file-mapping      a map from namespace symbols to vectors of AST nodes
;;                      Added if true :file-mapping keyword is passed as an option
(defn check-ns-info
  [impl ns-or-syms {:keys [collect-only trace file-mapping check-config] :as opt}]
  (do
    (let [start (. System (nanoTime))]
      (reset-caches/reset-caches)
      (let [nsym-coll (map #(if (symbol? %)
                              ; namespace might not exist yet, so ns-name is not appropriate
                              ; to convert to symbol
                              %
                              (ns-name %))
                           (if ((some-fn symbol? con/namespace?)
                                ns-or-syms)
                             [ns-or-syms]
                             ns-or-syms))]
        (impl/with-impl impl
          (binding [vs/*delayed-errors* (err/-init-delayed-errors)
                    vs/*already-checked* (atom #{})
                    vs/*trace-checker* trace
                    ; we only use this if we have exactly one namespace passed
                    vs/*checked-asts* (when (#{impl/clojure} impl)
                                        (when (== 1 (count nsym-coll))
                                          (atom {})))
                    vs/*lexical-env* (lex-env/init-lexical-env)
                    ;; nested check-ns inside check-form switches off check-form
                    vs/*in-check-form* false]
            (let [terminal-error (atom nil)]
              ;(reset-env/reset-envs!)
              ;(reset-caches)
              ;; handle terminal type error
              (try
                ;-------------------------
                ; Check phase
                ;-------------------------
                (when-not collect-only
                  (let [check-ns (impl/impl-case
                                   :clojure #(binding [vs/*check-config* (atom check-config)]
                                               (chk-clj/check-ns-and-deps %))
                                   :cljs    (requiring-resolve 'typed.cljs.checker.check/check-ns-and-deps))]
                    (doseq [nsym nsym-coll]
                      (check-ns nsym))))
                (catch ExceptionInfo e
                  (if (-> e ex-data :type-error)
                    (reset! terminal-error e)
                    (throw e))))
              (merge
                {:delayed-errors (vec (concat (some-> vs/*delayed-errors* deref)
                                              (when-let [e @terminal-error]
                                                [e])))}
                (when (#{impl/clojure} impl)
                  (when (and file-mapping
                             (== 1 (count nsym-coll)))
                    {:file-mapping (apply merge
                                          (map #(impl/with-impl impl
                                                  (file-map/ast->file-mapping %))
                                               (get (some-> vs/*checked-asts* deref) (first nsym-coll))))}))))))))))

(defn check-ns
  ([impl ns-or-syms opt]
   (let [{:keys [delayed-errors]} (check-ns-info impl ns-or-syms opt)]
     (impl/with-impl impl
       (if-let [errors (seq delayed-errors)]
         (err/print-errors! errors)
         :ok)))))
