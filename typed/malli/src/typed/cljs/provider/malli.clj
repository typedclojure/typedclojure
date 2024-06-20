;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljs.provider.malli
  "Automatically convert malli annotations into types."
  (:refer-clojure :exclude [requiring-resolve delay])
  (:require [typed.malli.schema-to-type :as-alias s->t]
            [typed.cljs.provider.malli-cljs :as-alias provider-cljs]
            [clojure.core.typed.runtime.jvm.configs :as configs]
            [malli.core :as m]
            [cljs.core.server]
            [cljs.repl.node]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.server :as server]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]
            [malli.instrument.cljs :as malli-instr])
  (:import [java.net Socket ServerSocket]))

(defonce register!
  (delay
    (configs/register-cljs-malli-extensions)))

#_
(defn malli->Type [m opts]
  @register!
  ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-type)
   (s->t/malli->type m opts)))

(def cljs-rdr-writer
  (delay
    (println "Starting Node prepl to retrieve Malli schemas from CLJS runtime...")
    (let [^ServerSocket server (server/start-server {:accept 'cljs.core.server/io-prepl
                                                     :address "127.0.0.1"
                                                     :port 0
                                                     :name (str `cljs-prepl-server)
                                                     :args [:repl-env (cljs.repl.node/repl-env)]})
          port (.getLocalPort server)
          socket (doto (Socket. "127.0.0.1" port)
                   (.setSoTimeout 20000))]
      [(io/reader socket)
       (io/writer socket)])))

(defn cljs-eval [exprs]
  (let [[reader writer] @cljs-rdr-writer]
    (into []
          (mapcat (fn [expr]
                    (binding [*out* writer
                              *in* reader]
                      (println (pr-str expr))
                      (loop [acc []]
                        (let [r (edn/read-string (read-line))]
                          (cond-> (conj acc r)
                            (not= :ret (:tag r)) recur))))))
          exprs)))

(comment
  (cljs-eval ['(require 'typed.malli.schema-to-type 'malli.core)
               ` (m/form (m/schema :int))])
  (cljs-eval ['(require 'typed.malli.schema-to-type)
               `(s->t/malli->type :int {::s->t/mode :validator-type})])
  (cljs-eval '[(def a 1) a])
  (cljs-eval '[(def a 1) (inc)])
  (cljs-eval "(typed.malli.schema-to-type/malli->type :int {:typed.malli.schema-to-type/mode :validator-mode})")
  (cljs-eval ['(require 'typed.cljs.provider.malli-cljs)
              `(provider-cljs/var-type-syntax `provider-cljs/var-type-syntax)])
  (cljs-eval ['(require 'typed.cljs.provider.malli-cljs)
              ;`(m/function-schemas :cljs)
              `(m/function-schemas :clj)
              ])
  )

(defn var-type [var-qsym opts]
  {:pre [(qualified-symbol? var-qsym)]}
  (let [rs (cljs-eval ['(require 'typed.cljs.provider.malli-cljs
                                 #_'malli.instrument.cljs)
                       ;`(malli-instr/-collect-all-ns)
                       `(provider-cljs/var-type-syntax '~var-qsym)])
        res (first (filter (comp #{:ret} :tag) (rseq rs)))]
    ;(prn rs)
    (if (:exception res)
      (println "Exception thrown while retrieving malli type from CLJS:" res)
      (when-some [tsyn (-> (:val res) edn/read-string)]
        ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-type)
         tsyn)))))

(comment
  (require '[clojure.core.typed.current-impl :as impl])
  (some-> (impl/with-cljs-impl (var-type `provider-cljs/var-type-syntax))
          ((requiring-resolve 'typed.clj.checker.parse-unparse/unparse-type)))
  )
