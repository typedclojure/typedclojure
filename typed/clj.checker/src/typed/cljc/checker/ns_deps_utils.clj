;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc ^:typed.clojure/ignore
  typed.cljc.checker.ns-deps-utils
  (:require [clojure.tools.namespace.parse :as ns-parse]
            [clojure.tools.namespace.file :as ns-file]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.internal :as internal]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn ns-form-for-file
  "Returns the namespace declaration for the file, or
  nil if not found"
  [file opts]
  (some-> (io/resource file)
          (ns-file/read-file-ns-decl
            (impl/impl-case opts
              :clojure ns-parse/clj-read-opts
              :cljs ns-parse/cljs-read-opts))))

(defn ns-form-for-ns
  "Returns the namespace declaration for the namespace, or
  nil if not found."
  [nsym opts]
  {:pre [(symbol? nsym)]}
  (-> nsym (coerce/ns->file opts) (ns-form-for-file opts)))

(defn ns-form-deps
  "Given a ns-form, returns a set of dependencies"
  [ns-form]
  {:pre [ns-form]
   :post [((con/set-c? symbol?) %)]}
  (let [ndeps (ns-parse/deps-from-ns-decl ns-form)]
    ;; tools.namespace can return nil here
    (set ndeps)))

(defn deps-for-ns
  "Returns the dependencies for a namespace"
  [nsym opts]
  {:pre [(symbol? nsym)]
   :post [(set? %)]}
  (if-let [ns-form (ns-form-for-ns nsym opts)]
    (ns-form-deps ns-form)
    #{}))

(defn ns-form-name
  "Returns the symbol naming this namespace, with any
  metadata attached."
  [ns-form opts]
  {:post [(symbol? %)]}
  (let [ns-form (next ns-form)
        [nsym ns-form] (internal/take-when symbol? ns-form)
        _ (when-not (symbol? nsym)
            (err/int-error "Malformed ns form" opts))
        [docstr ns-form]  (internal/take-when string? ns-form)
        [metamap ns-form] (internal/take-when map? ns-form)]
    (if (map? metamap)
      (vary-meta nsym merge metamap)
      nsym)))

(defn ns-meta
  "Returns the metadata map for this namespace"
  [ns-form opts]
  (meta (ns-form-name ns-form opts)))

(defn ignore-ns? [ns-form opts]
  (let [nmeta (ns-meta ns-form opts)]
    (or (true? (:typed.clojure/ignore nmeta))
        (-> nmeta :typed.clojure :ignore true?))))

(def fixup? (= "true" (System/getProperty "typed.clojure.preserve-check-ns-after-opt-in")))

(defn fixup-ns [nsym opts]
  (if-some [f (-> nsym (coerce/ns->URL opts))]
    (let [s (slurp f)]
      (spit f
            (cond
              (str/includes? s "(ns ") (str/replace-first s "(ns " "(ns ^:typed.clojure ")
              (str/includes? s "(ns\n") (str/replace-first s "(ns\n" "(ns\n  ^:typed.clojure\n")
              :else (throw (ex-info (str "Could not fix :typed.clojure meta for " nsym " (could not find ns form)") {}))))
      true)
    (do (println (str "WARNING: Could not fix :typed.clojure meta for " nsym " (could not find file)"))
        false)))

(defn check-ns? [ns-form opts]
  (let [nmeta (ns-meta ns-form opts)]
    (if (ignore-ns? ns-form opts)
      false
      (if-some [[_ v] (find nmeta :typed.clojure)]
        (boolean v)
        (if fixup?
          (fixup-ns (second ns-form) opts)
          (do (println (str "WARNING: " (second ns-form) " not being checked, consider setting -Dtyped.clojure.preserve-check-ns-after-opt-in=true"))
              false))))))

(defn should-check-ns-form?
  [ns-form opts]
  {:post [(boolean? %)]}
  (and (boolean ns-form)
       (check-ns? ns-form opts)))

(defn should-check-ns?
  "Returns true if the given namespace should be type checked"
  [nsym opts]
  {:pre [(symbol? nsym)]
   :post [(boolean? %)]}
  (should-check-ns-form? (ns-form-for-ns nsym opts) opts))
