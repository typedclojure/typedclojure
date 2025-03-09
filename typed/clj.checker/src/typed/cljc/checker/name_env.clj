;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.name-env
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.clojure :as t]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.clj.checker.rclass-env :as rcls]
            [typed.cljc.checker.datatype-env :as dtenv]
            [typed.cljc.checker.declared-kind-env :as kinds]
            [typed.cljc.checker.protocol-env :as prenv]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env :as env]))

(t/defalias NameEnv
  "Environment mapping names to types. Keyword values are special."
  (t/Map t/Sym (t/U t/Kw r/Type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Name Env

(t/ann temp-binding t/Kw)
(def temp-binding ::temp-binding)

(t/tc-ignore
  (doseq [k [impl/declared-name-type impl/protocol-name-type impl/datatype-name-type]]
    (derive k temp-binding)))

(t/ann ^:no-check name-env? [t/Any -> t/Any])
(def name-env? (con/hash-c? (every-pred (some-fn namespace 
                                                 #(some #{\.} (str %)))
                                        symbol?)
                            (some-fn r/Type? #(isa? % temp-binding))))

(t/ann ^:no-check name-env [t/Any -> NameEnv])
(defn name-env [checker]
  (get (env/deref-checker checker) impl/current-name-env-kw {}))

(t/ann ^:no-check reset-name-env! [t/Any NameEnv -> nil])
(defn reset-name-env! [checker nme-env]
  (env/swap-checker! checker assoc impl/current-name-env-kw nme-env)
  nil)

(t/ann ^:no-check find-type-name-entry [t/Sym t/Any -> (t/Nilable (t/MapEntry t/Sym (t/U t/Kw r/Type)))])
(defn find-type-name-entry [sym opts]
  (let [env (name-env (env/checker opts))]
    (some->
      (or (find env sym)
          (when-some [sym-nsym (some-> (namespace sym) symbol
                                       ((requiring-resolve
                                          (impl/impl-case opts
                                                          :clojure 'typed.clj.checker.parse-unparse/ns-rewrites-clj
                                                          :cljs 'typed.clj.checker.parse-unparse/ns-rewrites-cljs))))]
            (find env (symbol (name sym-nsym) (name sym)))))
      (update 1 #(force-type % opts)))))

(t/ann ^:no-check get-type-name* [t/Sym t/Any -> (t/Nilable (t/U t/Kw (t/Delay r/Type) [:-> r/Type]))])
(defn- get-type-name*
  "Does not resolve type."
  [sym opts]
  (let [env (name-env (env/checker opts))]
    (or (env sym)
        (when-some [sym-nsym (some-> (namespace sym) symbol
                                     ((requiring-resolve
                                        (impl/impl-case opts
                                          :clojure 'typed.clj.checker.parse-unparse/ns-rewrites-clj
                                          :cljs 'typed.clj.checker.parse-unparse/ns-rewrites-cljs))))]
          (env (symbol (name sym-nsym) (name sym)))))))

(t/ann ^:no-check get-type-name [t/Sym t/Any -> (t/U nil t/Kw r/Type)])
(defn get-type-name
  "Return the (forced) type mapped to var with symbol sym.
  Returns nil if not found."
  [sym opts]
  {:pre [(symbol? sym)]
   :post [(or (nil? %)
              (keyword? %)
              (r/Type? %))]}
  (some-> (get-type-name* sym opts) (force-type opts)))

(t/ann ^:no-check add-type-name [t/Any t/Sym (t/U t/Kw r/Type) -> nil])
(def add-type-name impl/add-tc-type-name)

(t/ann ^:no-check declare-name* [t/Sym -> nil])
(def declare-name* impl/declare-name*)

(t/ann ^:no-check declared-name? [t/Sym t/Any -> t/Bool])
(defn declared-name? [sym opts]
  (= impl/declared-name-type (get-type-name sym opts)))

(t/ann ^:no-check declare-protocol* [t/Any t/Sym -> nil])
(def declare-protocol* impl/declare-protocol*)

(t/ann ^:no-check declared-protocol? [t/Sym t/Any -> t/Bool])
(defn declared-protocol? [sym opts]
  (= impl/protocol-name-type (get-type-name sym opts)))

(t/ann ^:no-check declare-datatype* [t/Any t/Sym -> nil])
(def declare-datatype* impl/declare-datatype*)

(t/ann ^:no-check declared-datatype? [t/Sym t/Any -> t/Bool])
(defn declared-datatype? [sym opts]
  (= impl/datatype-name-type (get-type-name sym opts)))

(t/ann ^:no-check resolve-name* [t/Sym t/Any -> r/Type])
(defn resolve-name* [sym opts]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (let [checker (env/checker opts)
        t (get-type-name sym opts)]
    (or (dtenv/get-datatype checker sym opts)
        (prenv/get-protocol checker sym opts)
        (impl/impl-case opts
          :clojure (or (rcls/get-rclass checker sym opts)
                       (when (class? (resolve sym))
                         (c/RClass-of-with-unknown-params sym opts)))
          :cljs ((requiring-resolve 'typed.cljs.checker.jsnominal-env/get-jsnominal) sym opts))
        ; during the definition of RClass's that reference
        ; themselves in their definition, a temporary TFn is
        ; added to the declared kind env which is enough to determine
        ; type rank and variance.
        (kinds/declared-kind-or-nil checker sym opts)
        (cond
          (= impl/protocol-name-type t) (prenv/resolve-protocol checker sym opts)
          (= impl/datatype-name-type t) (dtenv/resolve-datatype checker sym opts)
          (= impl/declared-name-type t) (throw (IllegalArgumentException. (str "Reference to declared but undefined name " sym)))
          (r/Type? t) (with-meta t (assoc (meta t) :source-Name sym))
          :else (err/int-error (str "Cannot resolve name " (pr-str sym)
                                    (when t
                                      (str " (Resolved to instance of)" (pr-str (class t)))))
                               opts)))))
