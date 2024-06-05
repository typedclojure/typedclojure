;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.datatype-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]
            [typed.clojure :as t]
            [typed.cljc.runtime.env :as env]))

(t/ann ^:no-check clojure.core.typed.errors/deprecated-warn [String -> nil])
(t/ann ^:no-check clojure.core.typed.errors/int-error [String -> t/Nothing])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datatype Env

(t/defalias DataTypeEnv
  "An Environment mapping datatype symbols to types."
  (t/Map t/Sym (t/U (t/Delay r/Type) r/Type)))

(defn datatype-env [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/current-datatype-env-kw {}))

(t/ann ^:no-check add-datatype [t/Any t/Sym r/Type -> nil])
(def add-datatype impl/add-datatype)

(t/ann get-datatype [t/Any t/Sym -> (t/U nil r/Type)])
(defn get-datatype
  "Get the datatype with class symbol sym.
  Returns nil if not found."
  [checker sym]
  {:pre [(symbol? sym)]
   :post [(or (nil? %) (r/DataType? %) (r/TypeFn? %))]}
  (force-type (get (datatype-env checker) sym)))

(t/ann resolve-datatype [t/Any t/Sym -> r/Type])
(defn resolve-datatype 
  "Same as get-datatype, but fails if datatype is not found."
  [checker sym]
  {:pre [(symbol? sym)]
   :post [(r/Type? %)]}
  (let [d (get-datatype checker sym)]
    (when-not d 
      (err/int-error (str "Could not resolve DataType: " sym)))
    d))

(t/ann reset-datatype-env! [t/Any DataTypeEnv -> DataTypeEnv])
(defn reset-datatype-env! [checker new-env]
  {:pre [(map? new-env)]
   :post [(nil? %)]}
  (env/swap-checker! checker assoc impl/current-datatype-env-kw new-env)
  nil)
