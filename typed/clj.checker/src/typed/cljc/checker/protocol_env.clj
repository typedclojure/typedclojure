;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.protocol-env
  (:require [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-ann]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.errors-ann]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env :as env]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protocol Env

(t/defalias ProtocolEnv 
  "A map mapping protocol symbols their types."
  (t/Map t/Sym r/Type))

(t/ann protocol-env? [t/Any -> t/Any])
(def protocol-env? (con/hash-c? (every-pred symbol? namespace)
                                (some-fn delay? r/Protocol? r/TypeFn?)))

(t/ann ^:no-check protocol-env [t/Any -> ProtocolEnv])
(defn protocol-env [checker]
  {:post [(map? %)
          #_(protocol-env? %)]}
  (get (env/deref-checker checker) impl/current-protocol-env-kw {}))

(t/ann ^:no-check reset-protocol-env! [ProtocolEnv -> nil])
(defn reset-protocol-env! [checker e]
  {:pre [#_(protocol-env? e)]}
  (env/swap-checker! checker assoc impl/current-protocol-env-kw e)
  nil)

(defn merge-protocol-env! [checker e]
  {:pre [(map? e)]}
  (env/swap-checker! checker update impl/current-protocol-env-kw merge e)
  nil)

(t/ann ^:no-check add-protocol [t/Any t/Sym r/Type -> nil])
(def add-protocol impl/add-protocol)

(t/ann get-protocol [t/Any t/Sym t/Any -> (t/U nil r/Type)])
(defn get-protocol
  "Returns the protocol with var symbol sym.
  Returns nil if not found."
  [checker sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn nil? r/Protocol? r/TypeFn?) %)]}
  (force-type (get (protocol-env checker) sym) opts))

(t/ann resolve-protocol [t/Any t/Sym t/Any -> r/Type])
(defn resolve-protocol [checker sym opts]
  {:post [(r/Type? %)]}
  (let [p (get-protocol checker sym opts)]
    (when-not p 
      (err/int-error (str "Could not resolve Protocol: " sym
                          "\n\nHint: Add protocol annotations with ann-protocol")
                     opts))
    p))
