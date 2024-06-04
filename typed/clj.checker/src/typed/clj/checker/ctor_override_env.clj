;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.ctor-override-env
  (:require [clojure.core.typed.contract-utils :as con]
            [typed.cljc.runtime.env :as env]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructor Override Env

(def add-constructor-override impl/add-constructor-override)

(defn reset-constructor-override-env! [checker m]
  (env/swap-checker! checker assoc impl/constructor-override-env-kw m)
  nil)

(defn merge-constructor-override-env! [checker m]
  {:pre [(map? m)]}
  (env/swap-checker! checker update impl/constructor-override-env-kw merge m)
  nil)

(defn constructor-override-env [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/constructor-override-env-kw {}))

(defn get-constructor-override [checker sym]
  {:post [((some-fn nil? r/Type?) %)]}
  (force-type (get (constructor-override-env checker) sym)))
