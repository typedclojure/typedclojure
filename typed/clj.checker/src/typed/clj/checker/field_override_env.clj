;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.checker.field-override-env
  (:require [typed.cljc.runtime.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field Override Env

(def add-field-override impl/add-field-override)

(defn reset-field-override-env! [checker m]
  (env/swap-checker! checker assoc impl/field-override-env-kw m)
  nil)

(defn merge-field-override-env! [checker m]
  {:pre [(map? m)]}
  (env/swap-checker! checker update impl/field-override-env-kw merge m)
  nil)

(defn field-override-env [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/field-override-env-kw {}))

(defn get-field-override [checker m opts]
  {:post [((some-fn r/Type? nil?) %)]}
  (force-type (get (field-override-env checker) m) opts))
