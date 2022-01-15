;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker.field-override-env
  (:require [typed.cljc.runtime.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.type-rep :as r]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Field Override Env

(def add-field-override impl/add-field-override)

(defn reset-field-override-env! [m]
  (env/swap-checker! assoc impl/field-override-env-kw m)
  nil)

(defn merge-field-override-env! [m]
  {:pre [(map? m)]}
  (env/swap-checker! update impl/field-override-env-kw merge m)
  nil)

(defn field-override-env []
  {:post [(map? %)]}
  (get (env/deref-checker) impl/field-override-env-kw {}))

(defn get-field-override [m]
  {:post [((some-fn r/Type? nil?) %)]}
  (force (get (field-override-env) m)))
