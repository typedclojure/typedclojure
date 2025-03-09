;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; don't require the checker from here
(ns ^:typed.clojure ^:no-doc typed.clj.checker.rclass-env
  (:require [typed.cljc.runtime.env :as env]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.current-impl :as impl]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Restricted Class

(def rclass-env-kw ::rclass-env)

(defn- rclasses [checker]
  (impl/current-rclass-env-kw (env/deref-checker checker) {}))

(defn get-rclass
  "Returns the RClass with class symbol csym.
  Returns nil if not found."
  [checker csym opts]
  {:post [((some-fn nil? r/RClass? r/TypeFn?) %)]}
  (force-type (get (rclasses checker) csym) opts))
