;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.clj.checker.method-override-env
  (:require [typed.cljc.runtime.env :as env]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.type-rep :as r]))

; Should only override a method with a more specific type
; eg. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Method Override Env

(def add-method-override impl/add-method-override)

(defn reset-method-override-env! [checker m]
  (env/swap-checker! checker assoc impl/method-override-env-kw m)
  nil)

(defn merge-method-override-env! [checker m]
  {:pre [(map? m)]}
  (env/swap-checker! checker update impl/method-override-env-kw merge m)
  nil)

(defn method-override-env [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/method-override-env-kw {}))

(defn get-method-override [checker m opts]
  {:post [((some-fn r/Poly? r/FnIntersection? nil?) %)]}
  (force-type (get (method-override-env checker) m) opts))
