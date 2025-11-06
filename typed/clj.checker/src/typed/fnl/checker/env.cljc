;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.fnl.checker.env
  (:require [typed.cljc.runtime.env :as env]
            [clojure.core.typed.util-vars :as vs]))

(defn empty-fnl-checker []
  (env/empty-checker))

(defonce fnl-checker-atom (env/init-checker (empty-fnl-checker)))

(defn clear-fnl-checker! []
  (reset! fnl-checker-atom (empty-fnl-checker)))

(let [-opts {::env/checker fnl-checker-atom
             :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/fnl}]
  (defn fnl-opts [] -opts))
