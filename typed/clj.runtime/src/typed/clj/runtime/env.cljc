;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.runtime.env
  (:require [typed.cljc.runtime.env :as env]))

(defn empty-clj-checker []
  (assoc (env/empty-checker) :clojure.core.typed.current-impl/current-impl :clojure.core.typed.current-impl/clojure))

(defonce clj-checker-atom (env/init-checker (empty-clj-checker)))

(defn clear-clj-checker! []
  (reset! clj-checker-atom (empty-clj-checker)))
