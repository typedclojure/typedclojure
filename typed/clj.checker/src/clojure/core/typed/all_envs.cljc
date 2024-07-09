;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.all-envs
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-if-needed :refer [load-if-needed]]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.clj.checker.parse-unparse :refer [unparse-type]]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.var-env :refer [var-annotations]]
            [typed.clj.runtime.env :refer [clj-opts]]
            [typed.cljc.runtime.env :as env]))

(defn- name-env [checker opts]
  (into {}
        (for [[k v] (nme-env/name-env checker)]
          (when-not (keyword? v)
            (when-some [t (force-type v opts)]
              [k (unparse-type t opts)])))))

(defn- var-env [checker opts]
  (into {}
        (for [[k v] (var-annotations checker)]
          (when-some [t (force-type v opts)]
            [k (unparse-type t opts)]))))

(defn all-envs-clj []
  (load-if-needed)
  (let [opts (assoc (clj-opts) ::vs/verbose-types true)
        checker (impl/clj-checker)]
    {:aliases (name-env checker opts)
     :vars (var-env checker opts)}))
