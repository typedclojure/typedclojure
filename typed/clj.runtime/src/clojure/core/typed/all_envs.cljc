;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc clojure.core.typed.all-envs
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.load-if-needed :refer [load-if-needed]]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.parse-unparse :refer [unparse-type]]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.var-env :refer [var-annotations]]
            [typed.cljc.runtime.env :as env]))

(defn- name-env []
  (binding [vs/*verbose-types* true]
    (into {}
          (for [[k v] (nme-env/name-env)]
            (when-not (keyword? v)
              [k (unparse-type v)])))))

(defn- var-env []
  (binding [vs/*verbose-types* true]
    (into {}
          (for [[k v] (var-annotations)]
            [k (unparse-type (force v))]))))

(defn all-envs-clj []
  (load-if-needed)
  (impl/with-clojure-impl
    (env/with-pinned-env
      {:aliases (name-env)
       :vars (var-env)})))
