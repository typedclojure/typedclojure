;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.ns-options
  (:require [typed.clojure :as t]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.runtime.env :as env]))

(t/defalias NsOptions
  "Options for namespaces"
  (t/HMap :optional
          {:warn-on-unannotated-vars Boolean}))

(t/defalias OptMap
  (t/Map t/Sym NsOptions))

(t/ann ^:no-check reset-ns-opts! [t/Any -> nil])
(defn reset-ns-opts! [checker]
  (env/swap-checker! checker assoc impl/ns-opts-kw {})
  nil)

(t/ann ^:no-check register-warn-on-unannotated-vars [t/Any t/Sym -> nil])
(def register-warn-on-unannotated-vars impl/register-warn-on-unannotated-vars)

(defn get-ns-opts [checker nsym]
  {:post [(map? %)]}
  (get-in (env/deref-checker checker) [impl/ns-opts-kw nsym] {}))

(t/ann ^:no-check warn-on-unannotated-vars? [t/Any t/Sym -> Boolean])
(defn warn-on-unannotated-vars? [checker nsym]
  (boolean (:warn-on-unannotated-vars (get-ns-opts checker nsym))))
