;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.datatype-ancestor-env
  (:require [typed.cljc.checker.utils :as u]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.runtime.env-utils :refer [force-type]]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.subst :as subst]
            [clojure.core.typed :as t]
            [typed.cljc.runtime.env :as env]
            [typed.cljc.checker.nilsafe-utils :as nilsafe]
            [clojure.core.typed.current-impl :as impl]
            [clojure.set :as set])
  (:import (typed.cljc.checker.type_rep DataType)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Aliases

(t/defalias DTAncestorEnv
  "Environment mapping datatype names to sets of ancestor types."
  (t/Map t/Sym (t/Set r/ScopedType)))

(def tmap? (con/hash-c? any? (some-fn delay? r/Scope? r/Type?)))
(def dt-ancestor-env? (con/hash-c? symbol? tmap?))

(t/ann ^:no-check inst-ancestors [DataType (t/U nil (t/Map t/Any (t/Seqable r/Type))) -> (t/Set r/Type)])
(defn inst-ancestors
  "Given a datatype, return its instantiated ancestors"
  [{poly :poly? :as dt} anctrs]
  {:pre [(r/DataType? dt)
         ((some-fn nil? map?) anctrs)]
   :post [((con/set-c? r/Type?) %)]}
  (into #{}
        (map (fn [[_ t]]
               (c/inst-and-subst (force-type t) poly)))
        anctrs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface

(defn all-dt-ancestors [checker]
  {:post [(map? %)]}
  (get (env/deref-checker checker) impl/current-dt-ancestors-kw {}))

(t/ann ^:no-check get-datatype-ancestors [t/Any DataType -> (t/Set r/Type)])
(defn get-datatype-ancestors
  "Returns the set of overriden ancestors of the given DataType."
  [checker {:keys [poly? the-class] :as dt}]
  {:pre [(r/DataType? dt)]}
  (let [as (get (all-dt-ancestors checker) the-class)]
    (inst-ancestors dt as)))

(t/ann ^:no-check add-datatype-ancestors [t/Any t/Sym (t/Map t/Any (t/U (t/Delay r/Type) r/Type)) -> nil])
(def add-datatype-ancestors impl/add-datatype-ancestors)

(t/ann ^:no-check reset-datatype-ancestors! [t/Any DTAncestorEnv -> nil])
(defn reset-datatype-ancestors! 
  "Reset the current ancestor map."
  [checker aenv]
  {:pre [(dt-ancestor-env? aenv)]}
  (env/swap-checker! checker assoc impl/current-dt-ancestors-kw aenv)
  nil)
