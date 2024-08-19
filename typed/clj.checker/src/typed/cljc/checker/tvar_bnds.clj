;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc 
  typed.cljc.checker.tvar-bnds
  (:refer-clojure :exclude [assert defn defn- fn])
  (:require [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.custom-assertions :refer [assert defn defn- fn]]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.perf-utils :as perf])
  (:import (typed.cljc.checker.type_rep Bounds Regex)))

;; this implements an environment from (fresh) type variable names
;; to their bounds.
;;
;; Intended to be equivalent to a field on F or B, but without the bloat
;; of adding it to every instance

(t/defalias TVarBndsEnv
  "A map from (fresh) type variable names (symbols) to
  their bounds."
  (t/Map t/Symbol (t/U Bounds Regex)))

(t/ann ^:no-check tvar-bnds-env? (t/Pred TVarBndsEnv))
(def tvar-bnds-env? (con/hash-c? symbol? (some-fn r/Bounds? r/Regex?)))

(t/ann initial-tvar-bnds-env TVarBndsEnv)
(def initial-tvar-bnds-env {})

(defn lookup-tvar-bnds
  "Returns the bounds of tvar or nil"
  [var {::keys [current-tvar-bnds] :as opts}]
  (get current-tvar-bnds var))

(defn extend-many
  "Extend env with pairwise mappings from vars to bndss derived from `frees-map`."
  [env syms bndss]
  {:pre [(every? symbol? syms)
         (every? (some-fn r/Bounds? r/Regex?) bndss)
         (= (count syms) (count bndss))]
   :post [(tvar-bnds-env? %)]}
  (perf/reduce assoc env syms bndss))

(defn with-extended-bnds
  "Takes a list of vars and bnds extends the current tvar environment.
  vars are the fresh names of the frees, rather than the scoped names."
  [opts syms bndss]
  (let [current-bnds (::current-tvar-bnds opts initial-tvar-bnds-env)]
    (assoc opts ::current-tvar-bnds (extend-many current-bnds syms bndss))))
