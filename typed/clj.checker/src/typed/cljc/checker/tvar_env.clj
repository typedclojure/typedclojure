;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc
  typed.cljc.checker.tvar-env
  (:refer-clojure :exclude [defn defn- fn assert])
  (:require [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.perf-utils :as perf]
            [typed.clojure :as t]
            [typed.cljc.checker.custom-assertions :refer [defn defn- fn assert]])
  (:import (typed.cljc.checker.type_rep F)))

;; this implements the Delta environment from the TOPLAS paper
;; (as well as every other paper on System F)

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;;
;; The mapped-to type is used to distinguish type variables bound
;; at different scopes

(t/defalias TVarEnv
  "Map from scoped symbols to the actual free
  variables they represent"
  (t/Map t/Sym F))

(t/ann ^:no-check tvar-env? (t/Pred TVarEnv))
;; slow
(def tvar-env? map? #_(con/hash-c? symbol? r/F?))

(t/ann initial-tvar-env TVarEnv)
(def initial-tvar-env {})

(t/ann extend-one [TVarEnv t/Sym (t/Nilable t/Sym) :? -> TVarEnv])
(defn extend-one
  "Extend a tvar environment. Adds an entry mapping var to itself,
  or if fresh-var is provided, mapping var to fresh-var"
  ([env var] (extend-one env var nil))
  ([env var fresh-var]
   {:pre [(tvar-env? env)
          (symbol? var)
          ((some-fn symbol? nil?) fresh-var)]
    :post [(tvar-env? %)]}
   (assoc env var (r/make-F (or fresh-var var)))))

(t/ann extend-many [TVarEnv (t/Coll t/Sym) (t/Nilable (t/Coll t/Sym)) -> TVarEnv])
(defn extend-many
  "Extends env with vars. If fresh-vars are provided, the vars will map to them
  pairwise in the resulting environment."
  ([env vars] (extend-many env vars nil))
  ([env vars fresh-vars]
   {:post [(tvar-env? %)]}
   (let [fresh-vars (or fresh-vars (repeat (count vars) nil))
         _ (assert (= (count vars) (count fresh-vars)))]
     (perf/reduce (fn [env var fresh-var]
                    {:pre [(symbol? var)
                           ((some-fn nil? symbol?) fresh-var)]}
                    (extend-one env var fresh-var))
                  env vars fresh-vars))))

(defn- extend-many-fresh-names
  "Extends env with vars given as `fresh-names`."
  [env fresh-names]
  {:pre [(every? symbol? fresh-names)]
   :post [(tvar-env? %)]}
  (reduce #(assoc %1 %2 (r/make-F %2)) env fresh-names))

(defn with-extended-new-tvars
  "Extends with new type variables (provided by (e.g., Poly-fresh))"
  ([opts vars fresh-vars]
   (update opts ::current-tvars (fnil extend-many initial-tvar-env) vars fresh-vars))
  ([opts fresh-names]
   (let [current-tvars (::current-tvars opts initial-tvar-env)]
     (assoc opts ::current-tvars (extend-many-fresh-names current-tvars fresh-names)))))

#_
(defn with-extended-tvars
  "Takes a list of vars and extends the current tvar environment."
  [opts vars]
  (update opts ::current-tvars (fnil extend-many initial-tvar-env) vars))
