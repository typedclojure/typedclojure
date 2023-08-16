;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.declared-kind-env
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env-utils :as env-utils]
            [typed.cljc.runtime.env :as env]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declared kind Env

(def declared-kind-env? (con/hash-c? symbol? r/TypeFn?))

(def current-declared-kinds-kw ::current-declared-kinds)

(defn declared-kinds []
  {:post [(map? %)]}
  (get (env/deref-checker) current-declared-kinds-kw {}))

(defn reset-declared-kinds! [m]
  {:pre [(declared-kind-env? m)]
   :post [(nil? %)]}
  (env/swap-checker! assoc current-declared-kinds-kw m)
  nil)

(defn add-declared-kind [sym tfn]
  {:pre [(symbol? sym)
         (r/TypeFn? tfn)]
   :post [(nil? %)]}
  (env/swap-checker! assoc-in [current-declared-kinds-kw sym] tfn)
  nil)

(defn declared-kind-or-nil [sym]
  {:pre [(symbol? sym)]
   :post [(or (nil? %) (r/TypeFn? %))]}
  (env-utils/force-type (get (declared-kinds) sym)))

(defn get-declared-kind [sym]
  (if-let [tfn (declared-kind-or-nil sym)]
    tfn
    (err/int-error (str "No declared kind for Name " sym))))

(defn has-declared-kind? [sym]
  (boolean (declared-kind-or-nil sym)))

(defn remove-declared-kind [sym]
  (env/swap-checker! update current-declared-kinds-kw dissoc sym)
  nil)

(defn declare-alias-kind* [sym ty]
  (add-declared-kind sym ty))
