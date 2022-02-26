;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.set
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  (:import (clojure.lang PersistentHashSet)))

(defn check-set [check {:keys [items] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:items %))]}
  (let [cargs (mapv check items)
        ts (map (comp c/fully-resolve-type r/ret-t u/expr-type) cargs)
        res-type (if (every? r/Value? ts)
                   (r/-hset (r/sorted-type-set ts))
                   (impl/impl-case
                     :clojure (c/RClass-of PersistentHashSet [(apply c/Un ts)])
                     :cljs (c/-name 'typed.clojure/Set (apply c/Un ts))))]
    (assoc expr
           :items cargs
           u/expr-type (binding [vs/*current-expr* expr]
                         (below/maybe-check-below
                           (r/ret res-type (fo/-true-filter))
                           expected)))))
