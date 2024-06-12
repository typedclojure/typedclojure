;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.set
  (:require [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u])
  (:import (clojure.lang PersistentHashSet)))

(defn check-set [{:keys [items] :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:items %))]}
  (let [cargs (mapv #(check-expr % nil opts) items)
        ts (map (comp #(c/fully-resolve-type % opts) r/ret-t u/expr-type) cargs)
        res-type (if (every? r/Value? ts)
                   (r/-hset (r/sorted-type-set ts))
                   (impl/impl-case opts
                     :clojure (c/RClass-of PersistentHashSet [(c/Un ts opts)] opts)
                     :cljs (c/-name 'typed.clojure/Set (c/Un ts opts))))]
    (assoc expr
           :items cargs
           u/expr-type (binding [vs/*current-expr* expr]
                         (below/maybe-check-below
                           (r/ret res-type (fo/-true-filter))
                           expected
                           opts)))))
