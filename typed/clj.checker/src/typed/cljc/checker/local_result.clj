;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.local-result
  (:require [clojure.core.typed.contract-utils :as con]
            [typed.cljc.checker.lex-env :as lex]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.type-rep :as r]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.check.utils :as cu]))

(defn local-ret [sym opts]
  {:pre [(symbol? sym)]
   :post [(r/TCResult? %)]}
  (let [[obj t] ((juxt #(lex/lookup-alias % opts) #(lex/lookup-local % opts)) sym)]
    (when-not t
      (err/int-error (str "Could not find type for local variable " sym) opts))
    (r/ret t 
           (if (c/overlap t (c/Un [r/-nil r/-false] opts) opts)
             (fo/-FS (fo/-not-filter-at r/-falsy obj)
                     (fo/-filter-at r/-falsy obj))
             (fo/-true-filter))
           obj)))

(defn local-result [expr sym expected opts]
  {:pre [(con/local-sym? sym)
         ((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (binding [vs/*current-expr* expr]
    (prs/with-unparse-ns (cu/expr-ns expr opts)
      (below/maybe-check-below
        (local-ret sym opts)
        expected
        opts))))
