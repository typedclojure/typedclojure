;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.ext.clojure.core.typed__inst
  "Type rule for clojure.core.typed/inst"
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.set :as set]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.dvar-env :as dvar-env]
            [typed.cljc.checker.inst :as inst]
            [typed.cljc.checker.tvar-env :as tvar-env]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defn -unanalyzed-special__inst
  [{[_ pform & targs :as form] :form :keys [env] :as expr} expected]
  {:post [(-> % u/expr-type r/TCResult?)]}
  (let [_ (when-not (next form)
            (err/int-error "Wrong arguments to inst"))
        ptype (-> pform (ana2/unanalyzed env)
                  check-expr u/expr-type r/ret-t c/fully-resolve-type)
        ; support (inst :kw ...)
        ptype (if (c/keyword-value? ptype)
                (c/KeywordValue->Fn ptype)
                ptype)]
    (if-not ((some-fn r/Poly? r/PolyDots?) ptype)
      (binding [vs/*current-expr* expr]
        (err/tc-delayed-error (str "Cannot instantiate non-polymorphic type: " (prs/unparse-type ptype))
                              :return (assoc expr
                                             u/expr-type (cu/error-ret expected))))
      (let [[targs-syn kwargs] (split-with (complement keyword?) targs)
            _ (when-not (even? (count kwargs))
                (err/int-error (str "Expected an even number of keyword options to inst, given: " (vec kwargs))))
            _ (when (seq kwargs)
                (when-not (apply distinct? (map first (partition 2 kwargs)))
                  (err/int-error (str "Gave repeated keyword args to inst: " (vec kwargs)))))
            {:keys [named] :as kwargs} kwargs
            _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
                (when (seq unsupported)
                  (err/int-error (str "Unsupported keyword argument(s) to inst " unsupported))))
            _ (when (contains? kwargs :named)
                (when-not (and (map? named)
                               (every? symbol? (keys named)))
                  (err/int-error (str ":named keyword argument to inst must be a map of symbols to types, given: " (pr-str named)))))
            named (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)
                            vs/*current-expr* expr]
                    (into {}
                          (map (fn [[k v]]
                                 [k (prs/parse-type v)]))
                          named))
            targs (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)
                            vs/*current-expr* expr]
                    (mapv prs/parse-type targs-syn))]
        (assoc expr
               u/expr-type (below/maybe-check-below
                             (r/ret 
                               (binding [prs/*unparse-type-in-ns* (cu/expr-ns expr)
                                         vs/*current-expr* expr]
                                 (inst/manual-inst ptype targs named)))
                             expected))))))
