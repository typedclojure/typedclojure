;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.do
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.special-form :as spec]
            [clojure.core.typed.util-vars :as vs]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.update :as update]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.var-env :as var-env]))

(defn internal-form? [expr]
  (u/internal-form? expr spec/special-form))

(defn enforce-do-folding [{:keys [statements] :as expr} kw opts]
  (when-not (<= 0 (count 
                    (filter #{kw}
                            (map :val statements)))
                1)
    (err/int-error (str "Folded special-forms detected " (ast-u/emit-form-fn expr opts)) opts)))

(defn check-do [expr expected {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:statements %))]}
  (enforce-do-folding expr spec/special-form opts)
  (let [internal-special-form
        (impl/impl-case opts
          :clojure (requiring-resolve 'typed.clj.checker.check/internal-special-form)
          :cljs (requiring-resolve 'typed.cljs.checker.check/internal-special-form))]
    (cond
      (internal-form? expr)
      (internal-special-form expr expected opts)

      :else
      (let [exprs (conj (vec (:statements expr)) (:ret expr))
            #_#_
            _ (assert (every? (comp #{:unanalyzed} :op) exprs)
                      (mapv (juxt :op :form) exprs))
            nexprs (count exprs)
            reachable (volatile! true)
            [env cexprs]
            (reduce (fn [[env cexprs] ^long n]
                      {:pre [(lex/PropEnv? env)
                             (integer? n)
                             (< n nexprs)]
                       ; :post checked after the reduce
                       }
                      (cond
                        (not @reachable) [env (assoc-in cexprs [n u/expr-type]
                                                        (r/ret (r/Bottom)
                                                               (fo/-unreachable-filter)
                                                               orep/-empty))]
                        :else
                        (let [expr (get cexprs n)
                              _ (assert (map? expr))
                              cexpr (binding [vs/*current-expr* expr]
                                      (check-expr expr
                                                  ;propagate expected type only to final expression
                                                  (when (= (inc n) nexprs)
                                                    expected)
                                                  (-> opts
                                                      ; always prefer envs with :line information, even if inaccurate
                                                      (update ::vs/lexical-env #(if (:line (:env expr))
                                                                                  (:env expr)
                                                                                  %))
                                                      (var-env/with-lexical-env env))))
                              res (u/expr-type cexpr)
                              {fs+ :then fs- :else} (r/ret-f res)
                              nenv (update/env+ env [(fo/-or [fs+ fs-] opts)] reachable opts)
                              _ (u/trace-when-let
                                  [ls (seq (cu/find-updated-locals (:l env) (:l nenv) opts))]
                                  (str "Updated local in exceptional control flow (do): " ls)
                                  opts)
                              ;_ (prn "check-do" nenv)
                              ]
                          (if @reachable
                            ;reachable
                            [nenv (assoc cexprs n cexpr)]
                            ;unreachable
                            (do ;(prn "Detected unreachable code")
                                [nenv (assoc cexprs n
                                             (assoc cexpr 
                                                    u/expr-type (r/ret (r/Bottom)
                                                                       (fo/-unreachable-filter)
                                                                       orep/-empty)))])))))
                    [(lex/lexical-env opts) exprs] (range nexprs))
            _ (assert (= (count cexprs) nexprs))
            actual-types (mapv u/expr-type cexprs)
            _ (assert (lex/PropEnv? env))
            _ (assert ((every-pred vector? seq) cexprs)) ; make sure we conj'ed in the right order
            _ (assert ((every-pred (con/every-c? r/TCResult?) seq) actual-types)
                      actual-types)]
        ;(prn "do actual-types" actual-types)
        (assoc expr
               :statements (pop cexprs)
               :ret (peek cexprs)
               u/expr-type (peek actual-types)))))) ;should be a r/ret already
