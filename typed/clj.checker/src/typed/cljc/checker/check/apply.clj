;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.apply
  (:require [typed.clojure :as t]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.string :as str]
            [typed.cljc.checker.check.utils :as cu]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.subst :as subst]))

; we should be able to remove check-apply completely, but we should also instantiate all poly function in test case
(defn maybe-check-apply
  [check-fn -invoke-apply {[fexpr & args] :args :as expr} expected opts]
  {:post [((some-fn nil? (comp r/TCResult? u/expr-type)) %)]}
  (or (-invoke-apply expr expected opts)
      (binding [vs/*current-expr* expr]
        (let [cfexpr (check-fn fexpr)
              ftype (r/ret-t (u/expr-type cfexpr))
              [fixed-args tail] [(butlast args) (last args)]]
          (cond
            ;; apply of a simple polymorphic function
            (r/Poly? ftype)
            (let [vars (c/Poly-fresh-symbols* ftype)
                  bbnds (c/Poly-bbnds* vars ftype opts)
                  body (c/Poly-body* vars ftype opts)
                  _ (assert (r/FnIntersection? body))
                  fixed-args (mapv check-fn fixed-args)
                  arg-tys (mapv (comp r/ret-t u/expr-type) fixed-args)
                  ctail (check-fn tail)
                  tail-ty (r/ret-t (u/expr-type ctail))
                  expr (assoc expr
                              :args (vec (cons cfexpr (conj fixed-args ctail))))]
              (loop [[{:keys [dom rng rest] :as ftype0} :as fs] (:types body)]
                (cond
                  (empty? fs) (err/tc-delayed-error (str "Bad arguments to polymorphic function in apply")
                                                    {:return (assoc expr
                                                                    u/expr-type (cu/error-ret expected))}
                                                    opts)

                  (not (#{:fixed :rest} (:kind ftype0))) nil

                  ;the actual work, when we have a * function and a list final argument
                  :else 
                  (if-let [substitution (cgen/handle-failure
                                          (and rest
                                               (<= (count dom)
                                                   (count arg-tys))
                                               (cgen/infer-vararg (zipmap vars bbnds)
                                                                  {}
                                                                  (cons tail-ty arg-tys)
                                                                  (cons (c/Un [r/-nil (c/-name `t/Seqable rest)] opts) dom)
                                                                  rest
                                                                  (r/Result-type* rng)
                                                                  (some-> expected r/ret-t)
                                                                  opts)))]
                    (assoc expr
                           u/expr-type (r/ret (subst/subst-all substitution (r/Result-type* rng) opts)))
                    (recur (next fs)))))))))))
