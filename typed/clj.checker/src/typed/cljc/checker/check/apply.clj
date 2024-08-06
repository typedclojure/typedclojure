;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.apply
  (:require [typed.clojure :as t]
            [typed.cljc.checker.free-ops :as free-ops]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.string :as str]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

; we should be able to remove check-apply completely, but we should also instantiate all poly function in test case
(defn maybe-check-apply
  [-invoke-apply {[fexpr & args] :args :as expr} expected {::check/keys [check-expr] :as opts}]
  {:post [((some-fn nil? (comp r/TCResult? u/expr-type)) %)]}
  (or (-invoke-apply expr expected opts)
      (let [opts (assoc opts ::vs/current-expr expr)
            cfexpr (check-expr fexpr nil opts)
            ftype (r/ret-t (u/expr-type cfexpr))
            [fixed-args tail] [(butlast args) (last args)]]
        (cond
          ;; apply of a simple polymorphic function
          (r/Poly? ftype)
          (let [vars (c/Poly-fresh-symbols* ftype)
                bbnds (c/Poly-bbnds* vars ftype opts)
                body (c/Poly-body* vars ftype opts)
                opts (free-ops/with-bounded-frees opts vars bbnds)
                _ (assert (r/FnIntersection? body))
                fixed-args (mapv #(check-expr % nil opts) fixed-args)
                arg-tys (mapv (comp r/ret-t u/expr-type) fixed-args)
                ctail (check-expr tail nil opts)
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
                  (recur (next fs))))))))))
