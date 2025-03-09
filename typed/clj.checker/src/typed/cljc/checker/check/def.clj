;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.check.def
  (:require [typed.clojure :as t]
            [clojure.core.typed.coerce-utils :as coerce]
            [typed.cljc.checker.ns-options :as ns-opts]
            [clojure.core.typed :as T]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.var-env :as var-env]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.utils :as u]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.runtime.env :as env])
  (:import (clojure.lang Var)))

(defn init-provided? [expr]
  (some? (:init expr)))

;[Expr (Option TCResult) -> Expr]
(defn check-normal-def
  "Checks a def that isn't a macro definition."
  [{:keys [meta init env] :as expr} expected {::vs/keys [check-config]
                                              ::check/keys [check-expr] :as opts}]
  {:post [(:init %)]}
  (let [checker (env/checker opts)
        init-provided (init-provided? expr)
        _ (assert init-provided)
        vsym (ast-u/def-var-name expr opts)
        warn-if-unannotated? (ns-opts/warn-on-unannotated-vars? checker (cu/expr-ns expr opts))
        t (var-env/lookup-Var-nofail vsym opts)
        ;_ (prn "lookup var" vsym t)
        check? (var-env/check-var? checker vsym)
        ;_ (prn "check? var" vsym check?)
        cljs-ret (r/ret r/-any)]
    (cond
      ; check against an expected type
      (and check? t)
      (let [cinit (when init-provided
                    (check-expr init (r/ret t)
                                (-> opts
                                    (assoc ::vs/current-env (:env init))
                                    (assoc ::vs/current-expr init))))
            cmeta (when meta
                    (check-expr meta nil
                                (-> opts
                                    (assoc ::vs/current-env (:env meta))
                                    (assoc ::vs/current-expr meta))))
            _ (when cinit
                ; now consider this var as checked
                (var-env/add-checked-var-def (env/checker opts) vsym))]
        (assoc expr
               :init cinit
               :meta cmeta
               u/expr-type (below/maybe-check-below
                             (impl/impl-case opts
                               :clojure (r/ret (c/-name `t/Var t)
                                               (fo/-true-filter))
                               :cljs cljs-ret)
                             expected
                             opts)))

      ; if warn-if-unannotated?, don't try and infer this var,
      ; just skip it
      (or (not check?) 
          (and warn-if-unannotated?
               (not t)))
      (do (println (when-let [line (-> expr :env :line)] 
                     (str line ": ")) 
                   "Not checking" vsym "definition")
          (assoc expr
                 u/expr-type (below/maybe-check-below
                               (impl/impl-case opts
                                 :clojure (r/ret (c/-name `t/AnyVar)
                                                 (fo/-true-filter))
                                 :cljs cljs-ret)
                               expected
                               opts)))

      ;otherwise try and infer a type
      :else
      (let [_ (println (when-let [line (-> expr :env :line)] 
                         (str line ": "))
                       "WARNING: Checking" vsym "definition without an expected type.")
            _ (assert (not t))
            unannotated-def (:unannotated-def check-config)
            ;_ (prn "unannotated-def" unannotated-def)
            cinit (when init-provided
                    (case unannotated-def
                      ;:unchecked (assoc init u/expr-type (r/ret (r/-unchecked vsym)))
                      (check-expr init nil opts)))
            cmeta (when meta
                    (check-expr meta nil
                                (-> opts
                                    ;; emit-form does not currently
                                    ;; emit :meta nodes in a :def. Don't
                                    ;; try and rewrite it, just type check.
                                    (assoc ::vs/can-rewrite false)
                                    (assoc ::vs/current-env (:env meta))
                                    (assoc ::vs/current-expr meta))))
            inferred (r/ret-t (u/expr-type cinit))
            _ (assert (r/Type? inferred))
            #_#_ ;; old behavior, type should now only be annotated at runtime
            _ (when (and (not= unannotated-def :unchecked)
                         cinit)
                ; now consider this var as checked
                (var-env/add-checked-var-def (env/checker opts) vsym)
                ; and add the inferred static type (might be Error)
                (var-env/add-var-type (env/checker opts) vsym inferred opts))]
        (assoc expr
               :init cinit
               :meta cmeta
               u/expr-type (below/maybe-check-below
                             (impl/impl-case opts
                               :clojure (r/ret (c/-name `t/Var inferred)
                                               (fo/-true-filter))
                               :cljs cljs-ret)
                             expected
                             opts))))))

(defn defmacro-or-declare? 
  "Returns true if this :def AST was originally a defmacro or declare."
  [{:keys [^Var var] :as expr}]
  (or (.isMacro var)
      (not (init-provided? expr))))

(defn check-defmacro-or-declare
  "To check a defmacro or declare, just assign it the most general
  Var type and ignore the body."
  [expr expected opts]
  (assoc expr
         u/expr-type (below/maybe-check-below
                       (r/ret (c/-name `t/AnyVar))
                       expected
                       opts)))

(defn check-def
  "Check a def. If it is a declare or a defmacro, don't try and check it."
  [{:keys [var init env] :as expr} expected opts]
  (impl/assert-clojure opts) ;;TODO cljs support
  ;(prn " Checking def" var)
  (cond 
    ;ignore macro definitions and declare
    (defmacro-or-declare? expr) (check-defmacro-or-declare expr expected opts)

    :else (check-normal-def expr expected opts)))

(defn add-checks-normal-def
  "Add runtime checks to a def with an initial value."
  [expr expected {::check/keys [check-expr] :as opts}]
  (let [checker (env/checker opts)
        _ (assert (init-provided? expr))
        vsym (ast-u/def-var-name expr opts)
        check? (var-env/check-var? checker vsym)
        t (when check?
            (var-env/lookup-Var-nofail vsym opts))]
    (assoc expr
           :init (if t
                   ;;cast immediately, don't propagate type.
                   (cu/add-cast
                     (check-expr (:init expr) nil opts)
                     t
                     {:positive "cast"
                      :negative "cast"}
                     opts)
                   ;;
                   (check-expr (:init expr) nil opts)))))
