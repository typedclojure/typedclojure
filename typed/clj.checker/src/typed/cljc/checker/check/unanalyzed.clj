;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.unanalyzed
  (:refer-clojure :exclude [requiring-resolve])
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.runtime.env :as env]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [clojure.core.typed.current-impl :as impl]))

(defn install*
  [impl op impl-sym]
  (let [info impl-sym
        prev (let [checker (get-in (impl/bindings-for-impl) [impl #'env/*checker*])]
               (-> (env/swap-checker-vals! checker assoc-in [impl/unanalyzed-special-kw op] impl-sym)
                   first
                   (get-in [impl/unanalyzed-special-kw op])))]
    (when prev
      (when (not= info prev)
        (println (str "WARNING: Unanalyzed rule for "
                      op
                      " changed from "
                      (pr-str prev)
                      " to "
                      (pr-str info)))))
    nil))

(defn -unanalyzed-special-dispatch [{:keys [op form env] :as expr} expected]
  {:pre [(#{:unanalyzed} op)]
   :post [((some-fn nil? qualified-symbol?) %)]}
  (let [res (when (seq? form)
              (-> (first form)
                  (ana2/resolve-sym env)
                  ana2/var->sym))]
    ;(prn `-unanalyzed-special-dispatch form res)
    res))

(defn run-passes+propagate-expr-type [expr]
  (-> expr
      ana2/run-passes
      (into (select-keys expr [u/expr-type]))))

(defn -unanalyzed-special [expr expected]
  (when-some [vsym (-unanalyzed-special-dispatch expr expected)]
    (when-some [impl-sym (get-in (env/deref-checker (env/checker)) [impl/unanalyzed-special-kw vsym])]
      ((requiring-resolve impl-sym) expr expected))))

;; API

(defn install-unanalyzed-special
  "Extension point for special typing rules for unanalyzed AST nodes,
  along with defuspecial Dispatches on the fully qualified
  operator of the :form of expr, if any.

  Prefer using defuspecial when possible. This
  multimethod it gives implementors complete control over the expansion, analysis,
  evaluation, and type checking of expr. This is useful if the form you
  are defining a rule for has non-trivial interaction
  between evaluation and macroexpansion at the top-level.
  For example, a macro that expands to a `do` expression
  whose subexpressions define macros used to expand its other subexpressions
  non-trivially interleaves expansion, evaluation, and type checking, and so
  -unanalyzed-top-level is more appropriate to define a typing rule for the
  macro than -unanalyzed-special.
  
  Implementors should type check the :unanalyzed expr at expected type.
  The return expr should be fully expanded, analyzed, evaluated (if top-level),
  with a u/expr-type entry for the TCResult of the entire expression.

  Implementors can return nil to give control back to the type checker."
  [impls v impl-sym]
  (doseq [impl impls]
    (install* impl v impl-sym)))

(defn install-defuspecial
  "Extension point for special typing rules for unanalyzed AST nodes.
  This is a thin wrapper around installing a method on
  -unanalyzed-special that implicitly handles
  evaluation. Dispatches on the fully qualified
  operator of the :form of expr, if any.

  Prefer defuspecial over -unanalyzed-special unless your
  form has non-trivial top-level interleaving between evaluation
  and macroexpansion (see doc for -unanalyzed-special).
  
  Implementors should, at minimum, check the expression has
  the expected type (if non-nil), and return the checked expression with
  its inferred TCResult as its u/expr-type entry.

  The return expression may be expanded and/or analyzed to any degree
  at the implementor's discretion, but *not* evaluated.
  The return expression is implicitly evaluated (via ana2/eval-top-level)
  after control is returned to the type checker.
  Use -unanalyzed-special if this requirement is too strict."
  [impls v impl-sym]
  (doseq [impl impls]
    (install* impl v impl-sym)))

(defmacro defuspecial
  [vsym & args]
  (let [[doc args] (if (string? (first args))
                     ((juxt first rest) args)
                     [nil args])
        [argv args] (if (vector? (first args))
                      ((juxt first rest) args)
                      [nil args])]
    (assert (vector? argv))
    (assert (= 2 (count argv)))
    `(defn ~vsym
       ~@(when doc [doc])
       ~argv
       (some-> (do ~@args)
               run-passes+propagate-expr-type))))
