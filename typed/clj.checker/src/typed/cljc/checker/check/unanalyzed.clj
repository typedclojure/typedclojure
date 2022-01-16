;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.unanalyzed
  (:require [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.utils :as u]))

(defmulti -unanalyzed-special
  "Extension point for special typing rules for unanalyzed AST nodes,
  along with -unanalyzed-special. Dispatches on the fully qualified
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
  (fn [{:keys [op form env] :as expr} expected]
    {:pre [(#{:unanalyzed} op)]
     :post [((some-fn nil? qualified-symbol?) %)]}
    (when (seq? form)
      (-> (first form)
          (ana2/resolve-sym env)
          ana2/var->sym))))

(defmethod -unanalyzed-special :default [expr expected])

(defn run-passes+propagate-expr-type [expr]
  (-> expr
      ana2/run-passes
      (into (select-keys expr [u/expr-type]))))

(defmacro defuspecial
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
  [op args & body]
  (assert (vector? args))
  (assert (= 2 (count args))
          "must provide bindings for expr and expected")
  `(defmethod -unanalyzed-special ~op
     ~args
     (some-> (do ~@body)
             run-passes+propagate-expr-type)))
