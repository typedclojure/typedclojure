;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns clojure.core.typed.checker.check.special.tc-ignore
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.checker.check-below :as below]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.checker.utils :as u]
            [typed.cljc.analyzer :as ana2]
            [clojure.core.typed.checker.type-rep :as r]))

(defn check-tc-ignore [check {:keys [statements] :as expr} expected]
  {:pre [(#{3} (count statements))]}
  (binding [vs/*current-expr* expr]
    (let [expr (if (ana2/top-level? expr)
                 ;; if this is a top-level form, there's no need to (re)traverse it with
                 ;; typed.clj.analyzer, since it's just for tag propagation (and top-levels
                 ;; don't propagate tags). send straight to the compiler for evaluation.
                 (-> expr
                     ana2/eval-ast
                     ana2/unmark-eval-top-level)
                 (-> expr
                     ana2/run-passes
                     ; ensure the main checking loop doesn't reevaluate this tc-ignore,
                     ; since run-passes has already if this is top-level.
                     ana2/unmark-eval-top-level))]
      (assoc expr
             ::t/tc-ignore true
             u/expr-type (below/maybe-check-below
                           (r/ret r/-any)
                           ;TODO use :form in 3rd statment to enhance expected error msg
                           expected)))))
