;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.fnl.checker.check
  (:refer-clojure :exclude [#?(:clj requiring-resolve)])
  (:require [clojure.core.typed.util-vars :as vs]
            [typed.clj.analyzer :as jana2]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.analyzer.env :as env]
            [typed.cljc.checker.lex-env :as lex]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(defn check-expr
  "Type checks a Fennel expression.
  For now, returns the expression with type Any."
  [expr expected opts]
  (let [;; For now, everything has type Any
        ret (r/ret r/-any)]
    (assoc expr u/expr-type ret)))

(defn check-top-level
  "Type checks a top-level Fennel form, returning a fully analyzed AST node
  with a u/expr-type entry giving its TCResult type."
  ([form expected {:keys [env] :as opt} opts]
   (let [nsym (or (:ns env) (::prs/parse-type-in-ns opts) 'user)
         _ (assert (symbol? nsym))
         opts (-> opts
                  (assoc ::vs/lexical-env (lex/init-lexical-env))
                  (assoc ::prs/parse-type-in-ns nsym)
                  (assoc ::prs/unparse-type-in-ns nsym)
                  (assoc ::ana2/macroexpand-1 (fn [form env opts] form)) ; No macro expansion for Fennel for now
                  (env/ensure (jana2/global-env)))]
     (-> form
         (ana2/unanalyzed-top-level (or env (jana2/empty-env nsym)) opts)
         (check-expr expected opts)))))
