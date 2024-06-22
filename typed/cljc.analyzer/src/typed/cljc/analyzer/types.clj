;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; adapted from clojure.tools.analyzer
(ns typed.cljc.analyzer.types
  (:refer-clojure :exclude [macroexpand-1 var?])
  (:require [typed.clojure :refer [ann defalias] :as t]
            [typed.cljc.analyzer :as-alias ana]
            [typed.cljc.analyzer.ast :as-alias ast]
            [typed.cljc.analyzer.utils :as u]))

(defalias ana/Env (t/Map t/Kw t/Any))
(defalias ana/Config (t/HMap :optional {:top-level t/Bool}))
(defalias ana/Opts
  "
  :typed.cljc.analyzer/resolve-ns
  Resolves the ns mapped by the given sym in the global env.
  If sym is shadowed by a local in env, returns nil.

  :typed.cljc.analyzer/current-ns-name
  Returns the name symbol of the current namespace.

  :typed.cljc.analyzer/parse
  Multimethod that dispatches on op, should default to -parse

  :typed.cljc.analyzer/eval-ast
  Evaluates an AST node, attaching result to :result.

  :typed.cljc.analyzer/create-var
  Creates a var for sym and returns it."
  (t/HMap :mandatory {::ana/resolve-ns [t/Sym ana/Env ana/Opts :-> t/Any]
                      ::ana/current-ns-name [t/Env ana/Opts :-> t/Sym]
                      ::ana/parse [(t/Seq t/Any) ana/Env ana/Opts :-> t/Any]
                      ::ana/eval-ast [ana/Expr ana/Opts :-> (t/Assoc ana/Expr ':result t/Any)]
                      ::ana/create-var [t/Sym ana/Env ana/Opts :-> t/Any]}))
(defalias ana/Expr (t/Merge
                     (t/HMap :mandatory {;:op t/Kw
                                         :env ana/Env}
                             :optional {::ana/config ana/Config
                                        :result t/Any})
                     (t/U (t/HMap :mandatory {:op ':do
                                              :ret ana/Expr}
                                  ;;FIXME causes stackoverflow
                                  ;:complete? true
                                  )
                          (t/HMap :mandatory {:op ':meta}
                                  ;:complete? true
                                  )
                          )
                     ))
(defalias ast/Pre [ana/Expr t/Any :-> ana/Expr])
(defalias ast/Post [ana/Expr t/Any :-> ana/Expr])
(defalias ana/Unanalyzed ana/Expr)
(defalias ana/Form t/Any)
(defalias ana/Op t/Kw)
(defalias u/Classification t/Kw)
(defalias u/Ctx (t/U ':ctx/expr))

(ann ana/macroexpand-1 [t/Any ana/Env t/Any :-> t/Any])
(ann ana/var? [t/Any :-> t/Bool])
(ann ana/scheduled-passes '{:init-ast [t/Any t/Any :-> t/Any]
                            :pre ast/Pre
                            :post ast/Post})
(ann ana/resolve-sym [t/Sym :-> t/Any])
(ann ana/current-ns-name [t/Env :-> t/Sym])
(ann ana/var->sym [t/Any :-> (t/Nilable t/Sym)])
(ann ana/analyze-outer [ana/Expr t/Any :-> ana/Expr])
(ann ana/unanalyzed [t/Any ana/Env t/Any :-> ana/Unanalyzed])
(ann ana/run-pre-passes [ana/Expr t/Any :-> ana/Expr])
(ann ana/run-post-passes [ana/Expr t/Any :-> ana/Expr])
(ann ana/run-passes [ana/Expr t/Any :-> ana/Expr])
(ann ana/specials (t/Set t/Sym))
(ann ana/analyze-form [t/Any ana/Env t/Any :-> ana/Expr])
(ann ana/analyze [t/Any ana/Env t/Any :-> (t/Assoc ana/Expr ':top-level true)])
(ann ana/defexpr-info (t/Map t/Sym '{:fields (t/Vec t/Sym)
                                     :ns t/Sym
                                     :name t/Sym}))
(ann ana/f->fs (t/All [x y] [[x :-> y] :-> [(t/Seqable x) :-> (t/Vec y)]]))
(ann ana/f->maybe-f (t/All [x y] [[x :-> y] :-> [(t/Nilable x) :-> (t/Nilable y)]]))

(ann ana/mark-top-level [ana/Expr :-> ana/Expr])
(ann ana/unmark-top-level [ana/Expr :-> ana/Expr])
(ann ana/top-level? [ana/Expr :-> t/Bool])
(ann ana/mark-eval-top-level [ana/Expr :-> ana/Expr])
(ann ana/unmark-eval-top-level [ana/Expr :-> ana/Expr])
(ann ana/eval-top-level? [ana/Expr :-> t/Bool])
(ann ana/unanalyzed-top-level [t/Any ana/Env t/Any :-> ana/Unanalyzed])
(ann ana/inherit-top-level [ana/Expr ana/Expr :-> ana/Expr])
(ann ana/propagate-top-level [ana/Expr :-> ana/Expr])
(ann ana/propagate-result [ana/Expr :-> ana/Expr])
(ann ana/eval-top-level [ana/Expr t/Any :-> ana/Expr])
(ann ana/analyze-outer-root [ana/Expr t/Any :-> ana/Expr])
(ann ana/unanalyzed-in-env [ana/Env t/Any :-> [t/Any :-> ana/Unanalyzed]])
(t/ann-protocol ast/IASTWalk
                children-of* [ast/IASTWalk :-> (t/Vec ana/Expr)]
                update-children* [ast/IASTWalk [ana/Expr :-> ana/Expr] :-> ast/IASTWalk])
(t/ann-record typed.cljc.analyzer.WithMetaExpr
              [op :- ana/Op
               form :- ana/Form
               env :- ana/Env
               meta :- ana/Expr
               expr :- ana/Expr
               top-level :- t/Any
               children :- (t/Vec t/Kw)])
(ann ana/update-withmetaexpr-children [typed.cljc.analyzer.WithMetaExpr
                                       [ana/Expr :-> ana/Expr]
                                       :-> typed.cljc.analyzer.WithMetaExpr])

(ann ana/wrapping-meta [ana/Expr t/Any :-> ana/Expr])

(t/ann-record typed.cljc.analyzer.ConstExpr
              [op :- ana/Op
               form :- ana/Form
               env :- ana/Env
               type :- u/Classification
               literal? :- t/Bool
               val :- ana/Form
               meta :- (t/Nilable ana/Expr)
               top-level :- t/Any
               children :- (t/Nilable (t/Vec t/Kw))])
(ann ana/update-constexpr-children [typed.cljc.analyzer.ConstExpr
                                    [ana/Expr :-> ana/Expr]
                                    :-> typed.cljc.analyzer.ConstExpr])

(t/ann-record typed.cljc.analyzer.VectorExpr
              [op :- ana/Op
               env :- ana/Env
               items :- (t/Vec ana/Expr)
               form :- ana/Form
               children :- (t/Vec t/Kw)
               top-level :- t/Any])
(ann ana/update-vectorexpr-children [typed.cljc.analyzer.VectorExpr
                                     [ana/Expr :-> ana/Expr]
                                     :-> typed.cljc.analyzer.VectorExpr])


(ann typed.cljc.analyzer.ast/walk [ana/Expr ast/Pre ast/Post t/Any :? :-> ana/Expr])
(ann typed.cljc.analyzer.ast/update-children [ana/Expr [ana/Expr :-> ana/Expr] t/Any :? :-> ana/Expr])
(ann typed.cljc.analyzer.ast/children [ana/Expr :-> (t/Vec t/Kw)])

(ann ana/analyze-symbol [t/Sym ana/Env t/Any :-> ana/Expr])
(ann ^:no-check ana/analyze-const [t/Any ana/Env (t/Nilable u/Classification) t/Any :-> ana/Expr])
(ann u/classify [t/Any :-> u/Classification])
(ann u/ctx [ana/Env u/Ctx :-> (t/Assoc ana/Env ':context u/Ctx)])
(ann u/obj? (t/Pred (t/Instance clojure.lang.IObj)))
(ann ana/analyze-seq [(t/NonEmptySeq t/Any) ana/Env t/Any :-> ana/Expr])
(ann ana/analyze-map [(t/Map t/Any t/Any) ana/Env t/Any :-> ana/Expr])
(ann ana/analyze-vector [(t/Vec t/Any) ana/Env t/Any :-> ana/Expr])
(ann ana/analyze-set [(t/Set t/Any) ana/Env t/Any :-> ana/Expr])
