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
(defalias ana/Expr (t/Map t/Kw t/Any))
(defalias ast/Pre [ana/Expr :-> ana/Expr])
(defalias ast/Post [ana/Expr :-> ana/Expr])
(defalias ana/Unanalyzed ana/Expr)
(defalias u/Classification t/Kw)

(ann ana/macroexpand-1 [t/Any ana/Env :-> t/Any])
(ann ana/parse [(t/Seq t/Any) ana/Env :-> t/Any])
(ann ana/create-var [t/Sym ana/Env :-> t/Any])
(ann ana/var? [t/Any :-> t/Bool])
(ann ana/scheduled-passes '{:init-ast [t/Any :-> t/Any]
                            :pre ast/Pre
                            :post ast/Post})
(ann ana/resolve-sym [t/Sym :-> t/Any])
(ann ana/resolve-ns [t/Sym :-> t/Any])
(ann ana/current-ns-name [t/Env :-> t/Sym])
(ann ana/eval-ast (t/All [[x :< ana/Expr]] [x :-> (t/Assoc x ':result t/Any)]))
(ann ana/var->sym [t/Any :-> (t/Nilable t/Sym)])
(ann ana/analyze-outer [ana/Expr :-> ana/Expr])
(ann ana/unanalyzed [t/Any :-> ana/Unanalyzed])
(ann ana/run-pre-passes [ana/Expr :-> ana/Expr])
(ann ana/run-post-passes [ana/Expr :-> ana/Expr])
(ann ana/run-passes [ana/Expr :-> ana/Expr])
(ann ana/specials (t/Set t/Sym))
(ann ana/analyze-form [t/Any ana/Env :-> ana/Expr])
(ann ana/analyze [t/Any ana/Env :-> (t/Assoc ana/Expr ':top-level true)])
(ann ana/defexpr-info (t/Map t/Sym '{:fields (t/Vec t/Sym)
                                     :ns t/Sym
                                     :name t/Sym}))
(ann ana/f->fs (t/All [x y] [[x :-> y] :-> [(t/Seqable x) :-> (t/Vec y)]]))
(ann ana/f->maybe-f (t/All [x y] [[x :-> y] :-> [x :-> (t/Nilable y)]]))

(ann ana/mark-top-level [ana/Expr :-> ana/Expr])
(ann ana/unmark-top-level [ana/Expr :-> ana/Expr])
(ann ana/top-level? [ana/Expr :-> t/Bool])
(ann ana/mark-eval-top-level [ana/Expr :-> ana/Expr])
(ann ana/unmark-eval-top-level [ana/Expr :-> ana/Expr])
(ann ana/eval-top-level? [ana/Expr :-> t/Bool])
(ann ana/unanalyzed-top-level [t/Any ana/Env :-> ana/Unanalyzed])
(ann ana/inherit-top-level [ana/Expr ana/Expr :-> ana/Expr])
(ann ana/propagate-top-level [ana/Expr :-> ana/Expr])
(ann ana/propagate-result [ana/Expr :-> ana/Expr])
(ann ana/eval-top-level [ana/Expr :-> ana/Expr])
(ann ana/analyze-outer-root [ana/Expr :-> ana/Expr])
(ann ana/unanalyzed-in-env [ana/Env :-> [t/Any :-> ana/Unanalyzed]])
(t/ann-record typed.cljc.analyzer.WithMetaExprWithMetaExpr
              [op :- t/Kw
               form :- t/Any
               env :- t/Env
               meta :- ana/Expr
               expr :- ana/Expr
               top-level :- t/Any
               children :- (t/Vec t/Kw)])
(ann ana/update-withmetaexpr-children [])

(ann typed.cljc.analyzer.ast/walk
     [ana/Expr ast/Pre ast/Post t/Any :? :-> ana/Expr])

(ann ana/analyze-symbol [t/Sym ana/Env :-> ana/Expr])
(ann ana/analyze-const [t/Any ana/Env (t/Nilable u/Classification) :? :-> ana/Expr])
(ann u/classify [t/Any :-> u/Classification])
(ann ana/analyze-seq [(t/NonEmptySeq t/Any) ana/Env :-> ana/Expr])
(ann ana/analyze-map [(t/Map t/Any t/Any) ana/Env :-> ana/Expr])
(ann ana/analyze-vector [(t/Vec t/Any) ana/Env :-> ana/Expr])
(ann ana/analyze-set [(t/Set t/Any) ana/Env :-> ana/Expr])
