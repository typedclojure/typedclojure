;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;copied from clojure.tools.analyzer.passes.source-info
(ns typed.cljc.analyzer.passes.source-info
  (:require [typed.cljc.analyzer.utils :refer [-source-info merge']]))

(defn source-info
  "Adds (when available) :line, :column, :end-line, :end-column and :file info to the AST :env"
  {:pass-info {:walk :pre :depends #{}}}
  [ast opts]
  (let [env (:env ast)
        env-with-source-info (-source-info (:form ast) env env)]
    (assoc ast :env env-with-source-info)))
