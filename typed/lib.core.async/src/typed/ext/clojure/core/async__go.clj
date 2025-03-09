;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.ext.clojure.core.async__go
  "Typing rules for clojure.core.async/go"
  (:require [clojure.core.typed.util-vars :as vs]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.cljc.checker.check :as check]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.lib.clojure.core.async :as tasync]))

;;======================
;; clojure.core.async/go

(defn -unanalyzed-special__go
  [{:keys [form env] :as expr} expected {::check/keys [check-expr] :as opts}]
  (let [;; type check the go body
        cbody (-> `(do ~@(rest form))
                  (ana2/unanalyzed env opts)
                  (check-expr nil opts))]
    (-> expr
        ;; put expanded body back into go call
        (update :form #(-> (list (first %)
                                 (emit-form/emit-form cbody opts))
                           (with-meta (meta %))))
        ;; evaluate partially expanded go call if top-level
        (ana2/eval-top-level opts)
        ;; use checked body to populate return type and check against expected
        (assoc
          u/expr-type (below/maybe-check-below
                        (r/ret (c/-name `tasync/Chan (-> cbody u/expr-type :t))
                               (fo/-true-filter))
                        expected
                        opts)
          :tag nil))))
