;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.ext.clojure.core.async
  "Typing rules for core.async"
  (:require [clojure.core.async :as async]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.analyzer.passes.emit-form :as emit-form]
            [typed.clj.checker.check :refer [check-expr]]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.clj.checker.check.unanalyzed :refer [install-unanalyzed-special]]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.lib.clojure.core.async :as tasync]))

;;======================
;; clojure.core.async/go

;; FIXME use install-unanalyzed-special
(defn -unanalyzed-special__go
  [{:keys [form env] :as expr} expected]
  (let [;; type check the go body
        cbody (-> `(do ~@(rest form))
                  (ana2/unanalyzed env)
                  check-expr)]
    (-> expr
        ;; put expanded body back into go call
        (update :form #(-> (list (first %)
                                 (emit-form/emit-form cbody))
                           (with-meta (meta %))))
        ;; evaluate partially expanded go call if top-level
        ana2/eval-top-level
        ;; use checked body to populate return type and check against expected
        (assoc
          u/expr-type (below/maybe-check-below
                        (r/ret (c/-name `tasync/Chan (-> cbody u/expr-type :t))
                               (fo/-true-filter))
                        expected)
          :tag nil))))

(install-unanalyzed-special
  'clojure.core.async/go
  `-unanalyzed-special__go)
