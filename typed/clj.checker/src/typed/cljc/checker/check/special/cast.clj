;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.special.cast
  (:require [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.ast-utils :as ast-u]
            [typed.cljc.checker.object-rep :as obj]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check-below :as below]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.filter-ops :as fo]
            [clojure.core.typed.current-impl :as impl]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.filter-rep :as fl]))

(defn check-cast
  [expr expected {::check/keys [check-expr] :as opts}]
  (assert (= :do (:op expr)))
  (let [{[_ _ texpr :as statements] :statements :keys [env] frm :ret :as expr}
        (-> expr 
            ; don't need to check these statements because it's just metadata
            ; embedded in the expression by the `t/cast` macro
            (update :statements #(mapv ana2/run-passes %))
            ; but we do want to check (a subset) of this, so just run pre-passes
            (update :ret (comp ana2/run-pre-passes ana2/analyze-outer-root)))
        _ (assert (= 3 (count statements)))
        tsyn-quoted (ast-u/map-expr-at texpr :type opts)
        _ (impl/impl-case opts
            :clojure (assert (and (seq? tsyn-quoted)
                                  (= 'quote (first tsyn-quoted)))
                             (pr-str tsyn-quoted))
            :cljs nil)
        tsyn (impl/impl-case opts
               :clojure (second tsyn-quoted)
               :cljs tsyn-quoted)
        parsed-t (binding [vs/*current-env* env
                           prs/*parse-type-in-ns* (cu/expr-ns expr opts)]
                   ;; unwrap quoted syntax with second
                   (prs/parse-type tsyn opts))
        ;; frm is of the form ((fn [x] ...) x), we want to type check
        ;; x, but not the lambda.
        _ (assert (= :invoke (:op frm)))
        _ (assert (= 1 (count (:args frm))))
        ;; allows silly down casts, might want to change that.
        expr (-> expr
                 ; just need to traverse :fn using the analyzer
                 (update-in [:ret :fn] ana2/run-passes)
                 ; check the expression being cast
                 (update-in [:ret :args 0] check-expr)
                 ; top-level could be propagated here since this is a :do form,
                 ; so call eval-top-level
                 (update :ret (comp ana2/eval-top-level ana2/run-post-passes)))]
    (assoc expr
           u/expr-type (r/ret parsed-t))))
