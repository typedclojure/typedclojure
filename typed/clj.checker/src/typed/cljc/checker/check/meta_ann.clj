;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.check.meta-ann
  (:require [typed.clojure :as-alias t]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.inst :as inst]
            [typed.clj.ext.clojure.core.typed__tc-ignore :as tc-ignore]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check-below :as below]))

(defn check-meta-debug [expr maybe-msg expected {::keys [meta-debug-depth]
                                                 :or {meta-debug-depth 0}
                                                 ::check/keys [check-expr] :as opts}]
  (when-not ((some-fn true? string?) maybe-msg)
    (err/int-error (str "::t/dbg value must be a string, given: " (pr-str maybe-msg)) opts))
  (let [id (gensym "check-meta-debug")
        prefix (str (apply str (repeat (or meta-debug-depth 0) " ")) "::t/dbg id=" id)
        _ (when (string? maybe-msg)
            (println prefix maybe-msg))
        _ (println prefix (binding [*print-namespace-maps* false]
                            (pr-str (:form expr))))

        _ (when expected
            (println prefix (str "expected: " (pr-str (prs/unparse-TCResult expected opts)))))
        cexpr (check-expr expr expected (assoc opts ::meta-debug-depth (inc meta-debug-depth)))
        _ (println prefix "result:" (binding [*print-namespace-maps* false]
                                      (pr-str (prs/unparse-TCResult (u/expr-type cexpr) opts))))]
    cexpr))

(defn check-meta-unsafe-cast [expr tsyn expected {::check/keys [check-expr] :as opts}]
  (-> expr
      (check-expr nil opts)
      (assoc u/expr-type (below/maybe-check-below
                           (r/ret (prs/parse-type tsyn opts))
                           expected
                           opts))))

(defn check-meta-ann [expr tsyn expected {::check/keys [check-expr] :as opts}]
  (let [inner-expected (r/ret (prs/parse-type tsyn opts))]
    (-> (check-expr expr inner-expected opts)
        (update u/expr-type below/maybe-check-below expected opts))))

(defn check-meta-ignore [expr ignore? expected opts]
  (when-not (true? ignore?)
    (prs/prs-error (str "::t/ignore must be true, actual: " (pr-str ignore?))))
  (tc-ignore/defuspecial__tc-ignore expr expected opts))

(defn check-meta-inst [expr targs-syn expected {::check/keys [check-expr] :as opts}]
  (let [targs-syn (cond-> targs-syn
                    (and (seq? targs-syn)
                         (= 2 (count targs-syn))
                         (= 'quote (first targs-syn))
                         (-> targs-syn second meta :fake-quote))
                    second)
        _ (when-not (vector? targs-syn)
            (prs/prs-error "::t/inst must be a vector"))]
    (-> expr
        (check-expr nil opts)
        (update u/expr-type #(inst/inst-from-targs-syn (:t %) targs-syn (cu/expr-ns expr opts) expected opts)))))

(def meta-keys
  [{:k ::t/dbg :f #'check-meta-debug :propagate-expected? true}
   {:k ::t/- :f #'check-meta-ann}
   {:k ::t/inst :f #'check-meta-inst}
   {:k ::t/unsafe-cast :f #'check-meta-unsafe-cast}
   {:k ::t/ignore :f #'check-meta-ignore}])

(defn maybe-check-meta-ann
  [expr expected opts]
  {:pre [(= :unanalyzed (:op expr))]}
  (let [form-metas (-> expr :form meta)]
    (when-some [[{:keys [k f v propagate-expected?] :as match} :as all-matching]
                (not-empty
                  (keep #(some-> (find form-metas (:k %))
                                 (->> val (assoc % :v)))
                        meta-keys))]
      (let [propagate-expected? (or propagate-expected?
                                    (= 1 (count all-matching)))]
        (-> (f (update expr :form vary-meta dissoc k) v (when propagate-expected? expected) opts)
            (cond-> (not propagate-expected?) (update u/expr-type below/maybe-check-below expected opts)))))))
