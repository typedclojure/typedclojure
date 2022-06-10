;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.meta-ann
  (:require [typed.clojure :as-alias t]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.inst :as inst]
            [typed.clj.ext.clojure.core.typed__tc-ignore :as tc-ignore]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check-below :as below]))

(def ^:dynamic ^:private *meta-debug-depth* 0)

(defn check-meta-debug [expr maybe-msg expected]
  (when-not ((some-fn true? string?) maybe-msg)
    (err/int-error (str "::t/dbg value must be a string, given: " (pr-str maybe-msg))))
  (let [id (gensym)
        prefix (str (apply str (repeat *meta-debug-depth* " ")) "::t/dbg id=" id)
        _ (when (string? maybe-msg)
            (println prefix maybe-msg))
        _ (println prefix (binding [*print-namespace-maps* false]
                            (pr-str (:form expr))))

        _ (when expected
            (println prefix (str "expected: " (pr-str (prs/unparse-TCResult expected)))))
        cexpr (binding [*meta-debug-depth* (inc *meta-debug-depth*)]
                (check-expr expr expected))
        _ (println prefix "result:" (binding [*print-namespace-maps* false]
                                      (pr-str (prs/unparse-TCResult (u/expr-type cexpr)))))]
    cexpr))

(defn check-meta-unsafe-cast [expr tsyn expected]
  (-> expr
      check-expr
      (assoc u/expr-type (below/maybe-check-below
                           (r/ret (prs/parse-type tsyn))
                           expected))))

(defn check-meta-ann [expr tsyn expected]
  (let [inner-expected (r/ret (prs/parse-type tsyn))]
    (-> (check-expr expr inner-expected)
        (update u/expr-type below/maybe-check-below expected))))

(defn check-meta-ignore [expr ignore? expected]
  (when-not (true? ignore?)
    (prs/prs-error (str "::t/ignore must be true, actual: " (pr-str ignore?))))
  (tc-ignore/defuspecial__tc-ignore expr expected))

(defn check-meta-inst [expr targs-syn expected]
  (let [targs-syn (cond-> targs-syn
                    (and (seq? targs-syn)
                         (= 2 (count targs-syn))
                         (= 'quote (first targs-syn))
                         (-> targs-syn second meta :fake-quote))
                    second)
        _ (when-not (vector? targs-syn)
            (prs/prs-error "::t/inst must be a vector"))]
    (-> expr
        check-expr
        (update u/expr-type #(inst/inst-from-targs-syn (:t %) targs-syn (cu/expr-ns expr) expected)))))

(def meta-keys
  [{:k ::t/dbg :f #'check-meta-debug :propagate-expected? true}
   {:k ::t/- :f #'check-meta-ann}
   {:k ::t/inst :f #'check-meta-inst}
   {:k ::t/unsafe-cast :f #'check-meta-unsafe-cast}
   {:k ::t/ignore :f #'check-meta-ignore}])

(defn maybe-check-meta-ann
  [expr expected]
  {:pre [(= :unanalyzed (:op expr))]}
  (let [form-metas (-> expr :form meta)]
    (when-some [[{:keys [k f v propagate-expected?] :as match} :as all-matching]
                (not-empty
                  (keep #(some-> (find form-metas (:k %))
                                 (->> val (assoc % :v)))
                        meta-keys))]
      (let [propagate-expected? (or propagate-expected?
                                    (= 1 (count all-matching)))]
        (-> (f (update expr :form vary-meta dissoc k) v (when propagate-expected? expected))
            (cond-> (not propagate-expected?) (update u/expr-type below/maybe-check-below expected)))))))
