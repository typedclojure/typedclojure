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
            [typed.cljc.checker.check.ignore :as ignore]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check-below :as below]))

(defn check-meta-debug [expr maybe-msg expected]
  (when-not ((some-fn true? string? maybe-msg))
    (err/int-error (str "::t/dbg value must be a string, given: " (pr-str maybe-msg))))
  (let [id (gensym)
        _ (some->> maybe-msg
                   (println id ":typed.clojure/dbg"))
        _ (println id ":typed.clojure/dbg" (pr-str (:form expr)))

        _ (println id ":typed.clojure/dbg" (if expected
                                             (str "expected:" (pr-str (prs/unparse-TCResult expected)))
                                             (str "no expected type")))
        cexpr (check-expr expr expected)
        _ (println id ":typed.clojure/dbg" "result:" (pr-str (prs/unparse-TCResult (u/expr-type cexpr))))]
    cexpr))

(defn check-meta-unsafe-cast [expr tsyn expected]
  (-> expr
      (cond-> (not (u/expr-type expr)) check-expr)
      (assoc u/expr-type (below/maybe-check-below
                           (r/ret (prs/parse-type tsyn))
                           expected))))

(defn check-meta-ann [expr tsyn expected]
  (let [inner-expected (r/ret (prs/parse-type tsyn))
        check? (not (u/expr-type expr))]
    (-> (if check?
          (check-expr expr inner-expected)
          (update expr u/expr-type below/maybe-check-below inner-expected))
        (update u/expr-type below/maybe-check-below expected))))

(defn check-meta-ignore [expr ignore? expected]
  (when-not (true? ignore?)
    (prs/prs-error (str "::t/ignore must be true, actual: " (pr-str ignore?))))
  (ignore/tc-ignore-expr expr expected))

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
        (cond-> (not (u/expr-type expr)) check-expr)
        (update u/expr-type #(inst/inst-from-targs-syn (:t %) targs-syn (cu/expr-ns expr) expected)))))

(def meta-keys
  [[::t/dbg #'check-meta-debug]
   [::t/ignore #'check-meta-ignore]
   [::t/unsafe-cast #'check-meta-unsafe-cast]
   [::t/inst #'check-meta-inst]
   [::t/- #'check-meta-ann]])

(defn maybe-check-meta-ann
  [expr expected]
  {:pre [(= :unanalyzed (:op expr))]}
  (let [form-metas (-> expr :form meta)]
    (when-some [[[k f] v] (some #(some-> (find form-metas (first %))
                                         (->> val (vector %)))
                                meta-keys)]
      (f (update expr :form vary-meta dissoc k) v expected))))
