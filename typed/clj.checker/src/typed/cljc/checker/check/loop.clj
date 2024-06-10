;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.loop
  (:require [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.utils :as u]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.check.let :as let]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.type-rep :as r]))

(defn parse-annotation
  "Parse the raw type annotation tsyn in the context of expr"
  [tsyn {:keys [env] :as expr} opts]
  (let [parsed-t (binding [vs/*current-env* env
                           prs/*parse-type-in-ns* (cu/expr-ns expr opts)]
                   (prs/parse-type tsyn opts))]
    parsed-t))

(defn inline-annotations [expr opts]
  {:pre [(= :loop (:op expr))]
   :post [(or (nil? %)
              (and (seq %)
                   (every? r/Type? %)))]}
  (let [;; cljs.analyzer :binding's don't have forms yet
        names (map (some-fn :form :name) (:bindings expr))
        _ (assert (every? symbol? names))
        maybe-anns (map (comp (fn [m]
                                ;(prn "meta" m)
                                (when-let [[_ tsyn] (find m :clojure.core.typed/ann)]
                                  (parse-annotation tsyn expr opts)))
                              meta)
                        names)
        normalize (when (some identity maybe-anns)
                    ;; annotate unannotated vars with Any
                    (seq (map (fn [t] (or t r/-any)) maybe-anns)))]
    normalize))

;; `recur-u/*loop-bnd-anns*` is populated in `typed.cljc.checker.check.special.loop`
(defn check-loop [expr expected opts]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (let [loop-bnd-anns recur-u/*loop-bnd-anns*
        inlines (inline-annotations expr opts)
        _ (when (and loop-bnd-anns inlines)
            (err/int-error "Cannot provide both an annotation with t/loop and inline loop" opts))
        ;_ (prn "inlines" inlines)
        anns (or loop-bnd-anns inlines)]
    (binding [recur-u/*loop-bnd-anns* nil]
      (let/check-let expr expected 
                     {:expected-bnds anns
                      :loop? true}
                     opts))))
