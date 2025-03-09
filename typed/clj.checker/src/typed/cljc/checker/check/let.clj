;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.cljc.checker.check.let
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check :as check]
            [typed.cljc.checker.check.print-env :as print-env]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.subst-obj :as subst-obj]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.update :as update]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.var-env :as var-env]))

(defn update-env [env sym {:keys [t fl o] :as r} is-reachable opts]
  {:pre [(lex/PropEnv? env)
         (simple-symbol? sym)
         (r/TCResult? r)
         (instance? clojure.lang.Volatile is-reachable)]
   :post [(lex/PropEnv? %)]}
  (let [{:keys [then else]} fl
        p* (cond
             ;; init has object `o`.
             ;; we don't need any new info -- aliasing and the
             ;; lexical environment will have the needed info.
             (obj/Path? o) []

             ;; FIXME can we push this optimisation further into
             ;; the machinery? like, check-below?
             (not (c/overlap t r/-falsy opts)) [then]

             ;; TODO (c/overlap t (NOT r/-falsy)) case,
             ;; which requires thorough testing of Not + (overlap, In, Un)

             ;; init does not have an object so remember new binding `sym`
             ;; in our propositions
             :else [(fo/-or [(fo/-and [(fo/-not-filter r/-falsy sym)
                                       then]
                                      opts)
                             (fo/-and [(fo/-filter r/-falsy sym) 
                                       else]
                                      opts)]
                            opts)])
        new-env (-> env
                    ;update binding type
                    (lex/extend-env sym t o opts)
                    (update/env+ p* is-reachable opts))]
    new-env))

;now we return a result to the enclosing scope, so we
;erase references to any bindings this scope introduces
;FIXME is this needed in the presence of hygiene?
(defn erase-objects [syms ret opts]
  {:pre [((con/set-c? simple-symbol?) syms)
         (r/TCResult? ret)]
   :post [(r/TCResult? %)]}
  (reduce (fn [ty sym]
            {:pre [(r/TCResult? ty)
                   (simple-symbol? sym)]}
            (-> ty
                (update :t subst-obj/subst-type sym obj/-empty true opts)
                (update :fl subst-obj/subst-filter-set sym obj/-empty true nil opts)
                (update :o subst-obj/subst-object sym obj/-empty true opts)))
          ret
          syms))

(defn check-let [{:keys [body bindings] :as expr} expected {is-loop :loop? :keys [expected-bnds]} {::check/keys [check-expr] :as opts}]
  {:post [(-> % u/expr-type r/TCResult?)
          (vector? (:bindings %))]}
  (cond
    (and is-loop (seq bindings) (not expected-bnds))
    (do
      (err/tc-delayed-error "Loop requires more annotations" opts)
      (assoc expr
             u/expr-type (or expected (r/ret (r/Bottom)))))
    :else
    (let [is-reachable (volatile! true)
          [env cbindings]
          (reduce
            (fn [[env cbindings] [n expected-bnd]]
              {:pre [@is-reachable
                     (lex/PropEnv? env)
                     ((some-fn nil? r/Type?) expected-bnd)
                     (= (boolean expected-bnd) (boolean is-loop))]
               :post [((con/maybe-reduced-c? (con/hvector-c? lex/PropEnv? vector?)) %)]}
              (let [expr (get cbindings n)
                    ; check rhs
                    {sym :name :as cexpr} (check-expr expr (when is-loop
                                                             (r/ret expected-bnd))
                                                      (var-env/with-lexical-env opts env))
                    new-env (update-env env sym (u/expr-type cexpr) is-reachable opts)
                    maybe-reduced (if @is-reachable identity reduced)]
                (maybe-reduced
                  [new-env (assoc cbindings n cexpr)])))
            [(lex/lexical-env opts) bindings]
            (map vector
                 (range (count bindings))
                 (or expected-bnds
                     (repeat nil))))
          _ (assert (= (count bindings) (count cbindings)))]
      (cond
        (not @is-reachable) (assoc expr 
                                   :bindings cbindings
                                   u/expr-type (or expected (r/ret (r/Bottom))))

        :else
        (let [cbody (let [opts (var-env/with-lexical-env opts env)]
                      (if is-loop
                        (let [opts (assoc opts ::recur-u/recur-target (recur-u/RecurTarget-maker expected-bnds nil nil nil))]
                          (check-expr body expected opts))
                        (check-expr body expected (assoc opts ::vs/current-expr body))))
             unshadowed-ret (erase-objects (into #{} (map :name) cbindings) (u/expr-type cbody) opts)]
          (assoc expr
                 :body cbody
                 :bindings cbindings
                 u/expr-type unshadowed-ret))))))

