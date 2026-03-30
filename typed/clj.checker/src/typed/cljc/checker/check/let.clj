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
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.proposition-rep :as fl]
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
             :else [(fo/-or [(fo/-and [(fo/-not-proposition r/-falsy sym)
                                       then]
                                      opts)
                             (fo/-and [(fo/-proposition r/-falsy sym) 
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
                (update :fl subst-obj/subst-proposition-set sym obj/-empty true nil opts)
                (update :o subst-obj/subst-object sym obj/-empty true opts)))
          ret
          syms))

(defn check-let
  ([expr expected opts] (check-let expr expected {} opts))
  ([{:keys [body bindings] :as expr} expected {is-loop :loop? :keys [expected-bnds]} {::check/keys [check-expr] :as opts}]
   {:post [(-> % u/expr-type r/TCResult?)
           (vector? (:bindings %))]}
   ;; When loop has no annotations, infer binding types from init expressions.
   ;; This enables type inference for loops without requiring explicit t/loop
   ;; annotations — the init expression types become the recur target types.
   ;; Value types (Val 0.0) are widened to their class type (Double) since
   ;; the loop invariant must accept all iterations, not just the init value.
   (let [expected-bnds (or expected-bnds
                           (when (and is-loop (seq bindings))
                             ;; Infer init types: check each init expression without
                             ;; an expected type (like let), collect the result types.
                             ;; Widen singleton/value types to class types.
                             (let [integer-names #{'Integer 'Long 'Short 'Byte
                                                      'java.lang.Integer 'java.lang.Long
                                                      'java.lang.Short 'java.lang.Byte}
                                   float-names #{'Double 'Float
                                                 'java.lang.Double 'java.lang.Float}
                                   ;; TC's AnyInteger — the canonical union of all integer types
                                   tc-any-integer (r/Name-maker 'typed.clojure/AnyInteger)
                                   widen (fn [t]
                                          (let [id (cond (r/Name? t) (:id t)
                                                         (r/RClass? t) (:the-class t)
                                                         :else nil)]
                                            (cond
                                              ;; Value types: widen integer vals to AnyInteger,
                                              ;; other numeric vals to Number, rest to class
                                              (r/Value? t)
                                              (let [v (:val t)]
                                                (cond
                                                  (integer? v) tc-any-integer
                                                  (number? v) (c/RClass-of Number opts)
                                                  :else (c/RClass-of (class v) opts)))
                                              ;; Named integer types → AnyInteger
                                              (contains? integer-names id) tc-any-integer
                                              ;; Named float types → Number
                                              (contains? float-names id) (c/RClass-of Number opts)
                                              :else t)))
                                   inferred
                                   (reduce
                                     (fn [[env types] n]
                                       (let [expr (get bindings n)
                                             {sym :name :as cexpr}
                                             (check-expr expr nil
                                                         (var-env/with-lexical-env opts env))
                                             t (widen (:t (u/expr-type cexpr)))
                                             ;; Update env with widened type for subsequent inits
                                             widened-result (r/ret t)
                                             new-env (update-env env sym widened-result
                                                                 (volatile! true) opts)]
                                         [new-env (conj types t)]))
                                     [(lex/lexical-env opts) []]
                                     (range (count bindings)))]
                               (second inferred))))
         is-reachable (volatile! true)
         [env cbindings]
         (reduce
           (fn [[env cbindings] [n expected-bnd]]
             {:pre [@is-reachable
                    (lex/PropEnv? env)
                    ((some-fn nil? r/Type?) expected-bnd)]
              :post [((con/maybe-reduced-c? (con/hvector-c? lex/PropEnv? vector?)) %)]}
             (let [expr (get cbindings n)
                   ; check rhs
                   {sym :name :as cexpr} (check-expr expr (when (and is-loop expected-bnd)
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
         ;; Attach per-binding types for loop forms.
         ;; let forms get binding-types via core__let extension;
         ;; loop forms go through check-let directly and need it here.
         ;; Store both uniquified (:name) and original (:form) names
         ;; so consumers can match either.
         (let [binding-types (when is-loop
                               (reduce (fn [m b]
                                         (let [uname (:name b)
                                               oname (or (:form b) uname)
                                               t (:t (u/expr-type b))]
                                           (cond-> m
                                             (and uname t) (assoc uname t)
                                             (and oname t (not= oname uname)) (assoc oname t))))
                                       {} cbindings))
               ;; Use the same metadata key as core__let for consistency
               bt-key (keyword "typed.clj.ext.clojure.core__let" "binding-types")]
           (cond-> (assoc expr
                          :body cbody
                          :bindings cbindings
                          u/expr-type unshadowed-ret)
             (seq binding-types)
             (update :form #(if % (vary-meta % assoc bt-key binding-types) %)))))))))
