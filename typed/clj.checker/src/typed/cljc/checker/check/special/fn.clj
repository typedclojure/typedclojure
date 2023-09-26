;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.special.fn
  (:require [typed.clojure :as t]
            [clojure.core.typed.ast-utils :as ast-u]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check.fn :as fn]
            [typed.cljc.checker.check.fn-method-one :as fn-method-one]
            [typed.cljc.checker.check.fn-methods :as fn-methods]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.dvar-env :as dvar]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.object-rep :as or]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]))

(declare wrap-poly)

(defn check-anon [{:keys [methods] :as expr} {:keys [doms rngs rests drests]}
                  {:keys [frees-with-bnds dvar]}]
  {:pre [(= :fn (:op expr))]}
  (assert (apply = (map count [doms rngs rests drests rngs methods]))
          (mapv count [doms rngs rests drests rngs methods]))
  ;(prn "check-anon")
  ;(prn "doms" doms)
  (let [; only ever at most one rest type. Enforced by the t/fn macro.
        _ (assert (#{0 1} (count (remove nil? (concat rests drests)))))
        ; fixed entries are indexed by their domain count,
        ; :rest entry has variable arity.
        fixed-expecteds (into {}
                              (map (fn [dom rng rest drest]
                                     [(if (or rest drest)
                                        :rest
                                        (count dom))
                                      {:dom dom
                                       :rng rng
                                       :rest rest
                                       :drest drest}])
                                   doms
                                   rngs
                                   rests
                                   drests))
        cmethod-specs
        (mapv
          (fn [method]
            (let [{:keys [dom rng rest drest]
                   :as expecteds}
                  (get fixed-expecteds (if (ast-u/method-rest-param method)
                                         :rest
                                         (count (ast-u/method-required-params method))))
                  _ (assert expecteds)]
              ;(prn "dom" (count dom))
              ;(prn "method args" (-> method ast-u/method-required-params count))
              (fn-method-one/check-fn-method1
                method
                (r/make-Function dom (or (some-> rng r/Result-type*)
                                         r/-wild)
                                 :rest rest
                                 :drest drest
                                 :filter (some-> rng r/Result-filter*)
                                 :object (or (some-> rng r/Result-object*)
                                             or/-infer-obj)))))
          methods)

        [fs cmethods] ((juxt #(map :ftype %)
                             #(mapv :cmethod %))
                       cmethod-specs)
        _ (assert (seq fs) fs)
        _ (assert (every? r/Function? fs) fs)
        ret-type (r/ret (c/In (c/-name `t/Fn)
                              (wrap-poly (apply r/make-FnIntersection fs) frees-with-bnds dvar))
                        (fo/-FS fl/-top fl/-bot))]
    (assoc expr
           :methods cmethods
           :clojure.core.typed/cmethods cmethods
           u/expr-type ret-type)))

(defn gen-defaults
  ([expr] (gen-defaults expr r/-any))
  ([{:keys [methods] :as expr} default-type]
   {:pre [((some-fn r/Type? nil?) default-type)]}
   (let [default-type (or default-type r/-any)
         ;; :infer-locals are enabled for this namespace, this
         ;; var dereference is the dynamic type
         infer-locals?
         (-> (cu/expr-ns expr)
             find-ns
             meta
             :core.typed
             :experimental
             (contains? :infer-locals))]
     (apply merge-with (comp vec concat)
            (for [method methods]
              (let [fixed-arity (ast-u/fixed-arity method)
                    variadic? (ast-u/variadic-method? method)]
                {:doms [(vec (repeat fixed-arity (if infer-locals? 
                                                   (r/-unchecked nil)
                                                   default-type)))]
                 :rngs [nil]
                 :rests [(when variadic?
                           (if infer-locals? 
                             (r/-unchecked nil)
                             default-type))]
                 :drests [nil]}))))))

(defn all-defaults? [fn-anns poly]
  (let [defaults (and
                   (every? (fn [{:keys [dom]}]
                             (every? :default dom))
                           fn-anns)
                   (every? (comp :default :rng) fn-anns)
                   (every? (fn [{:keys [rest]}]
                             (or (:default rest)
                                 (nil? rest)))
                           fn-anns)
                   (not-any? :drest fn-anns))]
    (and (not poly)
         defaults)))

(defn prepare-expecteds [expr fn-anns]
  (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
    {:doms
     (->> fn-anns
          (map :dom)
          (mapv (fn [dom]
                  (mapv (fn [{:keys [type default]}]
                          (prs/parse-type type))
                        dom))))
     :rngs (->> fn-anns
                (map :rng)
                (mapv (fn [{:keys [type default]}]
                        (when-not default
                          (r/make-Result (prs/parse-type type)
                                         (fo/-FS fl/-infer-top
                                                 fl/-infer-top)
                                         or/-no-object)))))
     :rests (->> fn-anns
                 (map :rest)
                 (mapv (fn [{:keys [type default] :as has-rest}]
                         (when has-rest
                           (prs/parse-type type)))))
     :drests (->> fn-anns
                  (map :drest)
                  (mapv (fn [{:keys [pretype bound] :as has-drest}]
                          (when has-drest
                            (r/DottedPretype1-maker
                              (prs/parse-type pretype)
                              bound)))))}))

(defn self-type [{:keys [doms rngs rests drests] :as expecteds}]
  (apply r/make-FnIntersection
         (map (fn [dom rng rest drest]
                {:pre [((some-fn nil? r/Result?) rng)
                       ((some-fn nil? r/Type?) rest)
                       ((some-fn nil? r/DottedPretype?) drest)
                       (every? r/Type? dom)]
                 :post [(r/Function? %)]}
                (r/make-Function dom (or (some-> rng r/Result-type*) r/-any) 
                                 :rest rest :drest drest))
              doms rngs rests drests)))

(defn parse-poly [bnds]
  {:pre [((some-fn nil? vector?) bnds)]}
  (prs/parse-unknown-binder bnds))

(defn wrap-poly [ifn frees-with-bnds dvar]
  (if (and (empty? frees-with-bnds)
           (not dvar))
    ifn
    (if dvar
      (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                   (map second (concat frees-with-bnds [dvar]))
                   ifn)
      (c/Poly* (map first frees-with-bnds)
               (map second frees-with-bnds)
               ifn))))

(defn thunk-fn-expr? [expr]
  {:pre [(= :fn (:op expr))]
   :post [(boolean? %)]}
  (not-any? (some-fn :variadic?
                     (comp pos? :fixed-arity))
            (:methods expr)))

(defn symbolic-closure-candidate? [expr expected]
  {:pre [(= :fn (:op expr))
         ((some-fn nil? r/TCResult?) expected)]}
  (and r/enable-symbolic-closures?
       ((some-fn nil? #(= (r/ret r/-wild) %)) expected)
       ;; check thunks eagerly
       (not (thunk-fn-expr? expr))
       ;; don't symbolically self-recursive fn's yet
       (not (cu/fn-self-name expr))))

(defn check-core-fn-no-expected
  [fexpr]
  {:pre [(= :fn (:op fexpr))]
   :post [(= :fn (:op %))
          (r/TCResult? (u/expr-type %))]}
  ;(prn "check-core-fn-no-expected")
  (let [symb? (symbolic-closure-candidate? fexpr nil)
        flat-expecteds (gen-defaults fexpr (when symb? r/-nothing))]
    (lex/with-locals (some-> (cu/fn-self-name fexpr)
                             (hash-map (self-type flat-expecteds)))
      (cond-> (check-anon fexpr flat-expecteds nil)
        symb? (update-in [u/expr-type :t] #(r/symbolic-closure fexpr %))))))

(defn check-special-fn*
  [expr fn-anns poly expected]
  (binding [prs/*parse-type-in-ns* (cu/expr-ns expr)]
    (let [expr (-> expr
                   ana2/analyze-outer-root
                   ana2/run-pre-passes)
          _ (assert (= :fn (:op expr))
                    ((juxt :op :form) expr))
          _ (assert (vector? fn-anns) (pr-str fn-anns))
          self-name (cu/fn-self-name expr)
          _ (assert ((some-fn nil? symbol?) self-name)
                    self-name)
          ;_ (prn "self-name" self-name)
          [frees-with-bnds dvar] (parse-poly poly)
          new-bnded-frees (into {} (map (fn [[n bnd]] [(r/make-F n) bnd])) frees-with-bnds)
          new-dotted (when dvar [(r/make-F (first dvar))])
          flat-expecteds 
          (free-ops/with-bounded-frees new-bnded-frees
            (dvar/with-dotted new-dotted
              (prepare-expecteds expr fn-anns)))
          ;_ (prn "flat-expecteds" flat-expecteds)
          _ (assert ((some-fn nil? vector?) poly))

          no-annotations? (all-defaults? fn-anns poly)
          sym-clos-candidate? (symbolic-closure-candidate? expr expected)
          ;_ (prn "sym-clos-candidate?" sym-clos-candidate?)
          useful-expected-type? (boolean
                                  (when expected
                                    (seq (fn-methods/function-types (r/ret-t expected)))))]
      (cond
        ;; don't need to check anything, return a symbolic closure
        (and no-annotations? sym-clos-candidate?)
        (check-core-fn-no-expected expr)

        ;; If we have an unannotated fn macro and a good expected type, use the expected type via check-fn
        (and no-annotations? useful-expected-type?)
        (fn/check-fn expr expected)

        ;; otherwise check against the expected type after a call to check-anon.
        :else
        (let [;_ (prn "using anon-fn")
              cexpr (lex/with-locals (when self-name
                                       (let [this-type (self-type flat-expecteds)
                                             ;_ (prn "this-type" this-type)
                                             ]
                                         {self-name this-type}))
                      (free-ops/with-bounded-frees new-bnded-frees
                        (dvar/with-dotted new-dotted
                          (check-anon
                            expr
                            flat-expecteds
                            {:frees-with-bnds frees-with-bnds
                             :dvar dvar}))))]
          (update cexpr u/expr-type below/maybe-check-below expected))))))

(defn check-special-fn 
  [_check {statements :statements fexpr :ret :as expr} expected]
  {:pre [((some-fn nil? r/TCResult?) expected)
         (= 3 (count statements))]}
  ;(prn "check-special-fn")
  (let [statements (update statements 2 ana2/run-passes)
        [_ _ fn-ann-expr :as statements] statements
        fn-anns-quoted (ast-u/map-expr-at fn-ann-expr :ann)
        ;_ (prn "fn-anns-quoted" fn-anns-quoted)
        poly-quoted (ast-u/map-expr-at fn-ann-expr :poly)
        ;; always quoted
        fn-anns (second fn-anns-quoted)
        ;; always quoted
        poly (second poly-quoted)
        cfexpr (check-special-fn* fexpr fn-anns poly expected)]
    (assoc expr
           :statements statements
           :ret cfexpr
           u/expr-type (u/expr-type cfexpr))))
