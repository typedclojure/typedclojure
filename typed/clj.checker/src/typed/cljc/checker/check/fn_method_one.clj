;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure typed.cljc.checker.check.fn-method-one
  (:require [clojure.core.typed.ast-utils :as ast-u]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.analyzer :as ana2]
            [typed.cljc.checker.check-below :as below]
            [typed.clj.checker.analyze-clj :as ana-clj]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check :as check]
            [typed.cljc.analyzer :as ana]
            [typed.cljc.checker.check.fn-method-utils :as fn-method-u]
            [typed.cljc.checker.check.funapp :as funapp]
            [typed.cljc.checker.check.isa :as isa]
            [typed.cljc.checker.check.multi-utils :as-alias multi-u]
            [typed.cljc.checker.check.recur-utils :as recur-u]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.proposition-ops :as fo]
            [typed.cljc.checker.proposition-rep :as fl]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.open-result :as open-result]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.update :as update]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.var-env :as var-env]))

;check method is under a particular Function, and return inferred Function
;
; check-fn-method1 exposes enough wiring to support the differences in deftype
; methods and normal methods via `fn`.
;
; # Differences in recur behaviour
;
; deftype methods do *not* pass the first parameter (usually `this`) when calling `recur`.
;
; eg. (my-method [this a b c] (recur a b c))
;
; The behaviour of generating a RecurTarget type for recurs is exposed via the :recur-target-fn
;
;
;[MethodExpr Function -> {:ftype Function :cmethod Expr}]
(defn check-fn-method1 [method {:keys [dom rest drest kws prest pdot] :as expected}
                        {:keys [recur-target-fn] :as opt}
                        {::check/keys [check-expr] :as opts}]
  {:pre [(r/Function? expected)]
   :post [(r/Function? (:ftype %))
          (-> % :cmethod :clojure.core.typed/ftype r/Function?)
          (:cmethod %)]}
  ;(prn "check-fn-method1" expected)
  (impl/impl-case opts
    :clojure (assert (#{:fn-method :method} (:op method))
                     (:op method))
    ; is there a better :op check here?
    :cljs (assert method))
  (let [check-rest-fn (or (:check-rest-fn opt) fn-method-u/check-rest-fn)
        method (-> method
                   (ana2/run-pre-passes opts)
                   (ast-u/visit-method-params #(ana2/run-passes % opts)))
        body ((ast-u/method-body-kw) method)
        required-params (ast-u/method-required-params method)
        rest-param (ast-u/method-rest-param method)

        param-obj (comp #(obj/-path nil %)
                        :name)
        ; Difference from Typed Racket:
        ;
        ; Because types can contain abstracted names, we instantiate
        ; the expected type in the range before using it.
        ;
        ; eg. Checking against this function type:
        ;      [Any Any
        ;       -> (HVec [(U nil Class) (U nil Class)]
        ;                :objects [{:path [Class], :id 0} {:path [Class], :id 1}])]
        ;     means we need to instantiate the HVec type to the actual argument
        ;     names with open-Result.
        ;
        ;     If the actual function method is (fn [a b] ...) we check against:
        ;
        ;       (HVec [(U nil Class) (U nil Class)]
        ;              :objects [{:path [Class], :id a} {:path [Class], :id b}])
        open-expected-rng (open-result/open-Result->TCResult
                            (:rng expected)
                            (map param-obj
                                 (concat required-params
                                         (some-> rest-param list)))
                            opts)
        open-expected-propositions (:fl open-expected-rng)
        _ (assert (fl/PropositionSet? open-expected-propositions))
        open-expected-rng-no-propositions (assoc open-expected-rng :fl (fo/-infer-proposition))
        _ (assert (r/TCResult? open-expected-rng-no-propositions))
        ;_ (prn "open-result open-expected-rng-no-propositions" open-expected-rng-no-propositions expected)
        ;_ (prn "open-result open-expected-rng filters" (some->> open-expected-rng-no-propositions :fl ((juxt :then :else)) (map fl/infer-top?)))
        ;ensure Function fits method
        _ (when-not (or ((case (:kind expected)
                           (:rest :drest :kws :prest :pdot) <=
                           :fixed =)
                         (count required-params) (count dom))
                        rest-param)
            (err/int-error (str "Checking method with incorrect number of expected parameters"
                                ", expected " (count dom) " required parameter(s) with"
                                (if rest " a " " no ") "rest parameter, found " (count required-params)
                                " required parameter(s) and" (if rest-param " a " " no ")
                                "rest parameter.")
                           opts))

        props (:props (lex/lexical-env opts))
        crequired-params (map (fn [p t] (assoc p u/expr-type (r/ret t)))
                              required-params
                              (concat dom 
                                      (repeat (or rest (:pre-type drest) prest (:pre-type pdot)))))
        _ (assert (every? (comp r/TCResult? u/expr-type) crequired-params))
        fixed-entry (map (juxt :name (comp r/ret-t u/expr-type)) crequired-params)
        ;_ (prn "checking function:" (prs/unparse-type expected opts))
        crest-param (some-> rest-param
                            (assoc u/expr-type (r/ret (check-rest-fn
                                                        (drop (count crequired-params) dom)
                                                        (select-keys expected [:rest :drest :kws :prest :pdot :kind])
                                                        opts))))
        rest-entry (when crest-param
                     [[(:name crest-param) (r/ret-t (u/expr-type crest-param))]])
        ;_ (prn "rest entry" rest-entry)
        _ (when (some? fixed-entry)
            (assert ((con/hash-c? symbol? r/Type?)
                     (into {} fixed-entry))
                    (into {} fixed-entry)))
        _ (when (some? rest-entry)
            (assert ((con/hash-c? symbol? r/Type?)
                     (into {} rest-entry))
                    (into {} rest-entry)))

        ; if this fn method is a multimethod dispatch method, then infer
        ; a new filter that results from being dispatched "here"
        mm-proposition (when-let [{:keys [dispatch-fn-type dispatch-val-ret]} (::multi-u/current-mm opts)]
                    (assert (and dispatch-fn-type dispatch-val-ret))
                    (assert (= :fixed (:kind expected)))
                    (assert (not rest-param))
                    (let [disp-app-ret (funapp/check-funapp nil nil 
                                                            (r/ret dispatch-fn-type)
                                                            (map r/ret dom (repeat (fo/-FS fl/-top fl/-top)) 
                                                                 (map param-obj required-params))
                                                            nil {} opts)
                          ;_ (prn "disp-app-ret" disp-app-ret)
                          ;_ (prn "disp-fn-type" (prs/unparse-type dispatch-fn-type opts))
                          ;_ (prn "dom" dom)
                          isa-ret (isa/tc-isa? disp-app-ret dispatch-val-ret nil opts)
                          then-proposition (-> isa-ret r/ret-f :then)
                          _ (assert then-proposition)]
                      then-proposition))
        ;_ (prn "^^^ mm-proposition" (::multi-u/current-mm opts))

        ;_ (prn "funapp1: inferred mm-proposition" mm-proposition)

        env (let [env (-> (lex/lexical-env opts)
                          ;add mm-proposition
                          (assoc :props (cond-> (set props) mm-proposition (conj mm-proposition)))
                          ;add parameters to scope
                          ;IF UNHYGIENIC order important, (fn [a a & a]) prefers rightmost name
                          (update :l merge (into {} fixed-entry) (into {} rest-entry)))
                  flag (volatile! true)
                  env (cond-> env
                        mm-proposition (update/env+ [mm-proposition] flag opts))]
              (when-not @flag
                (err/int-error "Unreachable method: Local inferred to be bottom when applying multimethod filter" opts))
              env)

        ; rng with inferred filters, and before manually inferring new filters
        crng-nopass
        (let [opts (-> opts
                       (assoc ::multi-u/current-mm nil)
                       (var-env/with-lexical-env env))
              rec (or ; if there's a custom recur behaviour, use the provided
                        ; keyword argument to generate the RecurTarget.
                        (when recur-target-fn
                          (recur-target-fn expected))
                        ; Otherwise, assume we are checking a regular `fn` method
                        (recur-u/RecurTarget-maker dom rest drest nil))
              _ (assert (recur-u/RecurTarget? rec))]
          (let [opts (recur-u/with-recur-target opts rec)]
            (check-expr body open-expected-rng-no-propositions opts)))

        ; Apply the filters of computed rng to the environment and express
        ; changes to the lexical env as new filters, and conjoin with existing filters.

        flag (volatile! true)
        then-env (let [{:keys [then]} (-> crng-nopass u/expr-type r/ret-f)]
                   (cond-> env
                     (not (fl/NoProposition? then))
                     (update/env+ [then] flag opts)))
        ;TODO
        ;_ (when-not @flag
        ;    (err/int-error "Unreachable method: Local inferred to be bottom when applying multimethod filter" opts))
        new-then-props (reduce-kv (fn [fs sym t]
                                    {:pre [((con/set-c? fl/Proposition?) fs)]}
                                    (cond-> fs
                                      (not= t (get-in env [:l sym]))
                                      ;new type, add positive proposition
                                      ;(otherwise, type hasn't changed, no new propositions)
                                      (conj (fo/-proposition-at t (lex/lookup-alias sym {:env env} opts)))))
                                  #{}
                                  (:l then-env))

        crng+inferred-propositions (update-in crng-nopass [u/expr-type :fl :then]
                                         (fn [f]
                                           (fo/-and (cons f new-then-props) opts)))
        ;_ (prn "open-expected-propositions" open-expected-propositions)
        crng (if (= open-expected-propositions (fo/-infer-proposition))
               ;; infer mode
               crng+inferred-propositions
               ;; check actual filters and fill in expected filters
               (let [{actual-propositions :fl :as actual-ret} (u/expr-type crng+inferred-propositions)
                     _ (when-not (below/filter-better? actual-propositions open-expected-propositions opts)
                         (below/bad-proposition-delayed-error
                           actual-ret
                           (assoc open-expected-rng-no-propositions :fl open-expected-propositions)
                           opts))]
                 (assoc-in crng+inferred-propositions [u/expr-type :fl] open-expected-propositions)))
        ;_ (prn "crng" (u/expr-type crng))
        rest-param-name (some-> rest-param :name)

        ftype (fn-method-u/FnResult->Function
                (fn-method-u/FnResult-maker
                  fixed-entry
                  (when (and kws rest-param)
                    [rest-param-name kws])
                  (when (and rest rest-param)
                    [rest-param-name rest])
                  (when (and drest rest-param)
                    [rest-param-name drest])
                  (when (and prest rest-param)
                    [rest-param-name prest])
                  (when (and pdot rest-param)
                    [rest-param-name pdot])
                  (u/expr-type crng))
                opts)
        _ (assert (r/Function? ftype))
                        
        cmethod (-> method
                    (assoc (ast-u/method-body-kw) crng
                           :clojure.core.typed/ftype ftype)
                    (ast-u/reconstruct-arglist crequired-params crest-param opts))
        _ (assert (vector? (:params cmethod)))
        _ (assert (every? (comp r/TCResult? u/expr-type) (:params cmethod)))]
     {:ftype ftype
      :cmethod cmethod}))
