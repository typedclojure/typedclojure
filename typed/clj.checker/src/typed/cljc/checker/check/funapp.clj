;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.funapp
  (:require [typed.clojure :as t]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.set :as set]
            [typed.cljc.checker.open-result :as open-result]
            [typed.clj.checker.experimental.infer-vars :as infer-vars]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.check-below :as below]
            [typed.cljc.checker.check :refer [check-expr]]
            [typed.cljc.checker.check.app-error :as app-err]
            [typed.cljc.checker.check.funapp-one :as funapp1]
            [typed.cljc.checker.check.invoke-kw :as invoke-kw]
            [typed.cljc.checker.check.utils :as cu]
            [typed.cljc.checker.cs-rep :as crep]
            [typed.cljc.checker.cs-gen :as cgen]
            [typed.cljc.checker.filter-ops :as fops]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.frees :as frees]
            [typed.cljc.checker.hset-utils :as hset]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.runtime.env :as env]))

(def ^:private nth-type #((requiring-resolve 'typed.cljc.checker.check.nth/nth-type) %1 %2 %3))

; Expr Expr^n TCResult TCResult^n (U nil TCResult) -> TCResult
(t/ann check-funapp [t/Any t/Any t/Any t/Any t/Any t/Any :? :-> t/Any])
;TODO HeterogeneousMap case, see typed-test.cljc.checker.check.funapp/invoke-HMap-test
(defn check-funapp
  ([fexpr args fexpr-ret-type arg-ret-types expected]
   (check-funapp fexpr args fexpr-ret-type arg-ret-types expected nil))
  ([fexpr args fexpr-ret-type arg-ret-types expected {:keys [expr] :as opt}]
   {:pre [(r/TCResult? fexpr-ret-type)
          (every? r/TCResult? arg-ret-types)
          ((some-fn nil? r/TCResult?) expected)]
    :post [(r/TCResult? %)]}
   (let [fexpr-type (c/fully-resolve-type (r/ret-t fexpr-ret-type))
         arg-types (mapv r/ret-t arg-ret-types)]
     (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                              (some-> fexpr cu/expr-ns))
     ;(prn "check-funapp" (prs/unparse-type fexpr-type) (map prs/unparse-type arg-types) (some-> expected prs/unparse-type))
     (cond
       ;; a union of functions can be applied if we can apply all of the elements
       (r/Union? fexpr-type)
       (r/ret (reduce (fn [t ftype]
                        {:pre [(r/Type? t)
                               (r/Type? ftype)]
                         :post [(r/Type? %)]}
                        (c/Un t (r/ret-t (check-funapp fexpr args (r/ret ftype) arg-ret-types expected opt))))
                      (c/Un)
                      (:types fexpr-type)))

       ; try the first thing that looks like a Fn.
       ; FIXME This should probably try and invoke every Fn it can
       ; find, need to figure out how to clean up properly
       ; after a failed invocation.
       (r/Intersection? fexpr-type)
       (let [a-fntype (first (filter
                               (fn [t]
                                 ;; would be nice to recur on check-funapp on each of these
                                 ;; instead of this hack to find a function type.
                                 (or (r/FnIntersection? t)
                                     (r/HeterogeneousVector? t)
                                     (c/keyword-value? t)
                                     (c/ifn-ancestor t)
                                     (r/Poly? t)))
                               (map c/fully-resolve-type (:types fexpr-type))))]
         (if a-fntype
           (check-funapp fexpr args (r/ret a-fntype) arg-ret-types expected opt)
           (err/tc-delayed-error (str "Cannot invoke type: " (pr-str (prs/unparse-type fexpr-type))))))

       (c/ifn-ancestor fexpr-type)
       (check-funapp fexpr args (r/ret (c/ifn-ancestor fexpr-type)) arg-ret-types expected opt)

       ;keyword function
       (c/keyword-value? fexpr-type)
       (let [[target-ret default-ret & more-args] arg-ret-types]
         (assert (empty? more-args))
         (invoke-kw/invoke-keyword nil fexpr-ret-type target-ret default-ret expected))

       ;set function
       ;FIXME yuck. Also this is wrong, should be APersistentSet or something that *actually* extends IFn
       (and (r/RClass? fexpr-type)
            (isa? (coerce/symbol->Class (:the-class fexpr-type)) 
                  clojure.lang.IPersistentSet))
       (do
         (when-not (= 1 (count args))
           (err/tc-delayed-error (str "Wrong number of arguments to set function (" (count args)")")))
         (below/maybe-check-below
           (r/ret r/-any)
           expected))

       ;FIXME same as IPersistentSet case
       (and (r/RClass? fexpr-type)
            (isa? (coerce/symbol->Class (:the-class fexpr-type)) clojure.lang.IPersistentMap))
       ;rewrite ({..} x) as (f {..} x), where f is some dummy fn
       (let [mapfn (prs/parse-type `(t/All [x#] [(t/Map t/Any x#) t/Any :-> (t/U nil x#)]))]
         (check-funapp fexpr args (r/ret mapfn) (concat [fexpr-ret-type] arg-ret-types) expected opt))

       ;FIXME same as IPersistentSet case
       (and (r/RClass? fexpr-type)
            (isa? (coerce/symbol->Class (:the-class fexpr-type)) clojure.lang.IPersistentVector))
       ;rewrite ({..} x) as (f {..} x), where f is some dummy fn
       (let [mapfn (prs/parse-type (let [x 'x
                                         y 'y]
                                     `(t/All [~x ~y]
                                             (t/IFn [(t/Vec ~x) t/Int :-> ~x]
                                                    [(t/Vec ~x) t/Int ~y :-> (t/U ~y ~x)]))))]
         (check-funapp fexpr args (r/ret mapfn) (cons fexpr-ret-type arg-ret-types) expected opt))

       ;Symbol function
       (and (r/RClass? fexpr-type)
            (= 'clojure.lang.Symbol (:the-class fexpr-type)))
       (let [symfn (prs/parse-type `(t/All [x#] [(t/U (t/Map t/Any x#) t/Any) :-> (t/U x# nil)]))]
         (check-funapp fexpr args (r/ret symfn) arg-ret-types expected opt))
       
       ;Var function
       (and (r/RClass? fexpr-type)
            (= 'clojure.lang.Var (:the-class fexpr-type)))
       (let [{[_ ftype :as poly?] :poly?} fexpr-type
             _ (assert (= 2 (count poly?))
                       "Assuming clojure.lang.Var only takes 1 argument")]
         (check-funapp fexpr args (r/ret ftype) arg-ret-types expected opt))

       ;Error is perfectly good fn type
       (r/TCError? fexpr-type)
       (below/maybe-check-below
         (r/ret r/Err)
         expected)

       ; Unchecked function is upcast to anything so we don't check arguments,
       ; but return Unchecked or expected.
       (r/Unchecked? fexpr-type)
       (let [{:keys [vsym]} fexpr-type]
         (when vsym
           (infer-vars/add-inferred-type
             (or prs/*unparse-type-in-ns*
                 (when fexpr
                   (cu/expr-ns fexpr)))
             vsym
             (r/make-FnIntersection
               (r/make-Function
                 (repeat (count args) r/-any)
                 r/-any))))
         (or expected
             (r/ret (r/-unchecked nil))))

       (r/HeterogeneousVector? fexpr-type)
       (or (when (= 1 (count arg-types))
             (let [i (first arg-types)
                   {idx :val} (when (r/Value? i) i)]
               (when (integer? idx)
                 (below/maybe-check-below
                   ;; FIXME replace with path-type?
                   (r/ret (nth-type [fexpr-type] idx nil))
                   expected))))
           (check-funapp fexpr args (assoc fexpr-ret-type :t (c/upcast-HSequential fexpr-type)) arg-ret-types expected opt))

       (r/HSet? fexpr-type)
       (let [fixed (:fixed fexpr-type)
             ret (if (not= 1 (count arg-ret-types))
                   (err/tc-delayed-error (str "Expected 1 argument to set, given " (count args) ".")
                                         :return (r/ret r/Err))
                   (let [[argt] arg-ret-types
                         ; default value is nil
                         set-return (apply c/Un r/-nil fixed)]
                     (if (and (:complete? fexpr-type)
                              (every? (every-pred
                                        r/Value?
                                        (comp hset/valid-fixed? :val))
                                      fixed))
                       (let [filter-type (apply c/Un
                                                (disj (r/sorted-type-set fixed) 
                                                      (r/-val nil)
                                                      (r/-val false)))]
                         (r/ret set-return
                                (fops/-FS
                                  (fops/-filter-at filter-type (r/ret-o argt))
                                  (fops/-not-filter-at filter-type (r/ret-o argt)))))
                       (r/ret set-return))))]
         (below/maybe-check-below
           ret
           expected))

   ; FIXME error messages are worse here because we don't use line numbers for
   ; specific arguments
       ;ordinary Function, single case, special cased for improved error msgs
;       (and (r/FnIntersection? fexpr-type)
;            (let [[{:keys [drest] :as ft} :as ts] (:types fexpr-type)]
;              (and (= 1 (count ts))
;                   (not drest))))
;       ; check/funapp-single-arity-nopoly-nodots
;       (let [argtys arg-ret-types
;             {[t] :types} fexpr-type]
;         (funapp1/check-funapp1 fexpr args t argtys expected))

       ;ordinary Function, multiple cases
       (r/FnIntersection? fexpr-type)
       (let [ftypes (:types fexpr-type)
             matching-fn (some (fn [{:keys [dom rest kws prest] :as f}]
                                 {:pre [(r/Function? f)
                                        (case (:kind f)
                                          (:fixed :rest :kws :prest) true
                                          false)]}
                                 #_
                                 (prn "arg-ret-types"
                                      (mapv (comp #(mapv (juxt :env typed.clj.analyzer.passes.emit-form/emit-form) %) :origin-exprs meta)
                                            arg-ret-types))
                                 (when (if prest
                                         (sub/subtypes-prest? arg-types dom prest)
                                         (sub/subtypes-varargs? arg-types dom rest kws))
                                   f))
                               ftypes)
             success-ret-type (when matching-fn
                                (funapp1/check-funapp1 fexpr args matching-fn arg-ret-types expected :check? false))]
         ;(prn "success-ret-type" success-ret-type)
         (or success-ret-type
             (app-err/plainapp-type-error fexpr args fexpr-type arg-ret-types expected)))

       (r/SymbolicClosure? fexpr-type)
       (let [capp (sub/check-symbolic-closure
                    fexpr-type
                    (r/make-FnIntersection
                      (r/make-Function arg-types
                                       ;; blame application site for return errors (good strategy?)
                                       r/-wild)))
             actual-ret-ifn (-> capp u/expr-type :t)
             fni (cond
                   (r/FnIntersection? actual-ret-ifn) actual-ret-ifn
                   (r/Intersection? actual-ret-ifn) (first (filter r/FnIntersection? (:types actual-ret-ifn))))
             _ (assert (r/FnIntersection? fni))
             _ (assert (= 1 (count (:types fni))))
             res (-> fni :types first :rng
                     (open-result/open-Result->TCResult
                       (map :o arg-ret-types)
                       (map :t arg-ret-types)))]
         (below/maybe-check-below
           res
           expected))

       ;ordinary polymorphic function without dotted rest
       (when (r/Poly? fexpr-type)
         (let [names (c/Poly-fresh-symbols* fexpr-type)
               body (c/Poly-body* names fexpr-type)]
           (when (r/FnIntersection? body)
             (not-any? (some-fn :drest :pdot) (:types body)))))
       (let [fs-names (c/Poly-fresh-symbols* fexpr-type)
             _ (assert (every? symbol? fs-names))
             fin (c/Poly-body* fs-names fexpr-type)
             bbnds (c/Poly-bbnds* fs-names fexpr-type)
             _ (assert (r/FnIntersection? fin))
             ;; Only infer free variables in the return type
             ret-type
             (free-ops/with-bounded-frees (zipmap (map r/F-maker fs-names) bbnds)
               (let [fs-names->bbnds (zipmap fs-names bbnds)
                     expected-t (some-> expected r/ret-t c/fully-resolve-type)]
                 (loop [[{:keys [dom rng rest drest kws prest] :as ftype} & ftypes] (:types fin)]
                   (when ftype
                     ;; only try inference if argument types are appropriate
                     (if-let
                       [substitution
                        (cgen/handle-failure
                          (cond
                            ;possibly present rest argument, or no rest parameter
                            (and (not (or drest kws prest))
                                 ((if rest <= =) (count dom) (count arg-types)))
                            (cgen/infer-vararg fs-names->bbnds {}
                                               arg-types dom rest
                                               (r/Result-type* rng)
                                               (some-> expected r/ret-t)
                                               {:expr expr})

                            (and prest
                                 (<= (count dom) (count arg-types)))
                            (cgen/infer-prest fs-names->bbnds {}
                                              arg-types dom prest
                                              (r/Result-type* rng) expected-t
                                              {:expr expr})

                            ;keyword parameters
                            kws
                            (let [{:keys [mandatory optional]} kws
                                  [normal-argtys flat-kw-argtys] (split-at (count dom) arg-types)
                                  _ (when (odd? (count flat-kw-argtys))
                                      ; move to next arity
                                      (cgen/fail! nil nil)
                                      #_(err/int-error (str "Uneven number of keyword arguments "
                                                            "provided to polymorphic function "
                                                            "with keyword parameters.")))
                                  paired-kw-argtys (apply hash-map flat-kw-argtys)

                                  ;generate two vectors identical in length with actual kw val types
                                  ;on the left, and expected kw val types on the right.

                                  [kw-val-actual-tys kw-val-expected-tys]
                                  (reduce (fn [[kw-val-actual-tys kw-val-expected-tys]
                                               [kw-key-t kw-val-t]]
                                            {:pre [(vector? kw-val-actual-tys)
                                                   (vector? kw-val-expected-tys)
                                                   (r/Type? kw-key-t)
                                                   (r/Type? kw-val-t)]
                                             :post [((con/hvector-c? (every-pred vector? (con/every-c? r/Type?)) 
                                                                     (every-pred vector? (con/every-c? r/Type?)))
                                                     %)]}
                                            (when-not (r/Value? kw-key-t)
                                              ; move to next arity
                                              (cgen/fail! nil nil)
                                              #_(err/int-error 
                                                (str "Can only check keyword arguments with Value keys, found"
                                                     (pr-str (prs/unparse-type kw-key-t)))))
                                            (if-some [expected-val-t ((some-fn optional mandatory) kw-key-t)]
                                              [(conj kw-val-actual-tys kw-val-t)
                                               (conj kw-val-expected-tys expected-val-t)]
                                              (do 
                                                ; Using undeclared keyword keys is an error because we want to treat
                                                ; the rest param as a complete hash map when checking 
                                                ; fn bodies.
                                                (err/tc-delayed-error (str "Undeclared keyword parameter " 
                                                                           (pr-str (prs/unparse-type kw-key-t))))
                                                [(conj kw-val-actual-tys kw-val-t)
                                                 (conj kw-val-expected-tys r/-any)])))
                                          [[] []]
                                          paired-kw-argtys)]
                              ;make sure all mandatory keys are present
                              (when-some [missing-ks (not-empty
                                                       (apply disj (set (keys mandatory)) (keys paired-kw-argtys)))]
                                ; move to next arity
                                (cgen/fail! nil nil))
                                ;(err/tc-delayed-error (str "Missing mandatory keyword keys: "
                                ;                         (pr-str (vec (interpose ", "
                                ;                                                 (map prs/unparse-type missing-ks))))))
                              ;; it's probably a bug to not infer for unused optional args, revisit this
                              ;(when-let [missing-optional-ks (seq
                              ;                                 (set/difference (set (keys optional))
                              ;                                                 (set (keys paired-kw-argtys))))]
                              ;  (err/nyi-error (str "NYI POSSIBLE BUG?! Unused optional parameters"
                              ;                    (pr-str (interpose ", " (map prs/unparse-type missing-optional-ks)))))
                              ;  )
                              ; infer keyword and fixed parameters all at once
                              (cgen/infer fs-names->bbnds {}
                                          (concat normal-argtys kw-val-actual-tys)
                                          (concat dom kw-val-expected-tys) 
                                          (r/Result-type* rng)
                                          (some-> expected r/ret-t)
                                          {:expr expr}))))]
                       (if (r/SymbolicClosure? substitution)
                         (r/ret substitution)
                         (let [;_ (prn "subst:" substitution)
                               new-ftype (subst/subst-all substitution ftype)]
                           ;(prn "substituted type" new-ftype)
                           (funapp1/check-funapp1 fexpr args new-ftype
                                                  arg-ret-types expected :check? false)))
                       (if drest
                         (do (err/tc-delayed-error (str "Cannot infer arguments to polymorphic functions with dotted rest"))
                             nil)
                         (recur ftypes)))))))]
         (or ret-type
             (app-err/polyapp-type-error fexpr args fexpr-type arg-ret-types expected)))

       :else ;; any kind of dotted polymorphic function without mandatory or optional keyword args
       (if-let [[pbody fixed-map dotted-map]
                (letfn [(should-infer? [t]
                          (and (r/PolyDots? t)
                               (r/FnIntersection?
                                 (c/PolyDots-body* (c/PolyDots-fresh-symbols* t)
                                                   t))))
                        (collect-polydots [t]
                          {:post [((con/hvector-c? r/Type?
                                                   (con/hash-c? symbol? r/Bounds?)
                                                   (con/hash-c? symbol? r/Regex?))
                                   %)]}
                          (loop [pbody (c/fully-resolve-type t)
                                 fixed {}
                                 dotted {}]
                            (cond 
                              (r/PolyDots? pbody)
                              (let [vars (vec (c/PolyDots-fresh-symbols* pbody))
                                    bbnds (c/PolyDots-bbnds* vars pbody)
                                    pbody (c/PolyDots-body* vars pbody)]
                                (recur (c/fully-resolve-type pbody)
                                       (reduce (fn [fixed i]
                                                 (assoc fixed (nth vars i) (nth bbnds i)))
                                               fixed (range (dec (count vars))))
                                       (assoc dotted (peek vars) (peek bbnds))))

                              (and (r/FnIntersection? pbody)
                                   (seq (:types pbody))
                                   (not-any? :kws (:types pbody)))
                              [pbody fixed dotted])))]
                  ; don't support nested PolyDots yet
                  (when (should-infer? fexpr-type)
                    (collect-polydots fexpr-type)))]
         (let [;_ (prn "polydots, no kw args")
               _ (assert (= 1 (count dotted-map)))
               inferred-rng
               (free-ops/with-bounded-frees (update-keys fixed-map r/make-F)
                 ;(dvar-env/with-dotted-mappings (zipmap (keys dotted-map) (map r/make-F (vals dotted-map)))
                 (some (fn [{:keys [dom rest drest rng prest pdot] :as ftype}]
                         ;only try inference if argument types match
                         (when (cond
                                 rest (<= (count dom) (count arg-types))
                                 drest (and (<= (count dom) (count arg-types))
                                            (contains? (set (keys dotted-map)) (-> drest :name)))
                                 prest (<= (count dom) (count arg-types))
                                 pdot (and (<= (count dom) (count arg-types))
                                           (contains? (set (keys dotted-map)) (-> pdot :name)))
                                 :else (= (count dom) (count arg-types)))
                           (cgen/handle-failure
                             ;(prn "Inferring dotted fn" (prs/unparse-type ftype))
                             ;; Only try to infer the free vars of the rng (which includes the vars
                             ;; in filters/objects).
                             (let [expected-t (some-> expected r/ret-t)
                                   rng-t (r/Result-type* rng)
                                   substitution (cond
                                                  drest (let [de (first dotted-map)]
                                                          (cgen/infer-dots fixed-map (key de) (val de)
                                                                           arg-types dom (:pre-type drest) rng-t
                                                                           (frees/fv rng)
                                                                           :expected expected-t
                                                                           :expr expr))

                                                  rest (cgen/infer-vararg fixed-map dotted-map
                                                                          arg-types dom rest rng-t
                                                                          expected-t {:expr expr})

                                                  (and prest
                                                       (<= (count dom) (count arg-types)))
                                                  (cgen/infer-prest fixed-map dotted-map
                                                                    arg-types dom prest rng-t
                                                                    expected-t {:expr expr})

                                                  pdot (let [de (first dotted-map)]
                                                         (cgen/infer-pdot fixed-map (key de) (val de)
                                                                          arg-types dom (:pre-type pdot) rng-t
                                                                          (frees/fv rng)
                                                                          :expected expected-t
                                                                          #_;;TODO
                                                                          {:expr expr}))

                                                  :else (cgen/infer fixed-map dotted-map
                                                                    arg-types dom rng-t
                                                                    expected-t {:expr expr}))
                                   ;_ (prn "substitution:" substitution)
                                   substituted-type (cond-> substitution
                                                      (not (r/SymbolicClosure? substitution))
                                                      (subst/subst-all ftype))
                                   ;_ (prn "substituted-type" (prs/unparse-type substituted-type))
                                   ;_ (prn "args" (map prs/unparse-type arg-types))
                                   ]
                               (or (when (r/SymbolicClosure? substituted-type)
                                     (r/ret substituted-type (fops/-true-filter)))
                                   (and substitution
                                        (funapp1/check-funapp1 fexpr args 
                                                               substituted-type arg-ret-types expected :check? false))
                                   (err/tc-delayed-error "Error applying dotted type")
                                   nil)))))
                       (:types pbody)))]
           ;(prn "inferred-rng"inferred-rng)
           (or inferred-rng
               (app-err/polyapp-type-error fexpr args fexpr-type arg-ret-types expected)))

         (let [opts (:opts fexpr-ret-type)]
           ;(prn `check-funapp (class fexpr-type))
           (err/tc-delayed-error (str "Cannot invoke type: " (pr-str (prs/unparse-type fexpr-type)))
                                 :return (or expected (r/ret (c/Un)))))))))))
