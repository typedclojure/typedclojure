;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.cljc.checker.check.fn-methods
  (:refer-clojure :exclude [methods rest])
  (:require [typed.cljc.checker.type-rep :as r]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.ast-utils :as ast-u]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.utils :as u]
            [typed.cljc.checker.check.utils :as cu]
            [clojure.core.typed.errors :as err]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.lex-env :as lex]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.check.fn-method-one :as fn-method1]))

(def function-type? (some-fn r/FnIntersection?
                             (every-pred r/Poly?
                                         (comp r/FnIntersection? r/Poly-body-unsafe*))
                             (every-pred r/PolyDots?
                                         (comp r/FnIntersection? r/PolyDots-body-unsafe*))
                             r/TCError?))

(def function-type-union? #(every? function-type?
                                   (if (r/Union? %)
                                     (let [ts (:types %)]
                                       (assert (seq ts))
                                       ts)
                                     [%])))

(def method?
  (fn [v opts]
    (or (ast-u/fn-method? v)
        (ast-u/deftype-method? v opts))))
(def methods?
  (fn [ms opts]
    (impl/impl-case opts
      :clojure ((con/vec-c? #(method? % opts)) ms)
      :cljs (every? #(method? % opts) ms))))

(def opt-map? (con/hmap-c? (con/optional :recur-target-fn) ifn?
                           (con/optional :check-rest-fn) ifn?
                           (con/optional :validate-expected-fn) ifn?
                           (con/optional :self-name) (some-fn nil? symbol?)))

(defn method-return? [opts]
  (con/hmap-c?
    :ifn function-type-union?
    :methods #(methods? % opts)
    :cmethods #(methods? % opts)))

(defn method->fixed-arity [{:keys [fixed-arity] :as method}]
  (cond-> fixed-arity
    (= :method (:op method)) inc))

(defn expected-for-method
  "Takes a :fn-method or :method AST node and a single Function arity type,
  and returns the Function if the :method node should be checked
  against the Function, otherwise returns nil."
  [{:keys [variadic?] :as method}
   {:keys [dom rest drest kws pdot prest] :as f}
   all-methods
   opts]
  {:pre [(method? method opts)
         (r/Function? f)]
   :post [((some-fn nil? r/Function?) %)]}
  ;; fn-method-u/check-rest-fn, and check-fn-method1
  ;; actually distribute the types amongst the fixed and rest parameters
  (assert (not rest)) ;handled by check-fni
  (let [ndom (count dom)
        fixed-arity (method->fixed-arity method)]
    (cond
      (or rest drest pdot prest)
      (cond
        ; extra domains flow into the rest argument
        variadic? (when (<= fixed-arity ndom)
                    f))

      ; kw and drest functions must have exact fixed domain match
      (or kws drest)
      (cond
        variadic? (when (= ndom fixed-arity) f))

      ; no variable arity
      (= nil rest drest kws pdot prest)
      (cond
        ;; ensure no other fixed arities would match this Function
        variadic? (when (not-any? (fn [m]
                                    (and (not (:variadic? m))
                                         (= ndom (:fixed-arity m))))
                                  all-methods)
                    ; extra domains flow into the rest argument
                    (when (<= fixed-arity ndom)
                      f))
        (= ndom fixed-arity) f))))

(defn check-fni
  "Check a vector of :method AST nodes mthods against
  an expected type that is a possibly-polymorphic function
  intersection.
  
  Returns a vector in the same order as the passed in methods,
  but each method replaced with a vector of type checked methods."
  [expected mthods
   {:keys [validate-expected-fn
           self-name]
    :as opt}
   opts]
  {:pre [(function-type? expected)
         (methods? mthods opts)
         (opt-map? opt)]
   :post [((method-return? opts) %)]}
  (let [; unwrap polymorphic expected types
        [fin inst-frees bnds poly?] (cu/unwrap-poly expected opts)
        ; this should never fail due to function-type? check
        _ (assert (r/FnIntersection? fin))
        _ (when validate-expected-fn
            (validate-expected-fn fin))

        out-fn-matches (atom (vec (repeat (count (:types fin)) nil)))
        ;; cmethodss is a vector in the same order as the passed in methods,
        ;; but each method replaced with a vector of type checked methods."
        cmethodss
        (let [opts (lex/with-locals opts (some-> self-name (hash-map expected)))]
          ;scope type variables from polymorphic type in body
          (free-ops/with-free-mappings (zipmap (map r/F-original-name inst-frees)
                                               (map #(hash-map :F %1 :bnds %2) inst-frees bnds))
            (let [;; ordered pairs from function type to a map of matching methods (integers) to expected types.
                  fn-matches
                  (into []
                        (map (fn [{:keys [dom rest drest kws pdot prest] :as t}]
                               {:pre [(r/Function? t)]}
                               (let [ms (cond
                                          rest (let [ndom (count dom)
                                                     fixed->mth (into (sorted-map)
                                                                      (map-indexed
                                                                        (fn [i m]
                                                                          (let [fixed-arity (method->fixed-arity m)]
                                                                            (when (or (:variadic? m)
                                                                                      (<= ndom fixed-arity))
                                                                              {fixed-arity (assoc m ::method-pos i)}))))
                                                                      mthods)
                                                     [max-fixed variadic] (-> fixed->mth rseq first)]
                                                 (if-not (:variadic? variadic)
                                                   (err/tc-delayed-error (str "Variadic method is required to satisfy type: " (prs/unparse-type t opts))
                                                                         {:return {}}
                                                                         opts)
                                                   (if (<= max-fixed ndom)
                                                     ; extra domains flow into the rest argument via fn-method-u/check-rest-fn 
                                                     (do (assert (= 1 (count fixed->mth))) ;; must be just the variadic method in the case
                                                         (into {}
                                                               (map (juxt ::method-pos (constantly t)))
                                                               (vals fixed->mth)))
                                                     ;; rest type may flow into unrolled positional args after dom if
                                                     ;; fixed arities monotonically increase between methods
                                                     ;; with equal or more number of fixed args than dom
                                                     (let [min-fixed (-> fixed->mth first key)
                                                           expected-arities (into #{} (range min-fixed (inc max-fixed)))
                                                           actual-arities (into #{} (keys fixed->mth))]
                                                       (if-some [missing-arities (not-empty (set/difference expected-arities actual-arities))]
                                                         (err/tc-delayed-error
                                                           (str "Missing fn method(s) with "
                                                                (str/join ", " (sort missing-arities))
                                                                " fixed argument(s) to satisfy type: "
                                                                (prs/unparse-type t opts))
                                                           {:return {}}
                                                           opts)
                                                         (into {}
                                                               (map (fn [[fixed mth]]
                                                                      [(::method-pos mth)
                                                                       (-> t
                                                                           (assoc :dom (into (vec dom) (repeat (- fixed ndom) rest)))
                                                                           (cond-> (not (:variadic? mth)) (assoc :rest nil)))]))
                                                               fixed->mth))))))
                                          ;; treat each method as separate functions
                                          :else (let [ms (into {}
                                                               (keep-indexed (fn [i m]
                                                                               (when (expected-for-method m t mthods opts)
                                                                                 [i t])))
                                                               mthods)]
                                                  ;; it is a type error if no matching methods are found.
                                                  (when (empty? ms)
                                                    (let [opts (assoc opts ::vs/current-env (impl/impl-case opts
                                                                                              :clojure (first mthods)
                                                                                              ; fn-method is not printable in cljs
                                                                                              :cljs vs/*current-expr*))]
                                                      (let [opts (update opts ::vs/current-env #(or (:env (first mthods)) %))]
                                                        (prs/with-unparse-ns (cu/expr-ns (first mthods) opts)
                                                          (err/tc-delayed-error (str "No matching arities: " (prs/unparse-type t opts)) opts)))))
                                                  ms))]
                                 [t ms])))
                        (:types fin))]
              ;; if a method occurs more than once in the entire map, it will be
              ;; checked twice, so we disable rewriting for that method.
              (into []
                    (map-indexed
                      (fn [method-index m]
                        (let [expecteds
                              (keep
                                (fn [[ifn-index [_ifn-arity relevant-method-idxs]]]
                                  (when-some [expected-type (relevant-method-idxs method-index)] 
                                    [ifn-index expected-type]))
                                (map-indexed vector fn-matches))
                              cmethods
                              (let [opts (update opts ::vs/can-rewrite #(and (= 1 (count expecteds)) %))]
                                (mapv (fn [[ifn-index expected-type]]
                                        {:pre [(integer? ifn-index)
                                               (r/Function? expected-type)]}
                                        (let [{:keys [cmethod ftype]}
                                              (fn-method1/check-fn-method1 m expected-type
                                                                           (select-keys opt [:recur-target-fn :check-rest-fn])
                                                                           opts)
                                              _ (assert (r/Function? ftype))
                                              union-Functions (fn [f1 f2]
                                                                {:pre [(r/Function? f1)
                                                                       (r/Function? f2)]}
                                                                (update f1 :rng c/union-Results (:rng f2) opts))
                                              _ (swap! out-fn-matches update ifn-index
                                                       (fn [old]
                                                         {:pre [(or (nil? old) (r/Function? old))]
                                                          :post [(r/Function? %)]}
                                                         (if old
                                                           (union-Functions old ftype)
                                                           ftype)))]
                                          cmethod))
                                      expecteds))]
                          cmethods)))
                    mthods))))

        out-fn-matches @out-fn-matches
        ;; if we infer the body of any return types, we now gather those inferred types.
        inferred-ifn (apply r/make-FnIntersection 
                            (map-indexed (fn [i match]
                                           {:post [(r/Function? %)]}
                                           (or match
                                               (get (:types fin) i)))
                                           out-fn-matches))
        maybe-poly-inferred-ifn (case poly?
                                  :Poly (c/Poly* (map :name inst-frees) bnds inferred-ifn opts)
                                  :PolyDots (c/PolyDots* (map :name inst-frees) bnds inferred-ifn opts)
                                  nil inferred-ifn)]
     ;; if a method is checked only once, then it could have
     ;; been rewritten, so propagate it up to the rest of the
     ;; AST.
    {:methods (mapv 
                (fn [cmethods m]
                  (if (== 1 (count cmethods))
                    (nth cmethods 0)
                    m))
                cmethodss
                mthods)
     :ifn maybe-poly-inferred-ifn
     ;; flatten out all checked methods
     :cmethods (into []
                     (mapcat identity)
                     cmethodss)}))

(defn function-types [expected opts]
  {:pre [(r/Type? expected)]
   :post [(and (every? function-type? %)
               (vector? %))]}
  (let [exp (c/fully-resolve-type expected opts)
        ts (filterv function-type?
                    (if (r/Union? exp)
                      (:types exp)
                      [exp]))]
    ts))

; Check a sequence of methods against a (possibly polymorphic) function type.
;
; If this is a deftype/reify method, provide a (:recur-target-fn opt) to handle recur behaviour
; and validate-expected-fn to prevent expected types that include a rest argument. Also provide (:check-rest-fn opt)
; to disallow rest parameter.
;
; (ann check-fn-methods [Expr Type (HMap :optional {:recur-target-fn (Nilable [Function -> RecurTarget])
;                                                   :validate-expected-fn (Nilable [FnIntersection -> Any])})
;                        Opts
;                        -> (Coll FnMethod)])
(defn check-fn-methods [mthods expected opt opts]
  {:pre [(r/Type? expected)
         ((every-pred #(methods? % opts) seq) mthods)
         (opt-map? opt)]
   :post [((method-return? opts) %)]}
  (let [ts (function-types expected opts)]
    (cond
      (empty? ts)
      (prs/with-unparse-ns (cu/expr-ns (first mthods) opts)
        (err/tc-delayed-error (str (pr-str (prs/unparse-type expected opts)) " is not a function type")
                              {:return {:methods mthods
                                        :ifn r/-error
                                        :cmethods []}}
                              opts))
      
      (= 1 (count ts))
      (check-fni (nth ts 0) mthods opt opts)

      ;; disable rewriting in case we recheck a method arity
      :else
      (let [opts (assoc opts ::vs/can-rewrite nil)
            method-returns+errors (mapv (fn [t]
                                          (let [delayed-errors (err/-init-delayed-errors)]
                                            (let [res (check-fni t mthods opt (assoc opts ::vs/delayed-errors delayed-errors))]
                                              {:errors (seq @delayed-errors)
                                               :res res})))
                                        ts)
            _ (when (every? :errors method-returns+errors)
                (swap! (::vs/delayed-errors opts) into (mapcat :errors) method-returns+errors))]
        {:methods mthods
         :ifn (c/Un (map (comp :ifn :res) method-returns+errors) opts)
         :cmethods (into [] (mapcat (comp :cmethods :res)) method-returns+errors)}))))
