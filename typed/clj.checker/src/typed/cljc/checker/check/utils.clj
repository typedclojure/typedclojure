;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.cljc.checker.check.utils
  (:require [clojure.core.typed :as t]
            [typed.cljc.checker.object-rep :as obj]
            [typed.cljc.checker.utils :as u]
            [typed.clj.checker.analyze-clj :as ana]
            [typed.cljc.checker.ns-deps-utils :as ns-depsu]
            [typed.clj.checker.reflect-utils :as reflect-u]
            [clojure.core.typed.errors :as err]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.datatype-env :as dt-env]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [typed.clj.checker.parse-unparse :as prs]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.path-rep :as pe]
            [clojure.core.typed.util-vars :as vs]
            [typed.cljc.checker.protocol-env :as pcl-env]
            [typed.clj.checker.method-param-nilables :as mtd-param-nil]
            [typed.clj.checker.method-return-nilables :as mtd-ret-nil]
            [typed.cljc.checker.filter-rep :as fl]
            [typed.cljc.checker.filter-ops :as fo]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.runtime.env :as env]
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.set :as set])
  (:import (clojure.lang MultiFn)))

;(t/ann expr-ns [Any t/Any -> t/Sym])
(defn expr-ns [expr opts]
  {:post [(symbol? %)]}
  (impl/impl-case opts
    :clojure (let [nsym (get-in expr [:env :ns])
                   _ (assert (symbol? nsym) (str "Bug! " (pr-str (:op expr)) " expr has no associated namespace: "
                                                 (pr-str nsym)))]
               (ns-name nsym))
    :cljs (or (-> expr :env :ns :name)
              (do (prn "WARNING: No associated ns for ClojureScript expr, defaulting to cljs.user")
                  'cljs.user))))

(defn KeyPE->Type [k]
  {:pre [(pe/KeyPE? k)]
   :post [(r/Type? %)]}
  (r/-val (:val k)))

(defn fn-self-name [{:keys [op] :as fexpr}]
  {:pre [(= :fn op)]
   :post [((some-fn nil? symbol?) %)]}
  (-> fexpr :local :name))

;[MethodExpr Any -> (U nil NamespacedSymbol)]
(defn MethodExpr->qualsym [{c :class :keys [op method] :as expr} opts]
  {:pre [(#{:static-call :instance-call :static-method :instance-method} op)]
   :post [((some-fn nil? symbol?) %)]}
  (impl/assert-clojure opts)
  (when c
    (assert (class? c))
    (assert (symbol? method))
    (symbol (str (coerce/Class->symbol c))
            (str (when (= :instance-method op)
                   ;;TODO perhaps use 1.12 syntax for overriding method types?
                   (throw (ex-info "TODO MethodExpr->qualsym :instance-call :instance-method" {}))
                   ".")
                 method))))

;[FieldExpr -> (U nil NamespacedSymbol)]
(defn FieldExpr->qualsym [{c :class :keys [op field] :as expr} opts]
  {:pre [(#{:static-field :instance-field} op)]
   :post [((some-fn nil? symbol?) %)]}
  (impl/assert-clojure opts)
  (when c
    (assert (class? c))
    (assert (symbol? field))
    (symbol (str (coerce/Class->symbol c)) (str field))))


;(t/ann expected-error [r/Type r/TCResult -> nil])
(defn expected-error
  ([actual expected opts] (expected-error actual expected {} opts))
  ([actual expected opt opts]
   {:pre [(r/Type? actual)
          (r/TCResult? expected)]}
   (let [opts (update opts ::prs/unparse-type-in-ns #(or % (some-> (::vs/current-expr opts) (expr-ns opts))))]
     (err/tc-delayed-error (str "Type mismatch:"
                                "\n\nExpected: \t" (pr-str (prs/unparse-type (:t expected) opts))
                                "\n\nActual: \t" (pr-str (prs/unparse-type actual opts)))
                           (into {:expected expected
                                  :actual actual}
                                 opt)
                           opts))))


;(t/ann error-ret [(U nil TCResult) -> TCResult])
(defn error-ret 
  "Return a TCResult appropriate for when a type
  error occurs, with expected type expected.
  
  Use *only* in case of a type error."
  [expected]
  {:pre [((some-fn nil? r/TCResult?) expected)]
   :post [(r/TCResult? %)]}
  (or expected
      (r/ret (r/TCError-maker))))

;[Type -> '[Type (Seqable t/Sym) (Seqable F) (Seqable (U Bounds Regex)) (Option (U :Poly :PolyDots))]
; Opts
; -> Type]
(defn unwrap-poly
  "Return a pair vector of the instantiated body of the possibly polymorphic
  type and the names used"
  [t opts]
  {:pre [(r/Type? t)]
   :post [((con/hvector-c? r/Type? 
                           (some-fn nil? (con/every-c? r/F?))
                           (some-fn nil? (con/every-c? (some-fn r/Bounds? r/Regex?)))
                           (some-fn nil? #{:Poly :PolyDots})) %)]}
  (cond
    (r/Poly? t) (let [new-nmes (c/Poly-fresh-symbols* t)
                      new-frees (mapv r/make-F new-nmes)
                      bnds (c/Poly-bbnds* new-nmes t opts)
                      opts (free-ops/with-bounded-frees opts new-nmes bnds)]
                  [(c/Poly-body* new-nmes t opts) new-frees bnds :Poly])
    (r/PolyDots? t) (let [new-nmes (c/PolyDots-fresh-symbols* t)
                          new-frees (mapv r/make-F new-nmes)
                          bnds (c/PolyDots-bbnds* new-nmes t opts)
                          opts (free-ops/with-bounded-frees opts new-nmes bnds)]
                      [(c/PolyDots-body* new-nmes t opts) new-frees bnds :PolyDots])
    :else [t nil nil nil]))

(def not-special :default)

;(t/ann hvec->rets [HSequential -> (Seqable TCResult)])
(defn hvec->rets [v]
  {:pre [(r/HeterogeneousVector? v)]
   :post [(every? r/TCResult? %)]}
  (map r/ret
       (:types v)
       (:fs v)
       (:objects v)))

(defn- get-demunged-protocol-method [unwrapped-p mungedsym opts]
  {:pre [(symbol? mungedsym)
         (r/Protocol? unwrapped-p)]
   :post [(r/Type? %)]}
  (let [munged-methods (zipmap 
                         (->> (keys (:methods unwrapped-p))
                              (map munge))
                         (vals (:methods unwrapped-p)))
        mth (get munged-methods mungedsym)
        _ (when-not mth
            (err/int-error (str "No matching annotation for protocol method implementation: "
                                mungedsym)
                           opts))]
    mth))

; don't check these implicit methods in a record
(def record-implicits
  '#{entrySet values keySet clear putAll remove put get containsValue isEmpty size without
     assoc iterator seq entryAt containsKey equiv cons empty count getLookupThunk valAt
     withMeta meta equals hashCode hasheq})

(def record-hidden-fields
  '#{__meta __extmap __hash __hasheq})

(declare symbol->PArray)

;[t/Sym Boolean -> Type]
(defn Java-symbol->Type [sym nilable? opts]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [(r/Type? %)]}
  (or ((prs/clj-primitives-fn opts) sym)
      (symbol->PArray sym nilable? opts)
      (when-let [cls (resolve sym)]
        (c/Un (cons (c/RClass-of-with-unknown-params cls opts)
                    (when nilable?
                      [r/-nil]))
              opts))
      (err/tc-delayed-error (str "Method or field symbol " sym " does not resolve to a type") opts)))

;[t/Sym Boolean -> (Option Type)]
(defn- symbol->PArray [sym nilable? opts]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [((some-fn nil? r/PrimitiveArray?) %)]}
  (let [s (str sym)]
    (when (.endsWith s "<>")
      (let [^String s-nosuffix (apply str (drop-last 2 s))]
        (assert (not (.contains s-nosuffix "<>")))
        ;Nullable elements
        (let [t (Java-symbol->Type (symbol s-nosuffix) nilable? opts)
              c (let [c (or (when-let [rclass ((prs/clj-primitives-fn opts) (symbol s-nosuffix))]
                              (r/RClass->Class rclass))
                            (resolve (symbol s-nosuffix)))
                      _ (assert (class? c) s-nosuffix)]
                  c)]
          (r/PrimitiveArray-maker c t t))))))

;[clojure.reflect.Field - Type]
(defn Field->Type [{:keys [type flags] :as field} opts]
  {:pre [(instance? clojure.reflect.Field field)
         flags]
   :post [(r/Type? %)]}
  (cond
    (:enum flags) (Java-symbol->Type type false opts)
    :else (Java-symbol->Type type true opts)))

(def method-map?
  (con/hmap-c? :declaring-class symbol?
               :return-type symbol?
               :name symbol?
               :parameter-types (con/every-c? symbol?)
               :flags (con/set-c? keyword?)))

;[MethodMap -> Type]
(defn instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method} opts]
  {:pre [(method-map? method)]
   :post [(r/FnIntersection? %)]}
  (assert (class? (resolve declaring-class)))
  (r/make-FnIntersection (r/make-Function (into [(c/RClass-of-with-unknown-params declaring-class opts)]
                                                (map #(Java-symbol->Type % false opts))
                                                parameter-types)
                                          (Java-symbol->Type return-type true opts))))


;[Type TCResult -> Type]
(defn extend-method-expected 
  "Returns the expected type with target-type intersected with the first argument"
  [target-type expected opts]
  {:pre [(r/Type? target-type)
         (r/Type? expected)]
   :post [(r/Type? %)]}
  (cond
    (r/FnIntersection? expected)
    (-> expected
        (update :types
                #(mapv
                   (fn [ftype]
                     (assert (<= 1 (count (:dom ftype))))
                     (-> ftype
                         (update :dom (fn [dom]
                                        (update (vec dom) 0 (fn [t] (c/In [t target-type] opts)))))))
                   %)))

    (r/Poly? expected)
    (let [names (c/Poly-fresh-symbols* expected)
          body (c/Poly-body* names expected opts)
          body (extend-method-expected target-type body opts)]
      (c/Poly* names 
               (c/Poly-bbnds* names expected opts)
               body
               {:named (:named expected)}
               opts))

    (r/PolyDots? expected)
    (let [names (c/PolyDots-fresh-symbols* expected)
          body (c/PolyDots-body* names expected opts)
          body (extend-method-expected target-type body opts)]
      (c/PolyDots* names 
                   (c/PolyDots-bbnds* names expected opts)
                   body
                   {:named (:named expected)}
                   opts))
    :else (err/int-error (str "Expected Function type, found " (prs/unparse-type expected opts))
                         opts)))

(defn protocol-implementation-type [root-t {:keys [declaring-class] :as method-sig} opts]
  (when-some [pvar (c/Protocol-interface->on-var declaring-class opts)]
    (when-not (pcl-env/get-protocol (env/checker opts) pvar opts)
      (err/int-error (str "Protocol " pvar " must be annotated via ann-protocol before its method implementations "
                          "can be checked.")
                     opts))
    (let [mungedsym (symbol (:name method-sig))
          gather-impl-type (fn gather-impl-type [t]
                             (let [t (c/fully-resolve-type t opts)]
                               (cond
                                 (r/Protocol? t) (when (= pvar (:the-var t))
                                                   (extend-method-expected root-t (get-demunged-protocol-method t mungedsym opts) opts))
                                 (r/DataType? t) (some-> (seq (keep gather-impl-type (c/Datatype-ancestors t opts)))
                                                         (c/In opts))
                                 (r/Intersection? t) (some-> (seq (keep gather-impl-type (:types t)))
                                                             (c/In opts))
                                 (r/Union? t) (some-> (seq (keep gather-impl-type (:types t)))
                                                      (c/Un opts)))))
          found (gather-impl-type root-t)]
      (when-not found
        (err/int-error
          (str "Datatype " (:the-class root-t) " must annotate how it extends protocol " pvar
               " via ann-datatype. e.g., "
               (format "(ann-datatype %s :extends [(%s ...)])"
                       (apply str (->> (:the-class root-t) str (partition-by #{\.}) last))
                       (-> pvar name symbol)))
          opts))
      found)))

(defn type->method-expected [t method-sig opts]
  {:pre [(r/Type? t)]
   :post [(r/Type? %)]}
  (or (protocol-implementation-type t method-sig opts)
      (extend-method-expected t (instance-method->Function method-sig opts) opts)))

;; TODO integrate reflecte-validated into run-passes
(defn FieldExpr->Field [expr]
  {:post [(or (instance? clojure.reflect.Field %)
              (nil? %))]}
  (-> expr
      ana/reflect-validated
      :reflected-field)) 

(defn MethodExpr->Method [expr opts]
  {:post [(or (nil? %) (instance? clojure.reflect.Method %))]}
  (impl/assert-clojure opts)
  (-> expr
      ana/reflect-validated
      :reflected-method))

(defn NewExpr->Ctor [expr]
  {:post [(or (instance? clojure.reflect.Constructor %)
              (nil? %))]}
  (-> expr
      ana/reflect-validated
      :reflected-ctor))

(defn NewExpr->qualsym [expr]
  (-> expr
      ast-u/new-op-class
      coerce/Class->symbol))

;FIXME I think this hurts more than it helps
;[Type (Seqable t/Sym) -> Type]
;[Type -> Type]
(defn unwrap-datatype
  "Takes a DataType that might be wrapped in a TypeFn and returns the 
  DataType after instantiating it"
  ([dt nms opts]
   {:pre [((some-fn r/DataType? r/TypeFn?) dt)
          (every? symbol? nms)]
    :post [(r/DataType? %)]}
   (if (r/TypeFn? dt)
     (c/TypeFn-body* nms (c/TypeFn-bbnds* nms dt opts) dt opts)
     dt))
  ([dt opts] (let [nms (when (r/TypeFn? dt)
                         (c/TypeFn-fresh-symbols* dt))]
               (unwrap-datatype dt nms opts))))

;[t/Sym -> Type]
(defn DataType-ctor-type [sym opts]
  (letfn [(resolve-ctor [dtp opts]
            (cond
              ((some-fn r/DataType? r/Record?) dtp) 
              (let [dt dtp
                    fields (c/DataType-fields* dt)
                    make-arity (fn [extra-args]
                                 (r/make-Function (into (vec (vals fields)) extra-args)
                                                  dt))]
                (apply r/make-FnIntersection 
                       (cond-> [(make-arity nil)]
                         (r/Record? dtp) (conj (make-arity (vals (c/extra-Record-fields dt opts)))))))

              (r/TypeFn? dtp) (let [nms (c/TypeFn-fresh-symbols* dtp)
                                    bbnds (c/TypeFn-bbnds* nms dtp opts)
                                    body (c/TypeFn-body* nms bbnds dtp opts)]
                                (c/Poly* nms
                                         bbnds
                                         (resolve-ctor body (free-ops/with-bounded-frees opts nms bbnds))
                                         opts))

              :else (err/tc-delayed-error (str "Cannot generate constructor type for: " sym)
                                          {:return r/Err}
                                          opts)))]
    (resolve-ctor (dt-env/get-datatype (env/checker opts) sym opts) opts)))

;[Method -> t/Sym]
(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

#_
(defn method-nilable-param? [msym nparams n opts]
  {:post [(boolean? %)]}
  (mtd-param-nil/nilable-param? msym nparams n opts))

#_
(defn method-nonnilable-return? [msym nparams]
  {:post [(boolean? %)]}
  (mtd-ret-nil/nonnilable-return? msym nparams opts))

;[clojure.reflect.Method -> Type]
(defn Method->Type [{{:keys [varargs]} :flags :keys [parameter-types return-type] :as method} opts]
  {:pre [(instance? clojure.reflect.Method method)
         (vector? parameter-types)]
   :post [(r/FnIntersection? %)]}
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (r/make-FnIntersection (r/make-Function (into [] (map-indexed (fn [n tsym]
                                                                    (Java-symbol->Type
                                                                      tsym
                                                                      (mtd-param-nil/nilable-param? msym nparams n opts)
                                                                      opts)))
                                                  (cond-> parameter-types
                                                    varargs pop))
                                            (Java-symbol->Type
                                              return-type
                                              (not (mtd-ret-nil/nonnilable-return? msym nparams opts))
                                              opts)
                                            :rest
                                            (when varargs
                                              (Java-symbol->Type
                                                (peek parameter-types)
                                                (mtd-param-nil/nilable-param? msym nparams (dec nparams) opts)
                                                opts))))))

;[clojure.reflect.Constructor -> Type]
(defn Constructor->Function [{:keys [declaring-class parameter-types] :as ctor} opts]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(r/FnIntersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (err/tc-delayed-error (str "Constructor for unresolvable class " (:class ctor)) opts))]
    (r/make-FnIntersection (r/make-Function (mapv #(Java-symbol->Type % false opts) parameter-types)
                                            (c/RClass-of-with-unknown-params cls opts)
                                            ;always a true value. Cannot construct nil
                                            ; or primitive false
                                            :filter (fo/-true-filter)))))

;[(Seqable Expr) (Option Expr) FnIntersection -> (Seqable Function)]
(defn relevant-Fns
  "Given a set of required-param exprs, rest-param expr, and a FnIntersection,
  returns a seq of Functions containing Function types
  whos arities could be a subtype to the method with the fixed and rest parameters given"
  [required-params rest-param fin]
  {:pre [(r/FnIntersection? fin)]
   :post [(every? r/Function? %)]}
  (let [nreq (count required-params)]
    ;(prn "nreq" nreq)
    ;(prn "rest-param" rest-param)
    (filter (fn [{:keys [dom rest drest kws prest pdot kind]}]
              {:pre [(#{:fixed :rest :drest :kws :prest :pdot} kind)]}
              (let [ndom (count dom)]
                (if rest-param 
                  (or ; required parameters can flow into the rest type
                      (when (or rest drest prest pdot)
                        (<= nreq ndom))
                      ; kw functions must have exact fixed domain match
                      (when kws
                        (= nreq ndom)))
                  (and (not rest) (= nreq ndom)))))
            (:types fin))))

(defn default-defmethod? [var dispatch-val]
  {:pre [(var? var)]}
  (let [^MultiFn multifn @var
        _ (assert (instance? clojure.lang.MultiFn multifn))
        default-val (.defaultDispatchVal multifn)]
    (= default-val dispatch-val)))

(t/ann checked-ns! [t/Sym t/Any -> nil])
(defn- checked-ns! [nsym {::vs/keys [already-checked] :as opts}]
  (assert already-checked (-> opts keys sort vec))
  (swap! already-checked conj nsym)
  nil)

(t/ann already-checked? [t/Sym t/Any -> t/Bool])
(defn already-checked? [nsym {::vs/keys [already-checked] :as opts}]
  (assert already-checked (-> opts keys sort vec))
  (boolean (@already-checked nsym)))

(t/ann check-ns-and-deps [t/Sym [t/Sym -> t/Any] t/Any -> nil])
(defn check-ns-and-deps
  "Type check a namespace and its dependencies."
  [nsym check-ns1 {::vs/keys [check-config] :as opts}]
  {:pre [(symbol? nsym)]
   :post [(nil? %)]}
  (cond
    (already-checked? nsym opts) (do
                                   ;(println (str "Already checked " nsym ", skipping"))
                                   ;(flush)
                                   nil)
    :else
    (let [; check deps
          _ (when (= :recheck (:check-ns-dep check-config))
              (checked-ns! nsym opts)
              ;check normal dependencies
              (doseq [dep (ns-depsu/deps-for-ns nsym opts)]
                ;; ensure namespace actually exists
                (when (ns-depsu/should-check-ns? nsym opts)
                  (check-ns-and-deps dep check-ns1 opts))))
          ; ignore ns declaration
          ns-form (ns-depsu/ns-form-for-ns nsym opts)
          check? (some-> ns-form (ns-depsu/should-check-ns-form? opts))]
      (when check?
        (let [start (. System (nanoTime))]
          (println "Start checking" nsym)
          (check-ns1 nsym nil opts)
          (println "Checked" nsym "in" (/ (double (- (. System (nanoTime)) start)) 1000000.0) "msecs"))))))

(defn find-updated-locals [env1 env2 opts]
  {:pre [(map? env1)
         (map? env2)]}
  (into #{}
        (keep
          (fn [[k v1]]
            (when-some [v2 (get env2 k)]
              (when (and (sub/subtype? v1 v2 opts)
                         (not (sub/subtype? v2 v1 opts)))
                k))))
        env1))

(defn Type->Class [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? class?) %)]}
  (cond
    (r/RClass? t) (r/RClass->Class t)
    (r/Value? t) (class (:val t))))

(defn should-rewrite? [{::vs/keys [in-check-form can-rewrite] :as opts}]
  (and in-check-form can-rewrite))

(defn add-cast
  "Given an AST node and a type, return a new AST that
  casts the original expression to the given type"
  [{:keys [env] :as expr} t {:keys [positive negative line column] :as opt} opts]
  {:pre [(map? expr)
         (r/Type? t)]
   :post [(map? %)]}
  (impl/assert-clojure opts)
  (let [placeholder nil
        pred-form `(t/cast ~(prs/unparse-type t opts) ~placeholder 
                           {:positive ~(or (str positive)
                                           "cast")
                            :negative ~(or (str negative)
                                           "cast")
                            :line ~(or line (:line env))
                            :column ~(or column (:column env))})
        pred-expr (ana/analyze1 ;; FIXME support CLJS
                    pred-form
                    env
                    {:eval-fn (fn [ast _] ast)}
                    opts)]
    (assert (= :do (:op pred-expr))
            (pr-str (:op pred-expr)))
    (assert (= :invoke (-> pred-expr :ret :op))
            (-> pred-expr :ret :op))
    (assert (== 1 (-> pred-expr :ret :args count))
            (-> pred-expr :ret :args count))
    (assert (= :const (-> pred-expr :ret :args first :op))
            (-> pred-expr :ret :args first :op))
    (assoc-in pred-expr [:ret :args 0] expr)))

(defn TCResult->map [ret opts]
  {:pre [(r/TCResult? ret)]
   :post [(map? %)]}
  (let [opts (assoc opts ::vs/verbose-types true)]
    {:type (prs/unparse-type (:t ret) opts)
     :filters (prs/unparse-filter-set (:fl ret) opts)
     :object (prs/unparse-object (:o ret) opts)
     :opts (not-empty (:opts ret))}))
                                   
(defn map->TCResult [expected opts]
  {:pre [(not (r/TCResult? expected))
         (map? expected)
         (empty?
           (set/difference (set (keys expected))
                           #{:type :filters :object :opts}))
         (empty?
           (set/difference (set (keys (:filters expected)))
                           #{:then :else}))
         ;; Should at least contain a :type entry (I think?)
         (contains? expected :type)]
   :post [(r/TCResult? %)]}
  (->
    (r/ret (if-let [[_ t] (find expected :type)]
             (prs/parse-type t opts)
             (throw (Exception. "Must provide type")))
           (if-let [[_ fl] (find expected :filters)]
             (fo/-FS
               (if-let [[_ f] (find fl :then)]
                 (prs/parse-filter f)
                 fl/-top)
               (if-let [[_ f] (find fl :else)]
                 (prs/parse-filter f)
                 fl/-top))
             (fo/-simple-filter))
           (if-let [[_ o] (find expected :object)]
             (prs/parse-object o)
             obj/-empty))
    (assoc :opts (or (:opts expected) {}))))

(defn maybe-map->TCResult [m opts]
  (some-> m map->TCResult opts))

(defn special-typed-expression [& args]
  )
