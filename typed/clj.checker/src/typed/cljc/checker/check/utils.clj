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
            [clojure.core.typed.ast-utils :as ast-u]
            [clojure.set :as set])
  (:import (clojure.lang MultiFn)))

;(t/ann expr-ns [Any -> t/Sym])
(defn expr-ns [expr]
  {:post [(symbol? %)]}
  (impl/impl-case
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

;[MethodExpr -> (U nil NamespacedSymbol)]
(defn MethodExpr->qualsym [{c :class :keys [op method] :as expr}]
  {:pre [(#{:static-call :instance-call} op)]
   :post [((some-fn nil? symbol?) %)]}
  (when c
    (assert (class? c))
    (assert (symbol? method))
    (symbol (str (coerce/Class->symbol c)) (str method))))

;[FieldExpr -> (U nil NamespacedSymbol)]
(defn FieldExpr->qualsym [{c :class :keys [op field] :as expr}]
  {:pre [(#{:static-field :instance-field} op)]
   :post [((some-fn nil? symbol?) %)]}
  (when c
    (assert (class? c))
    (assert (symbol? field))
    (symbol (str (coerce/Class->symbol c)) (str field))))


;(t/ann expected-error [r/Type r/TCResult -> nil])
(defn expected-error [actual expected & opt]
  {:pre [(r/Type? actual)
         (r/TCResult? expected)]}
  (prs/with-unparse-ns (or prs/*unparse-type-in-ns*
                           (some-> vs/*current-expr* expr-ns))
    (apply err/tc-delayed-error (str "Type mismatch:"
                                     "\n\nExpected: \t" (pr-str (prs/unparse-type (:t expected)))
                                     "\n\nActual: \t" (pr-str (prs/unparse-type actual)))
           :expected expected
           :actual actual
           opt)))


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
; -> Type]
(defn unwrap-poly
  "Return a pair vector of the instantiated body of the possibly polymorphic
  type and the names used"
  [t]
  {:pre [(r/Type? t)]
   :post [((con/hvector-c? r/Type? 
                           (some-fn nil? (con/every-c? r/F?))
                           (some-fn nil? (con/every-c? (some-fn r/Bounds? r/Regex?)))
                           (some-fn nil? #{:Poly :PolyDots})) %)]}
  (cond
    (r/Poly? t) (let [new-nmes (c/Poly-fresh-symbols* t)
                      new-frees (map r/make-F new-nmes)]
                  [(c/Poly-body* new-nmes t) new-frees (c/Poly-bbnds* new-nmes t) :Poly])
    (r/PolyDots? t) (let [new-nmes (c/PolyDots-fresh-symbols* t)
                          new-frees (map r/make-F new-nmes)]
                      [(c/PolyDots-body* new-nmes t) new-frees (c/PolyDots-bbnds* new-nmes t) :PolyDots])
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

(defn- get-demunged-protocol-method [unwrapped-p mungedsym]
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
                              mungedsym)))]
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
(defn Java-symbol->Type [sym nilable?]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [(r/Type? %)]}
  (or ((prs/clj-primitives-fn) sym)
      (symbol->PArray sym nilable?)
      (when-let [cls (resolve sym)]
        (apply c/Un (c/RClass-of-with-unknown-params cls)
               (when nilable?
                 [r/-nil])))
      (err/tc-delayed-error (str "Method or field symbol " sym " does not resolve to a type"))))

;[t/Sym Boolean -> (Option Type)]
(defn- symbol->PArray [sym nilable?]
  {:pre [(symbol? sym)
         (boolean? nilable?)]
   :post [((some-fn nil? r/PrimitiveArray?) %)]}
  (let [s (str sym)]
    (when (.endsWith s "<>")
      (let [^String s-nosuffix (apply str (drop-last 2 s))]
        (assert (not (.contains s-nosuffix "<>")))
        ;Nullable elements
        (let [t (Java-symbol->Type (symbol s-nosuffix) nilable?)
              c (let [c (or (when-let [rclass ((prs/clj-primitives-fn) (symbol s-nosuffix))]
                              (r/RClass->Class rclass))
                            (resolve (symbol s-nosuffix)))
                      _ (assert (class? c) s-nosuffix)]
                  c)]
          (r/PrimitiveArray-maker c t t))))))

;[clojure.reflect.Field - Type]
(defn Field->Type [{:keys [type flags] :as field}]
  {:pre [(instance? clojure.reflect.Field field)
         flags]
   :post [(r/Type? %)]}
  (cond
    (:enum flags) (Java-symbol->Type type false)
    :else (Java-symbol->Type type true)))

(def method-map?
  (con/hmap-c? :declaring-class symbol?
               :return-type symbol?
               :name symbol?
               :parameter-types (con/every-c? symbol?)
               :flags (con/set-c? keyword?)))

;[MethodMap -> Type]
(defn instance-method->Function [{:keys [parameter-types declaring-class return-type] :as method}]
  {:pre [(method-map? method)]
   :post [(r/FnIntersection? %)]}
  (assert (class? (resolve declaring-class)))
  (r/make-FnIntersection (r/make-Function (cons (c/RClass-of-with-unknown-params declaring-class)
                                                (map #(Java-symbol->Type % false) parameter-types))
                                          (Java-symbol->Type return-type true))))


;[Type TCResult -> Type]
(defn extend-method-expected 
  "Returns the expected type with target-type intersected with the first argument"
  [target-type expected]
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
                                        (update (vec dom) 0 (partial c/In target-type))))))
                   %)))

    (r/Poly? expected)
    (let [names (c/Poly-fresh-symbols* expected)
          body (c/Poly-body* names expected)
          body (extend-method-expected target-type body)]
      (c/Poly* names 
               (c/Poly-bbnds* names expected)
               body
               :named (:named expected)))

    (r/PolyDots? expected)
    (let [names (c/PolyDots-fresh-symbols* expected)
          body (c/PolyDots-body* names expected)
          body (extend-method-expected target-type body)]
      (c/PolyDots* names 
                   (c/PolyDots-bbnds* names expected)
                   body
                   :named (:named expected)))
    :else (err/int-error (str "Expected Function type, found " (prs/unparse-type expected)))))

(defn protocol-implementation-type [root-t {:keys [declaring-class] :as method-sig}]
  (when-some [pvar (c/Protocol-interface->on-var declaring-class)]
    (when-not (pcl-env/get-protocol pvar)
      (err/int-error (str "Protocol " pvar " must be annotated via ann-protocol before its method implementations "
                          "can be checked.")))
    (let [mungedsym (symbol (:name method-sig))
          gather-impl-type (fn gather-impl-type [t]
                             (let [t (c/fully-resolve-type t)]
                               (cond
                                 (r/Protocol? t) (when (= pvar (:the-var t))
                                                   (extend-method-expected root-t (get-demunged-protocol-method t mungedsym)))
                                 (r/DataType? t) (some->> (seq (keep gather-impl-type (c/Datatype-ancestors t)))
                                                          (apply c/In))
                                 (r/Intersection? t) (some->> (seq (keep gather-impl-type (:types t)))
                                                              (apply c/In))
                                 (r/Union? t) (some->> (seq (keep gather-impl-type (:types t)))
                                                       (apply c/Un)))
                               ))
          found (gather-impl-type root-t)]
      (when-not found
        (err/int-error
          (str "Datatype " (:the-class root-t) " must annotate how it extends protocol " pvar
               " via ann-datatype. e.g., "
               (format "(ann-datatype %s :extends [(%s ...)])"
                       (apply str (->> (:the-class root-t) str (partition-by #{\.}) last))
                       (-> pvar name symbol)))))
      found)))

(defn type->method-expected [t method-sig]
  {:pre [(r/Type? t)]
   :post [(r/Type? %)]}
  (or (protocol-implementation-type t method-sig)
      (extend-method-expected t (instance-method->Function method-sig))))

;; TODO integrate reflecte-validated into run-passes
(defn FieldExpr->Field [expr]
  {:post [(or (instance? clojure.reflect.Field %)
              (nil? %))]}
  (-> expr
      ana/reflect-validated
      :reflected-field)) 

(defn MethodExpr->Method [expr]
  {:post [(or (nil? %) (instance? clojure.reflect.Method %))]}
  (-> expr
      ana/reflect-validated
      :reflected-method))

(defn NewExpr->Ctor [expr]
  {:post [(or (instance? clojure.reflect.Constructor %)
              (nil? %))]}
  (-> expr
      ana/reflect-validated
      :reflected-ctor))

;FIXME I think this hurts more than it helps
;[Type (Seqable t/Sym) -> Type]
;[Type -> Type]
(defn unwrap-datatype
  "Takes a DataType that might be wrapped in a TypeFn and returns the 
  DataType after instantiating it"
  ([dt nms]
   {:pre [((some-fn r/DataType? r/TypeFn?) dt)
          (every? symbol? nms)]
    :post [(r/DataType? %)]}
   (if (r/TypeFn? dt)
     (c/TypeFn-body* nms dt)
     dt))
  ([dt] (let [nms (when (r/TypeFn? dt)
                    (c/TypeFn-fresh-symbols* dt))]
          (unwrap-datatype dt nms))))

;[t/Sym -> Type]
(defn DataType-ctor-type [sym]
  (letfn [(resolve-ctor [dtp]
            (cond
              ((some-fn r/DataType? r/Record?) dtp) 
              (let [dt dtp]
                (r/make-FnIntersection 
                  (r/make-Function (-> (c/DataType-fields* dt) vals) dt)))

              (r/TypeFn? dtp) (let [nms (c/TypeFn-fresh-symbols* dtp)
                                    bbnds (c/TypeFn-bbnds* nms dtp)
                                    body (c/TypeFn-body* nms dtp)]
                                (c/Poly* nms
                                         bbnds
                                         (free-ops/with-bounded-frees (zipmap (map r/make-F nms) bbnds)
                                           (resolve-ctor body))))

              :else (err/tc-delayed-error (str "Cannot generate constructor type for: " sym)
                                        :return r/Err)))]
    (resolve-ctor (dt-env/get-datatype sym))))

;[Method -> t/Sym]
(defn Method->symbol [{name-sym :name :keys [declaring-class] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [((every-pred namespace symbol?) %)]}
  (symbol (name declaring-class) (name name-sym)))

(defn method-nilable-param? [msym nparams n]
  {:post [(boolean? %)]}
  (mtd-param-nil/nilable-param? msym nparams n))

(defn method-nonnilable-return? [msym nparams]
  {:post [(boolean? %)]}
  (mtd-ret-nil/nonnilable-return? msym nparams))

;[clojure.reflect.Method -> Type]
(defn Method->Type [{:keys [parameter-types return-type flags] :as method}]
  {:pre [(instance? clojure.reflect.Method method)]
   :post [(r/FnIntersection? %)]}
  (let [msym (Method->symbol method)
        nparams (count parameter-types)]
    (r/make-FnIntersection (r/make-Function (mapv (fn [[n tsym]]
                                                    (Java-symbol->Type
                                                      tsym
                                                      (mtd-param-nil/nilable-param? msym nparams n)))
                                                  (map-indexed vector
                                                               (if (:varargs flags)
                                                                 (butlast parameter-types)
                                                                 parameter-types)))
                                            (Java-symbol->Type
                                              return-type
                                              (not (mtd-ret-nil/nonnilable-return? msym nparams)))
                                            :rest
                                            (when (:varargs flags)
                                              (Java-symbol->Type
                                                (last parameter-types)
                                                (mtd-param-nil/nilable-param? msym nparams (dec nparams))))))))

;[clojure.reflect.Constructor -> Type]
(defn Constructor->Function [{:keys [declaring-class parameter-types] :as ctor}]
  {:pre [(instance? clojure.reflect.Constructor ctor)]
   :post [(r/FnIntersection? %)]}
  (let [cls (resolve declaring-class)
        _ (when-not (class? cls)
            (err/tc-delayed-error (str "Constructor for unresolvable class " (:class ctor))))]
    (r/make-FnIntersection (r/make-Function (doall (map #(Java-symbol->Type % false) parameter-types))
                                            (c/RClass-of-with-unknown-params cls)
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

(t/ann checked-ns! [t/Sym -> nil])
(defn- checked-ns! [nsym]
  (swap! vs/*already-checked* conj nsym)
  nil)

(t/ann already-checked? [t/Sym -> t/Bool])
(defn already-checked? [nsym]
  (boolean (@vs/*already-checked* nsym)))

(t/ann check-ns-and-deps [t/Sym [t/Sym -> t/Any] -> nil])
(defn check-ns-and-deps
  "Type check a namespace and its dependencies."
  [nsym check-ns1]
  {:pre [(symbol? nsym)]
   :post [(nil? %)]}
  (cond
    (already-checked? nsym) (do
                              ;(println (str "Already checked " nsym ", skipping"))
                              ;(flush)
                              nil)
    :else
    (let [; check deps
          #_#_
          _ (when (= :recheck (some-> vs/*check-config* deref :check-ns-dep))
              (checked-ns! nsym)
              ;check normal dependencies
              (doseq [dep (ns-depsu/deps-for-ns nsym)]
                ;; ensure namespace actually exists
                (when (ns-depsu/should-check-ns? nsym)
                  (check-ns-and-deps dep check-ns1))))
          ; ignore ns declaration
          ns-form (ns-depsu/ns-form-for-ns nsym)
          check? (some-> ns-form ns-depsu/should-check-ns-form?)]
      (cond
        (not check?)
        (println (str "Not checking " nsym 
                      (cond
                        (not ns-form) " (ns form missing)"
                        (ns-depsu/ignore-ns? ns-form) " (tagged with :typed.clojure/ignore metadata)")))

        :else
        (let [start (. System (nanoTime))
              _ (println "Start checking" nsym)
              _ (flush)
              _ (check-ns1 nsym)
              _ (println "Checked" nsym "in" (/ (double (- (. System (nanoTime)) start)) 1000000.0) "msecs")
              _ (flush)]
          nil)))))

(defn find-updated-locals [env1 env2]
  {:pre [(map? env1)
         (map? env2)]}
  (into #{}
        (keep
          (fn [[k v1]]
            (when-some [v2 (get env2 k)]
              (when (and (sub/subtype? v1 v2)
                         (not (sub/subtype? v2 v1)))
                k))))
        env1))

(defn Type->Class [t]
  {:pre [(r/Type? t)]
   :post [((some-fn nil? class?) %)]}
  (cond
    (r/RClass? t) (r/RClass->Class t)
    (r/Value? t) (class (:val t))))

(defn should-rewrite? []
  (and vs/*in-check-form*
       vs/*can-rewrite*))

(defn add-cast
  "Given an AST node and a type, return a new AST that
  casts the original expression to the given type"
  [{:keys [env] :as expr} t {:keys [positive negative line column] :as opt}]
  {:pre [(map? expr)
         (r/Type? t)]
   :post [(map? %)]}
  (impl/assert-clojure)
  (let [placeholder nil
        pred-form `(t/cast ~(prs/unparse-type t) ~placeholder 
                           {:positive ~(or (str positive)
                                           "cast")
                            :negative ~(or (str negative)
                                           "cast")
                            :line ~(or line (:line env))
                            :column ~(or column (:column env))})
        pred-expr (ana/analyze1 ;; FIXME support CLJS
                    pred-form
                    env
                    {:eval-fn (fn [ast _] ast)})]
    (assert (= :do (:op pred-expr))
            (pr-str (:op pred-expr)))
    (assert (= :invoke (-> pred-expr :ret :op))
            (-> pred-expr :ret :op))
    (assert (== 1 (-> pred-expr :ret :args count))
            (-> pred-expr :ret :args count))
    (assert (= :const (-> pred-expr :ret :args first :op))
            (-> pred-expr :ret :args first :op))
    (assoc-in pred-expr [:ret :args 0] expr)))

(defn TCResult->map [ret]
  {:pre [(r/TCResult? ret)]
   :post [(map? %)]}
  (binding [vs/*verbose-types* true]
    {:type (prs/unparse-type (:t ret))
     :filters (prs/unparse-filter-set (:fl ret))
     :object (prs/unparse-object (:o ret))
     :opts (not-empty (:opts ret))}))
                                   
(defn map->TCResult [expected]
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
             (prs/parse-type t)
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

(defn maybe-map->TCResult [m]
  (some-> m map->TCResult))

(defn special-typed-expression [& args]
  )
