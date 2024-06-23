;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

; untyped, clojure.core.typed depends on this namespace
(ns ^:no-doc clojure.core.typed.current-impl
  #?(:clj (:refer-clojure :exclude [requiring-resolve delay])
     :cljs (:refer-clojure :exclude [-val]))
  (:require [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.util-vars :as vs]
            [clojure.set :as set]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])
            #?(:clj [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])
            [typed.cljc.runtime.env :as env]
            [typed.clj.runtime.env :as clj-env]
            [typed.cljs.runtime.env :as cljs-env]))

;; copied to typed.clj.runtime.env, clojure.core.typed.type-contract
(def clojure :clojure.core.typed.current-impl/clojure)
;; copied to typed.cljs.runtime.env
(def clojurescript ::clojurescript)

;; We don't have a typed.cljr.runtime.env to copy to -- is this a problem?  What does this even mean?
(def cljr ::cljr)

(def unknown ::unknown)

(derive clojure unknown)
(derive clojurescript unknown)
(derive cljr unknown)

;; :clojure = ::clojure
;; :cljs = ::clojurescript
;; :cljr = ::cljr
;; :unknown = ::unknown
#?(:cljs :ignore
:default
(defmacro impl-case [opts & {clj-case :clojure cljs-case :cljs unknown :unknown :as opt}]
  (let [bad (set/difference (set (keys opt)) #{:clojure :cljs :cljr :unknown})
        _ (assert (empty? bad) (str "Incorrect cases to impl-case: " (pr-str bad)))
        gopts (gensym 'opts)
        gimpl (gensym 'impl)]
    `(let [~gopts ~opts
           ~gimpl (current-impl ~gopts)]
       (case ~gimpl
         ~clojure ~clj-case
         ~clojurescript ~cljs-case
         ~(if (contains? opt :unknown)
            unknown
            `(assert nil (str "No case matched for impl-case: " ~gimpl
                              " " ^:typed.clojure/ignore (-> ~gopts keys vec)
                              )))))))
)		
		

;; copied to typed.clj{s}.runtime.env
(def current-impl-kw ::current-impl)

(defn current-impl [opts]
  {:post [(qualified-keyword? %)]}
  (get opts current-impl-kw unknown))

(def register-clj!
  (let [d (delay ((requiring-resolve 'clojure.core.typed.runtime.jvm.configs/register-clj-config-anns)))]
    (fn [] @d)))
(def register-cljs!
  (let [d (delay ((requiring-resolve 'clojure.core.typed.runtime.jvm.configs/register-cljs-config-anns)))]
    (fn [] @d)))

(def current-var-annotations-kw ::current-var-annotations)
(def current-nocheck-var?-kw ::current-nocheck-var?)
(def current-used-vars-kw ::current-used-vars)
(def current-checked-var-defs-kw ::current-checked-var-defs)
(def cljs-jsvar-annotations-kw ::cljs-jsvar-annotations)
(def untyped-var-annotations-kw ::untyped-var-annotations)
(def current-name-env-kw ::current-name-env)
(def method-return-nonnilable-env-kw ::method-return-nonnilable-env)
(def method-param-nilable-env-kw ::method-param-nilable-env)
(def method-override-env-kw ::method-override-env)
(def field-override-env-kw ::field-override-env)
(def constructor-override-env-kw ::constructor-override-env)
(def protocol-name-type ::protocol-name)
(def current-protocol-env-kw ::current-protocol-env)
(def current-datatype-env-kw ::current-datatype-env)
(def current-dt-ancestors-kw ::current-dt-ancestors)
(def current-deps-kw ::current-deps)
(def datatype-name-type ::datatype-name)
(def ns-opts-kw ::ns-options)
(def unanalyzed-special-kw ::unanalyzed-special)
(def current-rclass-env-kw ::current-rclass-env)

(defn add-tc-var-type [checker sym type]
  (env/swap-checker! checker assoc-in [current-var-annotations-kw sym] type)
  nil)

(defn add-nocheck-var [checker sym]
  (env/swap-checker! checker update current-nocheck-var?-kw (fnil conj #{}) sym)
  nil)

(defn remove-nocheck-var [checker sym]
  (env/swap-checker! checker update current-nocheck-var?-kw (fnil disj #{}) sym)
  nil)

(defn var-no-checks [checker]
  {:post [(set? %)]}
  (get (env/deref-checker checker) current-nocheck-var?-kw #{}))

(defn check-var? [checker sym]
  (not (contains? (var-no-checks checker) sym)))

(defn add-tc-type-name [checker sym ty]
  (env/swap-checker! checker
                     assoc-in
                     [current-name-env-kw sym]
                     ty
                     #_(if (r/Type? ty)
                       (vary-meta ty assoc :from-name sym)
                       ty))
  nil)

(def declared-name-type ::declared-name)

(defn declare-name* [checker sym]
  {:pre [(qualified-symbol? sym)]}
  (add-tc-type-name checker sym declared-name-type)
  nil)

(defn declare-protocol* [checker sym]
  {:pre [(qualified-symbol? sym)]}
  (add-tc-type-name checker sym protocol-name-type)
  nil)

(defn declare-datatype* [checker sym]
  (add-tc-type-name checker sym datatype-name-type)
  nil)

(defn add-untyped-var [checker nsym sym t]
  {:pre [(symbol? nsym)
         (symbol? sym)
         ; enforced in var-env/get-untyped-var,
         ; not worth loading/importing r/Type? for this
         ; assertion.
         #_(or (r/Type? t)
               (delay? t)
               (fn? t))]
   :post [(nil? %)]}
  (env/swap-checker! checker assoc-in [untyped-var-annotations-kw nsym sym] t)
  nil)

(defn add-nonnilable-method-return [checker sym m]
  {:pre [(qualified-symbol? sym)
         ((some-fn #{:all}
                   (con/set-c? nat-int?))
          m)]}
  (env/swap-checker! checker assoc-in [method-return-nonnilable-env-kw sym] m)
  nil)

(defn add-method-nilable-param [checker sym a]
  {:pre [(qualified-symbol? sym)
         ((con/hash-c? (some-fn #{:all} nat-int?)
                       (some-fn #{:all} (con/set-c? nat-int?)))
          a)]}
  (env/swap-checker! checker assoc-in [method-param-nilable-env-kw sym] a)
  nil)

(defn add-method-override [checker sym t]
  {:pre [(qualified-symbol? sym)
         ;; checked at `get-method-override`
         #_
         ((some-fn fn? delay? r/Poly? r/FnIntersection?)
          t)]}
  (env/swap-checker! checker assoc-in [method-override-env-kw sym] t)
  nil)

(defn add-field-override [checker sym t]
  {:pre [(qualified-symbol? sym)
         ;; checked by `get-field-override`
         #_
         ((some-fn fn? delay? r/Type?)
          t)]}
  (env/swap-checker! checker assoc-in [field-override-env-kw sym] t)
  nil)

(defn add-constructor-override [checker sym t]
  {:pre [(simple-symbol? sym)
         ;; checked at `get-constructor-override`
         #_((some-fn fn? delay? r/Type?) t)]}
  (env/swap-checker! checker assoc-in [constructor-override-env-kw sym] t)
  nil)

(defn add-protocol [checker sym t]
  {:pre [(qualified-symbol? sym)
         ;; checked in get-protocol
         #_
         ((some-fn fn? delay? r/Type?) t)]}
  (env/swap-checker! checker assoc-in [current-protocol-env-kw sym] t)
  nil)

(defn add-rclass [checker sym t]
  {:pre [(simple-symbol? sym)
         ;; checked in get-protocol
         #_
         ((some-fn fn? delay? r/RClass? r/Poly?) t)]}
  (env/swap-checker! checker assoc-in [current-rclass-env-kw sym] t)
  nil)

(defn add-datatype [checker sym t opts]
  {:pre [(impl-case opts
           :clojure ((every-pred simple-symbol?
                                 (fn [k] (some #{\.} (str k))))
                     sym)
           :cljs (qualified-symbol? sym))
         ;; checked in get-datatype
         #_
         ((some-fn fn? delay? r/Type?) t)]
   :post [(nil? %)]}
  (env/swap-checker! checker assoc-in [current-datatype-env-kw sym] t)
  nil)

(defn add-ns-deps [checker nsym deps]
  {:pre [(simple-symbol? nsym)
         ((con/set-c? symbol?) deps)]
   :post [(nil? %)]}
  (env/swap-checker! checker update-in [current-deps-kw nsym] (fnil set/union #{}) deps)
  nil)

(defn register-warn-on-unannotated-vars [checker nsym]
  (env/swap-checker! checker assoc-in [ns-opts-kw nsym :warn-on-unannotated-vars] true)
  nil)

#?(:cljs :ignore 
:default
(def ^:private force-type #((requiring-resolve 'typed.cljc.runtime.env-utils/force-type) %1 %2)))
#?(:cljs :ignore :default
(defmacro ^:private delay-type [& body]
  `((requiring-resolve 'typed.cljc.runtime.env-utils/delay-type*)
    (fn [] (do ~@body)))))

#?(:cljs :ignore
:default
(defmacro create-env
  "For name n, creates defs for {n}, {n}-kw, add-{n},
  and reset-{n}!"
  [n]
  {:pre [(simple-symbol? n)]}
  (let [kw-def (symbol (str n "-kw"))
        add-def (symbol (str "add-" n))
        reset-def (symbol (str "reset-" n "!"))]
    `(do (def ~kw-def ~(keyword (str (ns-name *ns*)) (str n)))
         (defn ~n [checker#]
           {:post [(map? ~'%)]}
           (get (env/deref-checker checker#) ~kw-def {}))
         (defn ~add-def [checker# sym# t#]
           {:pre [(symbol? sym#)]
            :post [(nil? ~'%)]}
           (env/swap-checker! checker# assoc-in [~kw-def sym#] t#)
           nil)
         (defn ~reset-def [checker# m#]
           (env/swap-checker! checker# assoc ~kw-def m#)
           nil)
         nil))))

;; runtime environments
#?(:cljs :ignore :default
(create-env var-env))
#?(:cljs :ignore :default
(create-env alias-env))
#?(:cljs :ignore :default
(create-env protocol-env))
#?(:cljs :ignore :default
(create-env rclass-env))
#?(:cljs :ignore :default
(create-env datatype-env))
#?(:cljs :ignore :default
(create-env jsnominal-env))

#?(:cljs :ignore :default
(defn v [vsym]
  {:pre [(qualified-symbol? vsym)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    @var)))

#?(:cljs :ignore :default
(defn the-var [vsym]
  {:pre [(qualified-symbol? vsym)]
   :post [(var? %)]}
  (let [ns (find-ns (symbol (namespace vsym)))
        _ (assert ns (str "Cannot find namespace: " (namespace vsym)))
        var (ns-resolve ns (symbol (name vsym)))]
    (assert (var? var) (str "Cannot find var: " vsym))
    var)))

(def clj-checker-atom clj-env/clj-checker-atom)

(defn clj-checker []
  clj-checker-atom)

(def cljs-checker-atom cljs-env/cljs-checker-atom)

(defn cljs-checker []
  {:post [#?(:clj (instance? clojure.lang.IAtom %)
             :cljs (instance? Atom %))]}
  cljs-checker-atom)

(defn implementation-specified? [opts]
  (not= unknown (current-impl opts)))

(defn ensure-impl-specified [opts]
  (assert (implementation-specified? opts) "No implementation specified"))

(defn checking-clojure? [opts]
  (ensure-impl-specified opts)
  (= clojure (current-impl opts)))

(defn checking-clojurescript? [opts]
  (ensure-impl-specified opts)
  (= clojurescript (current-impl opts)))

(defmacro assert-clojure
  ([opts] `(assert-clojure nil ~opts))
  ([msg opts] `(assert (= clojure (current-impl ~opts))
                       (let [msg# ~msg]
                         (str "Clojure implementation only"
                              (when (seq msg#)
                                (str ": " msg#)))))))

(defmacro assert-cljs
  ([opts] `(assert-cljs nil ~opts))
  ([msg opts] `(assert (= clojurescript (current-impl ~opts))
                       (let [msg# ~msg]
                         (str "Clojurescript implementation only"
                              (when (seq msg#)
                                (str ": " msg#)))))))


#?(:cljs :ignore :default
(defn var->symbol [^clojure.lang.Var var]
  {:pre [(var? var)]
   :post [(qualified-symbol? %)]}
  (symbol (str (ns-name (.ns var)))
          (str (.sym var)))))

#?(:cljs :ignore :default
(defn Class->symbol [^#?(:cljr Type :clj Class) cls]
  {:pre [(class? cls)]
   :post [(simple-symbol? %)]}
  (symbol (#?(:cljr .FullName :clj .getName) cls))))

; for type-contract
(defn hmap-c? [& {:keys [mandatory optional absent-keys complete?]}]
  (every-pred map?
              #(cond
                 complete? (set/subset? (set (keys %))
                                        (into #{} (mapcat keys) [mandatory optional]))
                 :else
                 (let [actual-ks (set (keys %))]
                   (and 
                     ;required keys is a subset of actual keys
                     (set/subset? 
                       (set (keys mandatory))
                       actual-ks)
                     ;no absent-keys are present
                     (empty?
                       (set/intersection
                         absent-keys
                         actual-ks)))))
              #(every? (fn [[k vc]]
                         (and (contains? % k)
                              (vc (get % k))))
                       mandatory)
              #(every? (fn [[k vc]]
                         (or (not (contains? % k))
                             (vc (get % k))))
                       optional)))

#?(:cljs :ignore :default
(def ^:private int-error #(apply (requiring-resolve 'clojure.core.typed.errors/int-error) %&)))
#?(:cljs :ignore :default
(def ^:private parse-free-binder-with-variance #((requiring-resolve 'typed.clj.checker.parse-unparse/parse-free-binder-with-variance) %1 %2)))
#?(:cljs :ignore :default
(def ^:private with-bounded-frees #((requiring-resolve 'typed.cljc.checker.free-ops/with-bounded-frees) %1 %2)))
#?(:cljs :ignore :default
(def ^:private unparse-type #((requiring-resolve 'typed.clj.checker.parse-unparse/unparse-type) %1 %2)))
#?(:cljs :ignore :default
(def ^:private parse-type #((requiring-resolve 'typed.clj.checker.parse-unparse/parse-type) %1 %2)))
#?(:cljs :ignore :default
(def ^:private fully-resolve-type #((requiring-resolve 'typed.cljc.checker.type-ctors/fully-resolve-type) %1 %2)))
#?(:cljs :ignore :default
(def ^:private Poly? #((requiring-resolve 'typed.cljc.checker.type-rep/Poly?) %)))
#?(:cljs :ignore :default
(def ^:private Poly-fresh-symbols* #((requiring-resolve 'typed.cljc.checker.type-ctors/Poly-fresh-symbols*) %)))
#?(:cljs :ignore :default
(def ^:private Poly-body* #(apply (requiring-resolve 'typed.cljc.checker.type-ctors/Poly-body*) %&)))
#?(:cljs :ignore :default
(def ^:private PolyDots? #((requiring-resolve 'typed.cljc.checker.type-rep/PolyDots?) %)))
#?(:cljs :ignore :default
(def ^:private PolyDots-fresh-symbols* #((requiring-resolve 'typed.cljc.checker.type-ctors/PolyDots-fresh-symbols*) %)))
#?(:cljs :ignore :default
(def ^:private PolyDots-body* #((requiring-resolve 'typed.cljc.checker.type-ctors/PolyDots-body*) %1 %2 %3)))
#?(:cljs :ignore :default
(def ^:private FnIntersection? #((requiring-resolve 'typed.cljc.checker.type-rep/FnIntersection?) %)))
#?(:cljs :ignore :default
(def ^:private Protocol* #(apply (requiring-resolve 'typed.cljc.checker.type-ctors/Protocol*) %&)))
#?(:cljs :ignore :default
(def ^:private Protocol-var->on-class #((requiring-resolve 'typed.cljc.checker.type-ctors/Protocol-var->on-class) %)))
#?(:cljs :ignore :default
(def ^:private -any #(deref (requiring-resolve 'typed.cljc.checker.type-rep/-any))))
#?(:cljs :ignore :default
(def ^:private protocol-method-var-ann #(apply (requiring-resolve 'typed.cljc.checker.collect-utils/protocol-method-var-ann) %&)))
#?(:cljs :ignore :default
(def ^:private make-F #((requiring-resolve 'typed.cljc.checker.type-rep/make-F) %)))
#?(:cljs :ignore :default
(def ^:private DataType* #(apply (requiring-resolve 'typed.cljc.checker.type-ctors/DataType*) %&)))
#?(:cljs :ignore :default
(def ^:private Poly* #(apply (requiring-resolve 'typed.cljc.checker.type-ctors/Poly*) %&)))
#?(:cljs :ignore :default
(def ^:private make-FnIntersection #((requiring-resolve 'typed.cljc.checker.type-rep/make-FnIntersection) %)))
#?(:cljs :ignore :default
(def ^:private make-Function #(apply (requiring-resolve 'typed.cljc.checker.type-rep/make-Function) %&)))
#?(:cljs :ignore :default
(def ^:private DataType-of #(apply (requiring-resolve 'typed.cljc.checker.type-ctors/DataType-of) %&)))
#?(:cljs :ignore :default
(def ^:private subtype? #((requiring-resolve 'typed.clj.checker.subtype/subtype?) %1 %2 %3)))

#?(:cljs :ignore :default
(defn gen-protocol* [current-env current-ns vsym binder mths checker opts]
  {:pre [(symbol? current-ns)
         ((some-fn nil? map?) mths)]}
  (let [opts (assoc opts :typed.clj.checker.parse-unparse/parse-type-in-ns current-ns)
        _ (when-not (symbol? vsym)
            (int-error
              (str "First argument to ann-protocol must be a symbol: " vsym)
              opts))
        s (if (namespace vsym)
            vsym
            (symbol (str current-ns) (name vsym)))
        protocol-defined-in-nstr (namespace s)
        _ (when-let [[m] (seq (remove symbol? (keys mths)))]
            (int-error (str "Method names to ann-protocol must be symbols, found: " (pr-str m)) opts))
        _ (doseq [n1 (keys mths)
                  n2 (keys mths)]
            (when (and (not= n1 n2)
                       (= (munge n1) (munge n2)))
              (int-error 
                (str "Protocol methods for " vsym " must have distinct representations: "
                     "both " n1 " and " n2 " compile to " (munge n1))
                opts)))
        ; add a Name so the methods can be parsed
        _ (declare-protocol* checker s)
        parsed-binder (when binder 
                        (delay-type
                          (parse-free-binder-with-variance binder opts)))
        fs (when parsed-binder
             (delay-type 
               (map (comp make-F :fname) (force-type parsed-binder opts))))
        bnds (when parsed-binder
               (delay-type (map :bnd (force-type parsed-binder opts))))
        ms (into {} (map (fn [[knq v*]]
                           (let [_ (when (namespace knq)
                                     (int-error "Protocol method should be unqualified" opts))
                                 mtype 
                                 (delay-type
                                   (let [mtype (let [opts (-> opts
                                                              (with-bounded-frees (zipmap (force-type fs opts) (force-type bnds opts)))
                                                              (assoc ::vs/current-env current-env))]
                                                 (parse-type v* opts))
                                         _ (let [rt (fully-resolve-type mtype opts)
                                                 fin? (fn [f]
                                                        (let [f (fully-resolve-type f opts)]
                                                          (boolean
                                                            (when (FnIntersection? f)
                                                              (every? seq (map :dom (:types f)))))))]
                                             (when-not
                                               (or
                                                 (fin? rt)
                                                 (when (Poly? rt)
                                                   (let [names (Poly-fresh-symbols* rt)]
                                                     (fin? (Poly-body* names rt opts))))
                                                 (when (PolyDots? rt)
                                                   (let [names (PolyDots-fresh-symbols* rt)]
                                                     (fin? (PolyDots-body* names rt opts)))))
                                               ;(prn "throwing method type")
                                               (int-error (str "Protocol method " knq " should be a possibly-polymorphic function intersection"
                                                               " taking at least one fixed argument: "
                                                               (unparse-type mtype opts))
                                                          opts)))]
                                     mtype))]
                             [knq mtype])))
                 mths)
        ;_ (prn "collect protocol methods" (into {} ms))
        t (delay-type
            (Protocol* (map :name (force-type fs opts)) (map :variance (force-type parsed-binder opts))
                       (force-type fs opts) s (Protocol-var->on-class s) 
                       (into {} (map (fn [[k v]] [k (force-type v opts)])) ms) 
                       (map :bnd (force-type parsed-binder opts))
                       opts))]
    ;(prn "Adding protocol" s t)
    (add-protocol checker s t)
    ; annotate protocol var as Any
    (add-nocheck-var checker s)
    (add-tc-var-type checker s (delay-type (-any)))
    (doseq [[kuq mt] ms]
      (assert (not (namespace kuq))
              "Protocol method names should be unqualified")
      ;qualify method names when adding methods as vars
      (let [kq (symbol protocol-defined-in-nstr (name kuq))
            mt-ann (delay-type 
                     (protocol-method-var-ann (force-type mt opts) (map :name (force-type fs opts)) (force-type bnds opts) opts))]
        (add-nocheck-var checker kq)
        (add-tc-var-type checker kq mt-ann)))
    #_
    (prn "end gen-protocol" s (when (= "cljs.core" (namespace s))
                                (force-type (get-in (env/deref-checker checker) [current-protocol-env-kw s])
                                            opts)))
    nil)))

(defn add-datatype-ancestors
  "Add a mapping of ancestor overrides (from the type syntax of the override
  to the actual parsed type) for the datatype named sym."
  [checker sym tmap]
  {:pre [(symbol? sym)
         (map? tmap)]
   :post [(nil? %)]}
  (env/swap-checker! checker update-in [current-dt-ancestors-kw sym] merge tmap)
  nil)

#?(:cljs :ignore :default
(def ^:private demunge #((requiring-resolve 'clojure.repl/demunge) %)))
#?(:cljs :ignore :default
(def ^:private abstract-many #((requiring-resolve 'typed.cljc.checker.type-ctors/abstract-many) %1 %2 %3)))
#?(:cljs :ignore :default
(def ^:private with-frees #((requiring-resolve 'typed.cljc.checker.free-ops/with-frees) %1 %2)))
#?(:cljs :ignore :default
(def ^:private -val #((requiring-resolve 'typed.cljc.checker.type-rep/-val) %1)))
#?(:cljs :ignore :default
(def ^:private -nil #(deref (requiring-resolve 'typed.cljc.checker.type-rep/-nil))))
#?(:cljs :ignore :default
(def ^:private fv #((requiring-resolve 'typed.cljc.checker.frees/fv) %1 %2)))
#?(:cljs :ignore :default
(def ^:private fi #((requiring-resolve 'typed.cljc.checker.frees/fi) %1 %2)))
#?(:cljs :ignore :default
(def ^:private make-HMap #(apply (requiring-resolve 'typed.cljc.checker.type-ctors/make-HMap) %&)))

#?(:cljs :ignore :default
(defn gen-datatype* [current-env current-ns provided-name fields vbnd opt record? checker opts]
  {:pre [(symbol? current-ns)]}
  (let [_ (assert (impl-case opts
                    :clojure (simple-symbol? provided-name)
                    :cljs (qualified-symbol? provided-name)))
        opts (assoc opts :typed.clj.checker.parse-unparse/parse-type-in-ns current-ns)
        {ancests :unchecked-ancestors} opt
        ancests (or ancests (:extends opt))
        parsed-binders (when vbnd
                         (let [bfn (fn []
                                     (parse-free-binder-with-variance vbnd opts))]
                           (delay-type (bfn))))
        ;variances
        vs (when parsed-binders
             (delay-type (seq (map :variance (force-type parsed-binders opts)))))
        args (when parsed-binders
               (delay-type (seq (map :fname (force-type parsed-binders opts)))))
        bnds (when parsed-binders
               (delay-type (seq (map :bnd (force-type parsed-binders opts)))))
        provided-name-str (str provided-name)
        ;_ (prn "provided-name-str" provided-name-str)
        munged-ns-str (impl-case opts
                        :clojure (if (some #(= \. %) provided-name-str)
                                   (apply str (butlast (apply concat (butlast (partition-by #(= \. %) provided-name-str)))))
                                   (str (munge current-ns)))
                        :cljs nil)
        ;_ (prn "munged-ns-str" munged-ns-str)
        demunged-ns-str (impl-case opts
                          :clojure (str (demunge munged-ns-str))
                          :cljs (-> provided-name namespace))
        ;_ (prn "demunged-ns-str" demunged-ns-str)
        local-name (impl-case opts
                     :clojure (if (some #(= \. %) provided-name-str)
                                (symbol (apply str (last (partition-by #(= \. %) (str provided-name-str)))))
                                provided-name-str)
                     :cljs (-> provided-name name symbol))
        ;_ (prn "local-name" local-name)
        s (impl-case opts
            :clojure (symbol (str (name munged-ns-str) \. local-name))
            :cljs provided-name)
        fs (delay-type
             (let [parse-field (fn [[n colon t] opts]
                                 (when (not= :- colon)
                                   (int-error (format "Missing :- after field %s in ann-record."
                                                      n)
                                              opts))
                                 [n (parse-type t opts)])]
               (apply array-map (apply concat (let [opts (-> opts
                                                             (assoc ::vs/current-env current-env)
                                                             (with-frees (mapv make-F (force-type args opts))))]
                                                (mapv #(parse-field % opts) (partition 3 fields)))))))
        as (into {}
                 (map
                   (fn [an]
                     [an (delay-type
                           (let [opts (-> opts
                                          (assoc ::vs/current-env current-env)
                                          (with-frees (mapv make-F (force-type args opts))))
                                 t (parse-type an opts)]
                             (abstract-many (force-type args opts) t opts)))]))
                 ancests)
        ;_ (prn "collected ancestors" as)
        _ (add-datatype-ancestors checker s as)
        pos-ctor-name (symbol demunged-ns-str (str "->" local-name))
        map-ctor-name (symbol demunged-ns-str (str "map->" local-name))
        dt (let [bfn (fn []
                       (DataType* (force-type args opts) (force-type vs opts) (map make-F (force-type args opts)) s (force-type bnds opts) (force-type fs opts) record? opts))]
             (delay-type (bfn)))
        _ (add-datatype checker s dt opts)
        pos-ctor (let [bfn (fn []
                             (if args
                               (Poly* (force-type args opts) (force-type bnds opts)
                                      (make-FnIntersection
                                        (make-Function (vec (vals (force-type fs opts)))
                                                       (let [opts (with-bounded-frees opts (zipmap (map make-F (force-type args opts)) (force-type bnds opts)))]
                                                         (DataType-of s (map make-F (force-type args opts)) opts))))
                                      opts)
                               (make-FnIntersection
                                 (make-Function (vec (vals (force-type fs opts))) (DataType-of s opts)))))]
                   (delay-type (bfn)))]
    (do 
      ;(when vs
      ;  (let [f (mapv r/make-F (repeatedly (count vs) gensym))]
      ;    ;TODO replacements and unchecked-ancestors go here
      ;    (rcls/alter-class* s (c/RClass* (map :name f) (force-type vs opts) f s {} {} (force-type bnds opts) opts))))
      (add-tc-var-type checker pos-ctor-name pos-ctor)
      (add-nocheck-var checker pos-ctor-name)
      (when record?
        (let [map-ctor (let [bfn (fn []
                                   (let [hmap-arg ; allow omission of keys if nil is allowed and field is monomorphic
                                         (let [{optional true mandatory false} 
                                               (group-by (fn [[_ t]] (and (empty? (fv t opts))
                                                                          (empty? (fi t opts))
                                                                          (subtype? (-nil) t opts)))
                                                         (zipmap (map (comp -val keyword) (keys (force-type fs opts)))
                                                                 (vals (force-type fs opts))))]
                                           (make-HMap opts
                                                      {:optional (into {} optional)
                                                       :mandatory (into {} mandatory)}))]
                                     (if args
                                       (Poly* (force-type args opts) (force-type bnds opts)
                                              (make-FnIntersection
                                                (make-Function [hmap-arg] (DataType-of s (map make-F (force-type args opts)) opts)))
                                              opts)
                                       (make-FnIntersection
                                         (make-Function [hmap-arg] (DataType-of s opts))))))]
                         (delay-type (bfn)))]
          (impl-case opts
            :clojure (add-method-override checker (symbol (str s) "create") map-ctor)
            :cljs nil)
          (add-tc-var-type checker map-ctor-name map-ctor)
          (add-nocheck-var checker map-ctor-name)))))))
