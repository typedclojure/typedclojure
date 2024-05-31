;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.spec.impl
  "Implementation details for typed.clj.spec{1,2}"
  (:refer-clojure :exclude [requiring-resolve delay])
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec :as s2]
            [clojure.spec.alpha :as s1]
            [clojure.alpha.spec.gen :as gen2]
            [clojure.alpha.spec.protocols :as protocols2]
            [clojure.walk :as walk]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]]
            [typed.clj.spec.impl.shim :as shim]
            [typed.clj.spec.protocols
             :refer [ITypeFn apply-typefn*
                     IPoly inst*]]
            [typed.clj.spec1.util :as u1]))

(set! *warn-on-reflection* true)

;; TODO rename these
(in-ns 'typed.clj.spec1)
(in-ns 'typed.clj.spec)
(in-ns 'typed.clj.spec.impl)
(alias 't2 'typed.clj.spec)
(alias 't1 'typed.clj.spec1)

(defn ^:private eprn [& args]
  (binding [*out* *err*]
    (apply prn args)))

;; version-info
(def spec1-version-info-base {:spec-version 1})
(def spec2-version-info-base {:spec-version 2})

(defn spec2-reify-body
  [spec-impl
   spec-version]
  `(protocols2/Spec
     (protocols2/conform*
       [this# x# settings-key# settings#]
       ((:conform* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :x x#
         :settings-key settings-key#
         :settings settings#}))

     (protocols2/unform*
       [this# x#]
       ((:unform* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :x x#}))
     (protocols2/explain*
       [this# path# via# in# x# settings-key# settings#]
       ((:explain* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :path path#
         :via via#
         :in in#
         :x x#
         :settings-key settings-key#
         :settings settings#}))
     (protocols2/gen*
       [this# overrides# path# rmap#]
       ((:gen* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :overrides overrides#
         :path path#
         :rmap rmap#}))
     (protocols2/with-gen*
       [this# gfn#]
       ((:with-gen* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :gfn gfn#}))
     (protocols2/describe*
       [this#]
       ((:describe* ~spec-impl)
        {:this this#
         :spec-version ~spec-version}))))

(defn spec1-reify-body
  [spec-impl
   spec-version]
  `(s1/Specize
     (s1/specize* [this#] this#)
     (s1/specize* [this# _#] this#)

    s1/Spec
     (s1/conform*
       [this# x#]
       ((:conform* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :x x#}))

     (s1/unform*
       [this# x#]
       ((:unform* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :x x#}))
     (s1/explain*
       [this# path# via# in# x#]
       ((:explain* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :path path#
         :via via#
         :in in#
         :x x#}))
     (s1/gen*
       [this# overrides# path# rmap#]
       ((:gen* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :overrides overrides#
         :path path#
         :rmap rmap#}))
     (s1/with-gen*
       [this# gfn#]
       ((:with-gen* ~spec-impl)
        {:this this#
         :spec-version ~spec-version
         :gfn gfn#}))
     (s1/describe*
       [this#]
       ((:describe* ~spec-impl)
        {:this this#
         :spec-version ~spec-version}))))

;; Note: requires both spec1 and spec2 on the classpath
;; to avoid this entire library becoming one big macro
(defmacro generic-spec-impl
  [spec-version
   spec-impl
   extra-impl]
  (let [impl-gs (gensym 'spec-impl)]
    `(let [~impl-gs ~spec-impl
           spec-version# ~spec-version]
       (case (int spec-version#)
         2 (reify
             ~@extra-impl
             ~@(spec2-reify-body
                 impl-gs
                 spec-version))
         1 (reify
             ~@extra-impl
             ~@(spec1-reify-body
                 impl-gs
                 spec-version))
         (throw (ex-info (str "Unknown spec version: " (pr-str spec-version#))
                         {:spec-version spec-version#}))))))

(deftype Reduced [val])

; modified from clojure.walk
(defn walk
  "Traverses form, an arbitrary data structure.  inner and outer are
  functions.  Applies inner to each element of form, building up a
  data structure of the same type, then applies outer to the result.
  Recognizes all Clojure data structures. Consumes seqs as with doall.
  On prewalk, short-circuits on Reduced. `outer` should not return Reduced."
  [inner outer config form]
  (let [inner #(inner config %)]
    (cond
     (instance? Reduced form) (outer (.val ^Reduced form))
     (list? form) (outer (apply list (map inner form)))
     (instance? clojure.lang.IMapEntry form)
     (outer (clojure.lang.MapEntry/create (inner (key form)) (inner (val form))))
     (seq? form) (outer (doall (map inner form)))
     (instance? clojure.lang.IRecord form)
       (outer (reduce (fn [r x] (conj r (inner x))) form form))
     (coll? form) (outer (into (empty form) (map inner form)))
     :else (outer form))))

; modified from clojure.walk
(defn prewalk
  "Like postwalk, but does pre-order traversal. Short-circuits on Reduced."
  {:added "1.1"}
  [f config form]
  (let [{:keys [config form]} (f config form)]
    (walk (partial prewalk f)
          identity
          config
          form)))

(declare subst-tv)

(defn subst-binder [binder sbst {:keys [spec-version] :as version-info}]
  {:pre [(map? sbst)
         (seq? binder)
         (#{(case (int spec-version)
              1 `t1/binder
              2 `t2/binder)}
           (first binder))
         (even? (count (rest binder)))]
   :post [(vector? %)
          (seq? (nth % 0))
          (map? (nth % 1))]}
  (-> (reduce (fn [[bpairs sbst] [b v]]
                {:pre [(vector? bpairs)
                       (map? sbst)
                       (simple-keyword? b)
                       (seq? v)
                       (#{(case (int spec-version)
                            1 `t1/bind-tv
                            2 `t2/bind-tv)}
                         (first v))]
                 :post [(vector? %)
                        (vector? (nth % 0))
                        (map? (nth % 1))]}
                (let [[_ & {self-name :name}] v]
                  [(conj bpairs
                         [b (subst-tv v
                                      (cond-> sbst
                                        self-name (dissoc self-name))
                                      version-info)])
                   (dissoc sbst b)]))
              [[] sbst]
              (partition 2 (rest binder)))
      (update 0 (fn [bpairs]
                  `(~(case (int spec-version)
                       1 `t1/binder
                       2 `t2/binder)
                           ~@(mapcat identity bpairs))))))

(defn subst-tv-all-like [spec sbst version-info]
  (let [[_ & {:keys [binder body] :as all-args}] spec
        extra-keys (set (keys (dissoc all-args :binder :body)))
        _ (when (seq extra-keys)
            (throw (ex-info (str "Found extra keys to " (first spec) ": " extra-keys)
                            {:form spec
                             :extra-keys extra-keys})))
        [binder sbst] (subst-binder binder sbst version-info)
        body (subst-tv body sbst version-info)]
    (list (first spec) :binder binder :body body)))

(defmulti subst-tv-op
  "Extension point for type-variable substitution over specs.
  Extend this multimethod if your spec interacts with type variable
  scope.
  
  To extend to symbolic spec with form (my.ns/foobar spec),
  extend subst-tv-op with a 'my.ns/foobar method that returns
  a fully-substituted spec wrt substitution sbst.

  eg., (defmethod subst-tv-op `my.ns/foobar
         [[_ spec] sbst {:keys [subst-tv] :as opt}]
         (subst-tv spec sbst opt))
  
  sbst is a map from type variable names to values to substitute
  for them.

  opt map consists of:
  - :subst-tv    a function taking a spec form and a substitution and
                 returns a spec form after applying the substitution.
  "
  (fn [spec sbst opt]
    (and (seq? spec)
         (first spec))))

(defmethod subst-tv-op `t1/tfn [spec sbst opt] (subst-tv-all-like spec sbst opt))
(defmethod subst-tv-op `t2/tfn [spec sbst opt] (subst-tv-all-like spec sbst opt))
(defmethod subst-tv-op `t1/all [spec sbst opt] (subst-tv-all-like spec sbst opt))
(defmethod subst-tv-op `t2/all [spec sbst opt] (subst-tv-all-like spec sbst opt))

(defn subst-tv-op-tv
  [spec sbst opt]
  (let [[_ tv & {:keys [wrap]
                 :or {wrap `identity}}]
        spec
        _ (assert (simple-keyword? tv))
        wrap (eval wrap) ;don't substitute tvs in wrap's form
        [_ sval :as found] (find sbst tv)]
    ;(prn "tv" spec tv sval)
    (if found
      ; substitute wrap's expansion.
      ; note: infinite loop possible, might need Rec spec.
      (subst-tv (wrap sval) sbst opt)
      spec)))

(defmethod subst-tv-op `t1/tv [spec sbst opt] (subst-tv-op-tv spec sbst opt))
(defmethod subst-tv-op `t2/tv [spec sbst opt] (subst-tv-op-tv spec sbst opt))

(defn coerce-dotted-cat [{:keys [spec-version]} tv xs]
  {:pre [(simple-keyword? tv)]}
  ;(prn "coerce-dotted-cat" tv xs)
  (let [cat (case (int spec-version)
              1 `s1/cat
              2 `s2/cat)]
    `(~cat ~@(mapcat (fn [i x]
                       [(keyword (str (name tv) i)) x])
                     (range)
                     xs))))

(comment
  (coerce-dotted-cat :a `[integer? boolean?])
  (coerce-dotted-cat :x '[(clojure.alpha.spec/every clojure.core/boolean? :count 35)])
  )

(defn subst-tv-op-fold-binders
  [spec sbst opt]
  (let [[_ template tv & {:keys [wrap]}] spec
        wrap (if wrap
               (eval wrap)
               #(coerce-dotted-cat opt tv %))
        [_ sval :as found] (find sbst tv)
        _ (assert (simple-keyword? tv))
        _ (assert ((some-fn nil? sequential?) sval)
                  sval)]
    (if found
      (let [;_ (prn "substituting fold-binders" sval)
            ; returns a vector for fast indexing
            stemplates (mapv ; vars bound by dotted variable shadow others
                            #(subst-tv template (merge sbst %) opt)
                            sval)
            ;_ (prn "stemplates" stemplates)
            wrapped (wrap stemplates)
            ;_ (prn "wrapped" wrapped)
            ]
         wrapped)
       spec)))

(defmethod subst-tv-op `t1/fold-binders [spec sbst opt] (subst-tv-op-fold-binders spec sbst opt))
(defmethod subst-tv-op `t2/fold-binders [spec sbst opt] (subst-tv-op-fold-binders spec sbst opt))

;TODO rigorous testing for capture-avoidance
(defn subst-tv [spec sbst opt]
  {:pre [(map? sbst)]}
  (let [res (prewalk (fn [sbst spec]
                       {:pre [(map? sbst)]}
                       (if (and (seq? spec)
                                (contains? (methods subst-tv-op) (first spec)))
                         {:config sbst
                          :form (->Reduced (subst-tv-op spec sbst (assoc opt :subst-tv subst-tv)))}
                         {:config sbst
                          :form spec}))
                     sbst
                     spec)]
    ;(prn 'subst-tv spec res)
    res))

(comment
  (subst-tv `(list (t2/tv :x :... (t2/tv :x)))
              {:x [1 2 3]})
  (subst-tv `(list (t2/tv :x :... (t2/tv :x)))
              {:x [1 2 3]})
  )

; binder and body are unresolved
(defn- tfn-impl
  [{:keys [spec-version] :as version-info} binder body gfn]
  (let []
    (generic-spec-impl
      spec-version
      ;only describe* makes sense for tfn. everything else uses generalized body.
      {:conform* (fn [{:keys [x settings-key settings]}]
                   (assert nil "conform TFn"))
       :unform* (fn [{:keys [x]}]
                  (assert nil "unform TFn"))
       :explain* (fn [{:keys [path via in x settings-key settings]}]
                   (assert nil "explain TFn"))
       :gen* (fn [{:keys [overrides path rmap]}]
               (assert nil "gen TFn"))
       :with-gen* (fn [{:keys [gfn]}]
                    (tfn-impl version-info binder body gfn))
       :describe* (fn [{:keys [spec-version]}]
                    (let [op (case (int spec-version)
                               1 `t1/tfn
                               2 `t2/tfn)]
                      `(~op :binder ~binder :body ~body)))}
      (ITypeFn
        (apply-typefn* [_ subst-map]
                       (let [bs (shim/resolve-spec binder version-info)
                             sbst (shim/conform bs subst-map version-info)]
                         (if (shim/invalid? sbst version-info)
                           (let [ed (shim/explain-data bs subst-map version-info)]
                             (throw (ex-info (str "Invalid substitution"
                                                  (when ed
                                                    (str "\n"
                                                         (with-out-str
                                                           (shim/explain-out ed version-info)))))
                                             {:binder binder
                                              :explain-data ed
                                              :subst-map subst-map})))
                           (-> body
                               (subst-tv subst-map version-info)
                               (shim/resolve-spec version-info)))))))))

#_
(defn- validate-poly-fn
  "returns f if valid, else smallest"
  [f specs iters]
  (let [g (s/gen (:args specs))
        prop (gen2/for-all* [g] #(call-valid? f specs %))]
    (let [ret (gen2/quick-check iters prop)]
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        f))))

(comment
  (gen2/generate (s/gen (s/resolve-spec `integer?)))
  (gen2/generate
    (s/gen
      (s/resolve-spec
        `(t2/bind-tv :position #{:input :output}
                    :lower (s/or)
                    :upper any?))))
  (gen2/generate
    (s/gen
      (s/resolve-spec
        `(s/tuple #{:x} (t2/bind-tv :position #{:input :output}
                                   :lower (s/or)
                                   :upper any?)))))
  (s/conform
    (s/resolve-spec
      `(t2/all [:x (t2/bind-tv :position #{:input :output}
                           :lower (s/or)
                           :upper any?)
             :y (t2/bind-tv :position #{:input :output}
                           :lower (s/or)
                           :upper any?)]
            (s/fspec :args (s/cat :fn (s/fspec :args (s/cat :x (t2/tv :x))
                                               :ret (t2/tv :y))
                                  :coll (s/coll-of (t2/tv :x)))
                     :ret (s/coll-of (t2/tv :y)))))
    #_map
    (fn [f c]
      [1 2]))

  (-> (t2/bind-tv)
      s/gen
      gen2/generate)
  (s/conform
    (s/resolve-spec `symbol?)
    (gensym))
  (-> (clojure.test-clojure.typed/bind-tv :upper symbol?)
      s/form)
  (-> (clojure.test-clojure.typed/bind-tv :upper symbol?)
      s/gen
      gen2/generate)
  (-> (gen2/return `integer?)
      gen2/generate)

  (s/conform
    (s/describe
    (s/resolve-spec
      `(t2/inst
         (t2/all [:x (t2/bind-tv)
               :y (s/with-gen
                    (t2/bind-tv)
                    #(gen2/return
                       '~`(t2/tv :x)))]
              (s/fspec :args (s/cat :pred (s/with-gen
                                            (s/fspec :args (s/cat :in (t2/tv :x))
                                                     :fn (fn [{[~'in] :args :keys [~'ret]}]
                                                           (= (boolean ~'ret)
                                                              (s/valid? (s/resolve-spec '~`(t2/tv :y)) ~'in)))
                                                     :ret any?)
                                            #(gen2/return
                                               (fn [~'x]
                                                 (gen2/generate
                                                   (if (s/valid? (s/resolve-spec '~`(t2/tv :y)) ~'x)
                                                     (gen2/such-that identity (gen2/any-printable))
                                                     (gen2/one-of [(gen2/return nil)
                                                                  (gen2/return false)]))))))
                                    :coll (s/nilable (s/coll-of (t2/tv :x))))
                       :ret (s/coll-of (t2/tv :y))))
         {:x number?
          :y integer?})))
    filter)

  (s/conform
    `(t2/all [:x (t2/bind-tv :dotted true
                         :position #{:input :output}
                         :lower (s/or)
                         :upper any?)]
          (s/fspec :args (s/cat :els (s/spec (s/tuple (t2/tv :x :... (t2/tv :x)))))
                   :ret (s/tuple (t2/tv :x :... (t2/tv :x)))))
    vector)

  (s/tuple (t2/tv... :y (t2/tv :y)))
  )

(def sample-seq
  (delay
    (requiring-resolve 'clojure.test.check.generators/sample-seq)))

; assumes lower <: upper and lower == (or) for the moment
; takes unresolved spec forms
(defn- generator-between [{:keys [spec-version] :as version-info} gen*-args lower upper]
  ;(eprn 'generator-between lower upper)
  ;; impl integrating generator
  #_
  (let [rupper (shim/resolve-spec upper version-info)
        rgen (shim/gen* rupper gen*-args version-info)
        make-set-spec (fn [els]
                        (cond->> (set els)
                          ;; FIXME I have no idea what to do here
                          ;; must be self-evaluating in spec1
                          (= 1 spec-version)
                          (list 'quote)))
        set-spec-compatible? #(try ;ensure it's equivable
                                   (shim/valid? version-info
                                                (shim/resolve-spec (make-set-spec #{%}) version-info)
                                                (walk/prewalk identity %))
                                   (catch Error e false))
        singleton-spec-gen (fn [spec-gen]
                             (gen2/bind (gen2/such-that set-spec-compatible? spec-gen)
                                        (fn [el]
                                          (gen2/return (make-set-spec #{el})))))]
    (gen2/frequency
      [[10 (singleton-spec-gen rgen)]
       ;; TODO generate supertypes
       ]
      ))
  ;; original impl
  (let [rupper (shim/resolve-spec upper version-info)
        rgen (shim/gen* rupper gen*-args version-info)
        tries 100
        ncandidates 20
        ; try and find ncandidates varied constant values
        gs (into []
                 (comp
                   (take tries)
                   (filter #(try ;ensure it's equivable
                                 (shim/valid? version-info (shim/resolve-spec #{%} version-info) (walk/prewalk identity %))
                                 (catch Error e)))
                   (distinct)
                   (take ncandidates))
                 (@sample-seq rgen))]
    (if (seq gs)
      (let [el (rand-nth gs)]
        ;(eprn "generator-between el" (pr-str el))
        (gen2/return (cond->> #{el}
                       ;; FIXME I have no idea what to do here
                       ;; must be self-evaluating in spec1
                       (= 1 spec-version)
                       (list 'quote))))
      (throw (ex-info (str "Could not find a constant value between lower and upper "
                           "after " tries " tries")
                      {:lower lower
                       :upper upper
                       :tries tries
                       :ncandidates ncandidates})))))

; all unresolved
(defn- bind-tv-impl
  [{:keys [spec-version] :as version-info} tv position kind gen gfn]
  (let [bind-tv-op (case (int spec-version)
                     1 `t1/bind-tv
                     2 `t2/bind-tv)]
    (generic-spec-impl
      spec-version
      {:conform* (fn [conform*-args]
                   (shim/conform* (shim/resolve-spec kind version-info)
                                  conform*-args
                                  version-info))
       :unform* (fn [{:keys [x]}]
                  (assert nil "TODO"))
                
       :explain* (fn [explain*-args]
                   (shim/explain* (shim/resolve-spec kind version-info)
                                  (-> explain*-args
                                      (update :path conj :kind)
                                      (update :via conj bind-tv-op))
                                  version-info))
       :gen* (fn [gen*-args]
               ;(eprn "bind-tv gen*: kind" kind)
               (if gfn
                 (gfn)
                 (if gen
                   ((eval gen))
                   (-> kind
                       (shim/resolve-spec version-info)
                       (shim/gen* gen*-args version-info)))))
       :with-gen* (fn [{:keys [gfn]}]
                    (bind-tv-impl version-info tv position kind nil gfn))
       :describe* (fn [_]
                    `(~bind-tv-op ~@(some->> tv (vector :name))
                                  :position ~position
                                  :kind ~kind
                                  ~@(some->> gen (vector :gen))))}
      ())))

; cases = (Map Int v)
; default = (U nil v)
(defn- fcase-impl
  [{:keys [spec-version] :as version-info} cases default gfn]
  (let []
    (generic-spec-impl
      spec-version
      {:conform* (fn [{:keys [x] :as conform*-args}]
                   (reduce (fn [x spec]
                             (let [res (shim/conform* (shim/resolve-spec spec version-info)
                                                      (assoc conform*-args :x x)
                                                      version-info)]
                               (cond-> res
                                 (shim/invalid? res version-info) reduced)))
                           x
                           (concat (vals cases)
                                   (some-> default vector))))
       :unform* (fn [{:keys [x]}]
                  (assert nil "TODO"))
                
       :explain* (fn [explain*-args]
                   (some
                     (fn [[path spec]]
                       (shim/explain* (shim/resolve-spec spec version-info)
                                      (-> explain*-args
                                          (update :in conj path))
                                      version-info))
                     (concat cases (some->> default (vector :default)))))
                 
       :gen* (fn [{:keys [overrides]}]
               (if gfn
                 (gfn)
                 (gen2/return
                   (fn [& args]
                     (let [spec (loop [cases cases
                                       n 0
                                       args-tail args]
                                  (if (empty? cases)
                                    (or default
                                        (throw (ex-info (str "No fcase match for " n " or more arguments")
                                                        {:n n
                                                         :cases cases
                                                         :args args})))
                                    (if (empty? args-tail)
                                      (or (cases n)
                                          default
                                          (throw (ex-info (str "No fcase match for " n " arguments")
                                                          {:n n
                                                           :cases cases
                                                           :args args})))
                                      (recur (dissoc cases n)
                                             (inc n) 
                                             (next args-tail)))))
                           gf (-> spec
                                  (shim/resolve-spec version-info)
                                  (shim/gen overrides version-info)
                                  gen2/generate)]
                       (apply gf args))))))
       :with-gen* (fn [{:keys [gfn]}]
                    (fcase-impl version-info cases default gfn))
       :describe* (fn [_]
                    (let [op (case (int spec-version)
                               1 `t1/fcase
                               2 `t2/fcase)]
                      `(~op ~@(mapcat (fn [[v gs]]
                                        (let [ks (set (map first gs))]
                                          [(if (= 1 (count ks))
                                             (first ks)
                                             (set ks))
                                           v]))
                                      (group-by val cases))
                            ~@(some-> default vector))))}
      ())))

(def ^:dynamic *all-iterations* 10)

;based on clojure.alpha.spec.impl/validate-fn
(defn- validate-all
  "returns x if valid, else smallest"
  [{:keys [spec-version] :as version-info} x {:keys [binder body]} iters]
  ;(prn "validate-all ...")
  (let [g (-> binder
              (shim/resolve-spec version-info)
              (shim/gen version-info))
        prop (gen2/for-all* [g] #(let [;_ (prn "subst" %)
                                       sbody (subst-tv body % version-info)
                                       ;_ (prn "rbody" sbody)
                                       rbody (shim/resolve-spec sbody version-info)]
                                   ;(prn "subst" %)
                                   ;(prn "before body" (-> body s/resolve-spec s/describe))
                                   ;(prn "body" (s/describe rbody))
                                   (shim/valid? version-info rbody x)))]
    (let [ret (gen2/quick-check iters prop)]
      ;(prn "validate-all post qc")
      (if-let [[smallest] (-> ret :shrunk :smallest)]
        smallest
        x))))

(comment
  (clojure.repl/pst 100))

;FIXME spec2 roundtrip (s/spec (t2/all ..)) and (s/spec (t2/tfn ..))
(defn- all-impl
  [{:keys [spec-version] :as version-info} binder body gfn]
  (let [;; TODO cache resolve-spec on binder
        specs {:binder binder
               :body body}]
    (generic-spec-impl
      spec-version
      {:conform* (fn [{:keys [x]}]
                   ;(prn "all conform..")
                   (if (and binder body)
                     (if (identical? x (validate-all version-info x specs *all-iterations*))
                       x
                       (shim/invalid-kw version-info))
                     (throw (ex-info "Conform 't/all' requires both binder and body"
                                     {:binder binder
                                      :body body}))))
       :unform* (fn [{:keys [x]}] x)
       :explain* (fn [{:keys [x] :as explain*-args}]
                   ;(prn "all explain..")
                   (let [subst (validate-all version-info x specs 100)]
                     ;(prn "x" (pr-str x))
                     (if (identical? x subst) ;;hrm, we might not be able to reproduce
                       nil
                       (let [inst-op (case (int spec-version)
                                       1 `t1/inst
                                       2 `t2/inst)
                             all-op (case (int spec-version)
                                      1 `t1/all
                                      2 `t2/all)
                             inst-body (subst-tv body subst version-info)]
                         #_(eprn "inst-body" inst-body
                               explain*-args)
                         (shim/explain*
                           (shim/resolve-spec inst-body version-info)
                           (-> explain*-args
                               ;; FIXME :in or :path here?
                               (update :in (fnil conj []) (list inst-op subst)))
                           version-info)))))
       :gen* (fn [_]
               (assert nil))
       :with-gen* (fn [{:keys [gfn]}]
                    (all-impl version-info binder body gfn))
       :describe* (fn [_]
                    (let [op (case (int spec-version)
                               1 `t1/all
                               2 `t2/all)]
                      `(~op ~binder ~body)))}
      (IPoly
        (inst* [_ subst-map]
               (let [bs (shim/resolve-spec binder version-info)
                     ;_ (prn "inst*" subst-map)
                     sbst (shim/conform bs subst-map version-info)]
                 (if (shim/invalid? sbst version-info)
                   (let [ed (shim/explain-data bs subst-map version-info)]
                     (throw (ex-info (str "Invalid substitution"
                                          (when ed
                                            (str "\n"
                                                 (with-out-str
                                                   (shim/explain-out ed version-info)))))
                                     {:binder binder
                                      :explain-data ed
                                      :subst-map subst-map})))
                   (-> body
                       (subst-tv sbst version-info)
                       (shim/resolve-spec version-info)))))))))

(defn- binder-impl
  [{:keys [spec-version] :as version-info} tvs gfn]
  {:pre [(even? (count tvs))
         (apply distinct? (map first (partition 2 tvs)))]}
  (let [binder-op (case (int spec-version)
                    1 `t1/binder
                    2 `t2/binder)]
    (generic-spec-impl
      spec-version
      {; (conform (t2/binder :x (t2/bind-tv))
       ;          `{:x integer?})
       ; => `{:x integer?}
       :conform* (fn [{:keys [x] :as conform-args}]
                   ;note: doesn't blow up if x has extra entries
                   (if (not (map? x))
                     (shim/invalid-kw version-info)
                     (reduce (fn [smap [b v]]
                               (let [rsspec (shim/resolve-spec (subst-tv v smap version-info)
                                                               version-info)]
                                 (if-let [[_ i] (find x b)]
                                   (let [ic (shim/conform* rsspec
                                                           (assoc conform-args :x i)
                                                           version-info)]
                                     (if (shim/invalid? ic version-info)
                                       (reduced ic)
                                       (assoc smap b ic)))
                                   ; infer missing entries
                                   (let [;_ (prn "gv" (s/describe rsspec))
                                         gv (try (-> rsspec (shim/gen version-info) gen2/generate)
                                                 (catch Exception e
                                                   (throw (ex-info (str "Error catch while generating default instantiation"
                                                                        " for" (shim/describe rsspec version-info))
                                                                   {:spec (shim/form rsspec version-info)
                                                                    :smap smap
                                                                    :b b}
                                                                   e))))]
                                     (assoc smap b gv)))))
                             {}
                             (partition 2 tvs))))
       :unform* (fn [{:keys [x]}]
                  (assert nil))
       :explain* (fn [{:keys [path via in x settings-key settings]}]
                   ;note: doesn't blow up if x has extra entries
                   (case (int spec-version)
                     2 (if (not (map? x))
                         [{:path path :pred `map? :val x :via (conj via binder-op) :in in}]
                         (let [problem (atom nil)
                               _ (reduce (fn [smap [b v]]
                                           (let [rsspec (shim/resolve-spec (subst-tv v smap version-info)
                                                                           version-info)]
                                             (if-let [[_ i] (find x b)]
                                               (let [ic (protocols2/conform* rsspec
                                                                             i
                                                                             settings-key
                                                                             settings)]
                                                 (if (s/invalid? ic)
                                                   (reduced (reset! problem (protocols2/explain* rsspec (conj path b) (conj via binder-op) in i settings-key settings)))
                                                   (assoc smap b ic)))
                                               ; infer missing entries
                                               (assoc smap b (-> rsspec (shim/gen version-info) gen2/generate)))))
                                         {}
                                         (partition 2 tvs))]
                           @problem))))
       :gen* (fn [gen*-args]
               (let [; (s/coll-of (s/tuple Kw Spec)) (s/map-of Kw Spec) -> Generator
                     bgen (fn bgen [tvs gvs]
                            (if (empty? tvs)
                              (gen2/return gvs)
                              (let [[[b v] & tvs] tvs
                                    sspec (subst-tv v gvs version-info)
                                    ;_ (prn "binder gen*: sspec" sspec)
                                    rspec (shim/resolve-spec sspec version-info)]
                                (gen2/bind (shim/gen* rspec gen*-args version-info)
                                           (fn [gv]
                                             (bgen tvs (assoc gvs b gv)))))))]
                 ; allows the first gen* to be chosen multiple times
                 (gen2/bind (gen2/return nil)
                            (fn [_]
                              (bgen (partition 2 tvs) {})))))
       :with-gen* (fn [{:keys [gfn]}]
                    (binder-impl version-info tvs gfn))
       :describe* (fn [_]
                    `(~binder-op ~@tvs))}
      ())))

(comment
  (s/conform (t2/binder :x (t2/bind-tv))
             {:x #{(gensym)}})
  (s/conform (t2/binder :x (t2/bind-tv)
                     :y (t2/bind-tv))
             {:x #{(gensym)}})
  (s/conform (t2/binder :x (t2/bind-tv :kind nat-int?)
                     :y (t2/bind-tv :kind nat-int?))
             {:x 24})
  (s/conform (t2/binder :x (t2/bind-tv :kind nat-int?)
                     :y (t2/bind-tv :kind nat-int?))
             {:x 24
              :y 42})
  (s/conform (t2/binder :x (t2/bind-tv :kind nat-int?)
                     :y (t2/bind-tv :kind (s/int-in 0 (t2/tv :x))))
             {:x 2})
  (s/conform (t2/binder :x (t2/bind-tv :kind (s/int-in 0 5))
                     :y (t2/bind-tv :kind (s/coll-of integer?
                                                    :count (t2/tv :x))))
             {:x 4})
  (gen2/sample
    (s/gen (t2/binder :x (t2/bind-tv)
                   :y (t2/bind-tv))))
  (gen2/sample
    (s/gen (t2/binder :x (t2/bind-tv :kind nat-int?)
                   :y (t2/bind-tv :kind nat-int?))))

  (gen2/sample
    (gen2/keyword))

  (gen2/sample
    (s/gen (t2/binder)))

  (s/conform (t2/all :binder (t2/binder :x (t2/bind-tv))
                  :body (t2/tv :x))
             1)

  (s/conform (t2/all :binder (t2/binder :x (t2/bind-tv))
                  :body (s/fspec :args (s/cat :x (t2/tv :x))
                                 :ret (t2/tv :x)))
             (fn [a]
               (prn a)
               a))
  )

;Notes:
; - reread doc/infer-detail.md to remind myself about splicing dotted and a starred pretypes
; - they're more general than dotted pretypes and the deeper abstraction I'm looking for
;   to generate fold-binders is probably easier to find with a spliced-fold-binders op
(defn- fold-binders-impl
  [{:keys [spec-version] :as version-info} template tv wrap]
  (generic-spec-impl
    spec-version
    {:conform* (fn [{:keys [x settings-key settings]}]
                 (assert nil "conform"))
     :unform* (fn [{:keys [x]}]
                (assert nil "unform"))
     :explain* (fn [{:keys [path via in x settings-key settings]}]
                 (assert nil "explain*"))
     :gen* (fn [{:keys [overrides path rmap]}]
             (assert nil "gen*"))
     :with-gen* (fn [{:keys [gfn]}]
                  (assert nil "with-gen*"))
     :describe* (fn [_]
                  (let [op (case (int spec-version)
                             2 `t2/fold-binders)]
                    `(~op ~template ~tv :wrap ~wrap)))}
    ()))

(defn- spec-between-impl
  [{:keys [spec-version] :as version-info} lower upper gfn]
  (let [rupper (delay (shim/resolve-spec upper version-info))]
    (generic-spec-impl
      spec-version
      {:conform* (fn [{:keys [x] :as conform*-args}]
                   (if (set? x)
                     (into #{}
                           (map #(shim/conform*
                                   @rupper
                                   (assoc conform*-args :x %)
                                   version-info))
                           x)
                     ;FIXME use a subtyping relation instead of this brittle hack
                     (if (every? #(shim/valid? version-info @rupper %)
                                 (-> x
                                     (shim/resolve-spec version-info)
                                     (shim/gen version-info)
                                     (gen2/sample 10)))
                       x
                       (assert nil (str "spec-between-impl conform: " (pr-str x))))))
       :unform* (fn [{:keys [x]}]
                  (assert nil "unform"))
       :explain* (fn [{:keys [path via in x settings-key settings]}]
                   (assert nil "spec-between explain*"))
       :gen* (fn [gen*-args]
               (if gfn
                 (gfn)
                 (generator-between version-info gen*-args lower upper)))
       :with-gen* (fn [{:keys [gfn]}]
                    (spec-between-impl version-info lower upper gfn))
       :describe* (fn [_]
                    (let [op (case (int spec-version)
                               1 `t1/spec-between
                               2 `t2/spec-between)]
                      `(~op :lower ~lower
                            :upper ~upper)))}
      ())))

(defn expand-tfn [{:keys [spec-version]} [op & {:keys [binder body] :as opt}]]
  {:pre [(empty? (dissoc opt :binder :body))]}
  {:clojure.spec/op op
   :binder binder
   :body body})

(defmethod u1/expand-spec `t1/tfn [qform] (expand-tfn spec1-version-info-base qform))
(defmethod s/expand-spec `t2/tfn [qform] (expand-tfn spec2-version-info-base qform))

(defn create-tfn [version-info {:keys [binder body]}]
  (tfn-impl version-info binder body nil))

(defmethod u1/create-spec `t1/tfn [m] (create-tfn spec1-version-info-base m))
(defmethod s/create-spec `t2/tfn [m] (create-tfn spec2-version-info-base m))

(defn expand-bind-tv
  [{:keys [spec-version] :as version-info}
   [op & {tv :name
          :keys [position kind gen]
          :or {position #{:input :output}}
          :as opt}]]
  {:pre [(set? position)
         ((some-fn nil? simple-keyword?) tv)
         (empty? (apply dissoc opt [:name :position :kind :gen]))]}
  (let [kind (or kind
                 (case (int spec-version)
                   1 `(t1/spec-between)
                   2 `(t2/spec-between)))]
    {:clojure.spec/op op
     :tv tv
     :position position
     :kind kind
     :gen gen}))

(defmethod u1/expand-spec `t1/bind-tv [qform] (expand-bind-tv spec1-version-info-base qform))
(defmethod s/expand-spec `t2/bind-tv [qform] (expand-bind-tv spec2-version-info-base qform))

(defn create-bind-tv
  [version-info {:keys [tv position kind gen]}]
  (bind-tv-impl version-info tv position kind gen nil))

(defmethod u1/create-spec `t1/bind-tv [m] (create-bind-tv spec1-version-info-base m))
(defmethod s/create-spec `t2/bind-tv [m] (create-bind-tv spec2-version-info-base m))

(defn expand-all
  [version-info [op & {:keys [binder body]}]]
  {:pre [binder
         body]}
  {:clojure.spec/op op
   :binder binder
   :body body})

(defmethod u1/expand-spec `t1/all [qform] (expand-all spec1-version-info-base qform))
(defmethod s/expand-spec `t2/all [qform] (expand-all spec2-version-info-base qform))

(defn create-all
  [version-info {:keys [binder body]}]
  (all-impl version-info binder body nil))

(defmethod u1/create-spec `t1/all [m] (create-all spec1-version-info-base m))
(defmethod s/create-spec `t2/all [m] (create-all spec2-version-info-base m))

(defn expand-binder
  [version-info [op & tvs]]
  {:clojure.spec/op op
   :tvs tvs})

(defmethod u1/expand-spec `t1/binder [qform] (expand-binder spec1-version-info-base qform))
(defmethod s/expand-spec `t2/binder [qform] (expand-binder spec2-version-info-base qform))

(defn create-binder
  [version-info {:keys [tvs]}]
  (binder-impl version-info tvs nil))

(defmethod u1/create-spec `t1/binder [m] (create-binder spec1-version-info-base m))
(defmethod s/create-spec `t2/binder [m] (create-binder spec2-version-info-base m))

(defn expand-tapp
  [version-info [op tfn args-map & more]]
  {:pre [tfn
         args-map
         (not more)]}
  {:clojure.spec/op op
   :tfn tfn
   :args-map args-map})

(defmethod u1/expand-spec `t1/tapp [qform] (expand-tapp spec1-version-info-base qform))
(defmethod s/expand-spec `t2/tapp [qform] (expand-tapp spec2-version-info-base qform))

(defn create-tapp
  [version-info {:keys [tfn args-map]}]
  (apply-typefn* (shim/resolve-spec tfn version-info) args-map))

(defmethod u1/create-spec `t1/tapp [m] (create-tapp spec1-version-info-base m))
(defmethod s/create-spec `t2/tapp [m] (create-tapp spec2-version-info-base m))

(defn expand-inst
  [version-info [op poly args-map & more]]
  {:pre [poly
         args-map
         (not more)]}
  {:clojure.spec/op op
   :poly poly
   :args-map args-map})

(defmethod u1/expand-spec `t1/inst [qform] (expand-inst spec1-version-info-base qform))
(defmethod s/expand-spec `t2/inst [qform] (expand-inst spec2-version-info-base qform))

(defn create-inst
  [version-info {:keys [poly args-map]}]
  (inst* (shim/resolve-spec poly version-info) args-map))

(defmethod u1/create-spec `t1/inst [m] (create-inst spec1-version-info-base m))
(defmethod s/create-spec `t2/inst [m] (create-inst spec2-version-info-base m))

(defn expand-tv
  [version-info [op tv & {:keys [wrap] :as opt}]]
  {:pre [(simple-keyword? tv)
         (empty? (dissoc opt :wrap))]}
  {:clojure.spec/op op
   :tv tv
   :wrap wrap})

(defmethod u1/expand-spec `t1/tv [qform] (expand-tv spec1-version-info-base qform))
(defmethod s/expand-spec `t2/tv [qform] (expand-tv spec2-version-info-base qform))

(defn create-tv
  [version-info {:keys [tv wrap]}]
  (throw (ex-info (str "Cannot create tv spec for " tv)
                  {:tv tv
                   :wrap wrap})))

(defmethod u1/create-spec `t1/tv [m] (create-tv spec1-version-info-base m))
(defmethod s/create-spec `t2/tv [m] (create-tv spec2-version-info-base m))

(defn expand-fcase
  [version-info [op & args :as form]]
  (let [vargs (vec args)
        [default args] (if (odd? (count args))
                         ((juxt peek pop) vargs)
                         [nil args])
        ; flatten cases
        cases (reduce (fn [m [k v]]
                        (if (contains? m k)
                          (throw (ex-info (str "Duplicate fcase entry for arity " k)
                                          {:form form}))
                          (assoc m k v)))
                      {}
                      (mapcat (fn [[k v]]
                                (if (set? k)
                                  (map #(vector % v) k)
                                  [[k v]]))
                              (partition 2 vargs)))]
    {:clojure.spec/op op
     :cases cases
     :default default}))

(defmethod u1/expand-spec `t1/fcase [qform] (expand-fcase spec1-version-info-base qform))
(defmethod s/expand-spec `t2/fcase [qform] (expand-fcase spec2-version-info-base qform))

(defn create-fcase
  [version-info {:keys [cases default]}]
  (fcase-impl version-info cases default nil))

(defmethod u1/create-spec `t1/fcase [m] (create-fcase spec1-version-info-base m))
(defmethod s/create-spec `t2/fcase [m] (create-fcase spec2-version-info-base m))

(defn expand-fold-binders
  [version-info [op template tv & opt]]
  {:pre [(simple-keyword? tv)]}
  (let [{:keys [wrap]
         :or {wrap `#(coerce-dotted-cat ~version-info ~tv %)}}
        opt]
    {:clojure.spec/op op
     :template template
     :tv tv
     :wrap wrap}))

(defmethod u1/expand-spec `t1/fold-binders [qform] (expand-fold-binders spec1-version-info-base qform))
(defmethod s/expand-spec `t2/fold-binders [qform] (expand-fold-binders spec2-version-info-base qform))

(defn create-fold-binders
  [version-info {:keys [template tv wrap]}]
  (fold-binders-impl version-info template tv wrap))

(defmethod u1/create-spec `t1/fold-binders [m] (create-fold-binders spec1-version-info-base m))
(defmethod s/create-spec `t2/fold-binders [m] (create-fold-binders spec2-version-info-base m))

(defn expand-spec-between
  [{:keys [spec-version]}
   [op & {:keys [upper lower]
          :or {upper `any?}}]]
  (let [lower (or lower
                  (case (int spec-version)
                    1 `(s1/or)
                    2 `(s2/or)))]
    {:clojure.spec/op op
     :lower lower
     :upper upper}))

(defmethod u1/expand-spec `t1/spec-between [qform] (expand-spec-between spec1-version-info-base qform))
(defmethod s/expand-spec `t2/spec-between [qform] (expand-spec-between spec2-version-info-base qform))

(defn create-spec-between
  [version-info {:keys [lower upper]}]
  (spec-between-impl version-info lower upper nil))

(defmethod u1/create-spec `t1/spec-between [m] (create-spec-between spec1-version-info-base m))
(defmethod s/create-spec `t2/spec-between [m] (create-spec-between spec2-version-info-base m))

;; reduced-of

(defn expand-reduced-of
  [version-info [op s & more]]
  {:pre [(not more)]}
  {:clojure.spec/op op
   :s s})

(defmethod u1/expand-spec `t1/reduced-of [qform] (expand-reduced-of spec1-version-info-base qform))
(defmethod s/expand-spec `t2/reduced-of [qform] (expand-reduced-of spec2-version-info-base qform))

(defn- reduced-of-impl [{:keys [spec-version] :as version-info} s gfn]
  (generic-spec-impl
    spec-version
    {:conform* (fn [{:keys [x] :as conform*-args}]
                 (if (reduced? x)
                   (let [res (shim/conform* (shim/resolve-spec s version-info)
                                            (assoc conform*-args :x @x)
                                            version-info)]
                     (if (shim/invalid? res version-info)
                       res
                       (if (identical? res x)
                         x
                         (reduced res))))
                   (shim/invalid-kw version-info)))
     :unform* (fn [{:keys [x]}]
                (assert nil "TODO"))
              
     :explain* (fn [{:keys [path via in x settings-key settings]}]
                 (assert nil "TODO"))
               
     :gen* (fn [{:keys [overrides]}]
             (if gfn
               (gfn)
               (gen2/fmap reduced (-> s
                                      (shim/resolve-spec version-info)
                                      (shim/gen version-info)))))
     :with-gen* (fn [{:keys [gfn]}]
                  (reduced-of-impl version-info s gfn))
     :describe* (fn [_]
                  (let [op (case (int spec-version)
                             1 `t1/reduced-of
                             2 `t2/reduced-of)]
                    `(~op ~s)))}
    ()))

(defn create-reduced-of [version-info {:keys [s]}]
  (reduced-of-impl version-info s nil))

(defmethod u1/create-spec `t1/reduced-of [m] (create-reduced-of spec1-version-info-base m))
(defmethod s/create-spec `t2/reduced-of [m] (create-reduced-of spec2-version-info-base m))
