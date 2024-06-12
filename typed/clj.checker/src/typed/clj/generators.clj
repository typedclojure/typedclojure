;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.generators
  "In development -- do not use yet."
  (:refer-clojure :exclude [delay])
  (:require [typed.cljc.checker.type-ctors :as c]
            [clojure.core.typed.type-contract :as type-contract]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.util-vars :as vs]
            [clojure.core.typed.parse-ast :as ast]
            [typed.cljc.checker.type-rep :as r]
            [typed.clj.checker.parse-unparse :as prs]
            [clojure.test.check :as qc]
            [clojure.test.check.generators :as gen]
            [com.gfredericks.test.chuck.generators :as gen']
            [com.gfredericks.test.chuck.properties :as prop']
            [clojure.test.check.generators :as gen]
            [clojure.core.typed :as t]
            [typed.clj.checker.subtype :as sub]
            [typed.cljc.checker.subst :as subst]
            [typed.cljc.checker.name-utils :as nme-utils]
            [io.github.frenchy64.fully-satisfies.safe-locals-clearing :refer [delay]])
  (:import [typed.cljc.checker.type_rep Top Value Union Name RClass HSequential Mu HeterogeneousMap
            TApp F FnIntersection Function TopFunction Poly]))

(declare generator type-rep->pred sub-bottom?)

(defn flip-parity [opt]
  (update opt ::parity #(case %
                          :input :output
                          :output :input)))

;; returns a sequence of checkers/gens corresponding to each Function in order
(defn- all-FnIntersection-checkers+gens
  [fs opts]
  {:pre [(instance? FnIntersection fs)]
   :post [(seq %)]}
  (let [_ (assert (every? #(= :fixed (:kind %)) fs)
                  (str "Can only generate functions with positional arguments."))
        f->checkers+gens
        (map (fn [f]
               {:pre [(r/Function? f)]}
               (let [dpreds (mapv #(type-rep->pred % (flip-parity opts)) (:dom f))
                     dgens (mapv generator (:dom f))
                     nargs (count (:dom f))
                     rett (-> f :rng :t)
                     bottom-args? (boolean (some #(sub-bottom? % opts) (:dom f)))]
                 {:f f
                  :nargs nargs 
                  :bottom-args? bottom-args?
                  :args-gen (apply gen/tuple dgens)
                  ;; assumption: number of args is correct
                  :args-checker (fn [args]
                                  (dorun
                                    (map (fn [i dpred arg]
                                           (when-not (dpred arg)
                                             (throw (ex-info (format "Arg %s failed validation: %s" i (pr-str arg))
                                                             {:arity f
                                                              :type fs
                                                              :nth-arg i
                                                              :arg arg
                                                              :dpred dpred}))))
                                         (range)
                                         dpreds
                                         args)))
                  :ret-gen (generator rett)
                  :ret-pred (type-rep->pred rett opts)}))
             (:types fs))]
   f->checkers+gens))

;; returns map from arity number to its checkers/gens. uses final arity on overloading.
(defn- FnIntersection-checkers+gens
  [fs opts]
  {:pre [(instance? FnIntersection fs)]
   :post [(map? %)]}
  (into {}
        (map (juxt :nargs identity))
        (all-FnIntersection-checkers+gens fs opts)))

;; :form should be fully qualified due to *verbose-types* below (still, we really need an `unparse-ast`)
(defn- ast->type [ast]
  {:pre [(:op ast)]
   :post [(r/Type? %)]}
  (prs/parse-clj (:form ast)))

;; FIXME unsuccessfully hacking around phasing mismatch with type-contract/type-syntax->pred
(defn- generative-Fn-pred-syntax [t-ast target-arg-syntax opts]
  {:pre [(= :Fn (:op t-ast))
         (simple-symbol? target-arg-syntax)]
   :post [(seq? %)]}
  (let [t->checkers+gens (fn [t]
                           (vec (all-FnIntersection-checkers+gens t opts)))
        pred `(let [t# (prs/parse-clj '~(:form t-ast))
                    checkers+gens# (vec (~t->checkers+gens t#))]
                (fn [f#]
                  (and (ifn? f#)
                       (or (every? :bottom-args? checkers+gens#)
                           (:pass?
                             (qc/quick-check
                               100
                               (prop'/for-all
                                 [cg# (gen/elements (vec (remove :bottom-args? checkers+gens#)))
                                  args# (:args-gen cg#)]
                                 (try ((:ret-pred cg#)
                                       (apply f# args#))
                                      (catch ThreadDeath e# (throw e#))
                                      ;; FIXME hmm, to mirror type checking's handling of t/Nothing, I think we need to catch some exceptions here.
                                      ;; that's a big departure from spec. we should allow t/Nothing fail in :input position, but not :output.
                                      ;; idea: catch all exceptions but detect whether a t/Nothing was triggered, and fail when
                                      ;; the "bad" direction is found (not sure what that is yet)
                                      ;; eg., fail if first arg called: [[t/Nothing :-> t/Any] :-> t/Any]
                                      ;; eg., succeed if first arg called: [[t/Any :-> t/Nothing] :-> t/Any]
                                      (catch Throwable _# true)))))))))]
    (list pred target-arg-syntax)))

(defn- generative-Poly-pred-syntax [t-ast target-arg-syntax opts]
  {:pre [(= :Poly (:op t-ast))
         (simple-symbol? target-arg-syntax)]
   :post [(seq? %)]}
  (let [t->pred (fn [t]
                  (type-rep->pred t opts))
        pred `(let [t# (prs/parse-clj '~(:form t-ast))
                    opts# ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                    nms# (c/Poly-fresh-symbols* t#)
                    bbnds# (c/Poly-bbnds* nms# t# opts#)
                    _# (assert (every? #{r/no-bounds} bbnds#))]
                (fn [f#]
                  (:pass?
                    (qc/quick-check
                      100
                      (prop'/for-all
                        [inst# (apply gen/tuple (map (fn [bnd#]
                                                       (gen/elements
                                                         (into []
                                                               (comp (distinct)
                                                                     (filter #(and (sub/subtype? (:lower-bound bnd#) %)
                                                                                   (sub/subtype? % (:upper-bound bnd#)))))
                                                               [r/-nothing
                                                                (:lower-bound bnd#)
                                                                r/-nil
                                                                (r/-val :x)
                                                                (c/Un [(r/-val ::x)
                                                                       (r/-val 1)]
                                                                      opts#)
                                                                (prs/parse-clj `t/Int)
                                                                (:upper-bound bnd#)
                                                                r/-any])))
                                                     bbnds#))]
                        (let [body# (c/instantiate-poly t# inst# opts#)
                              inst-pred# (apply ~t->pred [body#])]
                          ;(prn body#)
                          (inst-pred# f#)))))))]
    (list pred target-arg-syntax)))

(defn- type-rep->pred [t opts]
  {:pre [(r/Type? t)]}
  (impl/with-clojure-impl
    (let [opts (assoc opts ::vs/verbose-types true)]
      (-> t
          (prs/unparse-type opts) 
          ;; TODO do any other pred cases need parity flips?
          (type-contract/type-syntax->pred (assoc opts
                                                  ::type-contract/Fn-pred-syntax generative-Fn-pred-syntax
                                                  ::type-contract/Poly-pred-syntax generative-Poly-pred-syntax))
          eval))))

(defn- sub-bottom? [t opts]
  (sub/subtype? t r/-nothing opts))

(defonce -unreachable-generator (gen/fmap
                                  (fn [_]
                                    (throw (ex-info "Unreachable" {})))
                                  (gen/return nil)))

(defprotocol IGenerate
  (-generator [this opts]))

(defmulti RClass->generator (fn [t opts] (:the-class t)))
(defmethod RClass->generator 'java.lang.Short [_ _] (gen/fmap short (gen/large-integer* {:min Short/MIN_VALUE :max Short/MAX_VALUE})))
(defmethod RClass->generator 'java.lang.Byte [_ _] gen/byte)
(defmethod RClass->generator 'java.math.BigInteger [_ _] (gen/fmap #(java.math.BigInteger. (str %)) gen/size-bounded-bigint))
(defmethod RClass->generator 'java.lang.Integer [_ _] (gen/fmap int (gen/large-integer* {:min Integer/MIN_VALUE :max Integer/MAX_VALUE})))
(defmethod RClass->generator 'clojure.lang.BigInt [_ _] (gen/fmap bigint gen/size-bounded-bigint))
(defmethod RClass->generator 'java.lang.Long [_ _] (gen/fmap long (gen/large-integer* {:min Long/MIN_VALUE :max Long/MAX_VALUE})))
(defmethod RClass->generator 'java.lang.Boolean [_ _] gen/boolean)
(defmethod RClass->generator 'java.lang.String [_ _] gen/string-ascii)
(defmethod RClass->generator 'clojure.lang.Keyword [_ _] (gen/one-of [gen/keyword gen/keyword-ns]))
(defmethod RClass->generator 'clojure.lang.Symbol [_ _] (gen/one-of [gen/symbol gen/symbol-ns]))
(defmethod RClass->generator 'java.lang.Number [_ _] (gen/one-of [gen/size-bounded-bigint gen/double]))
(defmethod RClass->generator 'clojure.lang.IPersistentSet
  _ipersistentset
  [{:keys [poly?]} opts]
  (case (count poly?)
    0 (gen/set gen/any-printable-equatable)
    1 (let [[el] poly?]
        (if (sub-bottom? el opts)
          (gen/return #{})
          (gen/set (generator el opts))))))
(defmethod RClass->generator 'clojure.lang.IPersistentVector
  _ipersistentvector
  [{:keys [poly?]} opts]
  (case (count poly?)
    0 (gen/vector gen/any-printable-equatable)
    1 (let [[el] poly?]
        (if (sub-bottom? el opts)
          (gen/return [])
          (gen/vector (generator el opts))))))
(defmethod RClass->generator 'clojure.lang.ISeq
  _iseq
  [{:keys [poly?]} opts]
  (case (count poly?)
    0 (gen/bind (gen/vector gen/any-printable-equatable) sequence)
    1 (let [[el] poly?]
        (if (sub-bottom? el opts)
          (gen/return (sequence ()))
          (gen/bind (gen/vector (generator el opts)) sequence)))))
(defmethod RClass->generator 'clojure.lang.IPersistentCollection
  _ipersistentcollection
  [{:keys [poly?]} opts]
  (case (count poly?)
    0 (gen/one-of [(gen/vector gen/any-printable-equatable)
                   (gen/set gen/any-printable-equatable)
                   (gen/map gen/any-printable-equatable gen/any-printable-equatable)])
    1 (let [[el] poly?]
        (if (sub-bottom? el opts)
          (gen/elements [(sequence []) (list) [] #{} (sorted-set) {} (sorted-map)])
          (gen/one-of
            (filterv
              identity
              [(gen/bind (gen/vector (generator el opts)) #(apply list %))
               (gen/vector (generator el opts))
               (gen/bind (gen/set (generator el opts))
                         ;;TODO figure out if it's already a set or not
                         set)
               (when (sub/subtype? el (prs/parse-clj `'[t/Any t/Any]))
                 #_ ;;FIXME destructure type
                 (gen/map kgen vgen))]))))))

(extend-protocol IGenerate
  Top
  (-generator [_ _] gen/any-printable-equatable)
  Value
  (-generator [{:keys [val]} _] (gen/return val))
  Union
  (-generator [{:keys [types] :as t} opts]
    (let [gs (into []
                   (comp (map #(generator % opts))
                         (remove #{-unreachable-generator}))
                   types)]
      (if (empty? gs)
        -unreachable-generator
        (gen/one-of gs))))
  Name
  (-generator [{:keys [id] :as t} {:keys [alias-overrides] ::keys [name->gen] :as opts}]
    (or (when name->gen
          (name->gen id))
        (when alias-overrides
          (when-some [f (alias-overrides id)]
            (assert (not (contains? (::overriden-alias opts) id))
                    "Recursive alias was overriden without extending ::name->gen.")
            (f t (update opts ::overriden-alias (fnil conj #{}) id))))
        (let [_ (when (contains? (::non-recusive-names opts) id)
                  (throw (ex-info "Bug! Recursive alias does not have bound generator."
                                  {:opts opts
                                   :t t})))
              container-gen (c/-resolve t opts)
              [scalar-gen erased-frequencies] (nme-utils/erase-names container-gen #{id} opts)]
          (if-some [div (erased-frequencies id)]
            (gen/recursive-gen
              (fn [rec]
                (generator container-gen (assoc-in opts [::name->gen id]
                                                   ;; distribute size over all occurrences.
                                                   (gen/scale #(/ % (+ div (Math/pow (rand-int div) 2))) rec))))
              (generator scalar-gen opts))
            (generator container-gen (update opts ::non-recusive-names (fnil conj #{}) id))))))
  RClass
  (-generator [t opts]
    (RClass->generator t opts))

  TApp
  (-generator [t opts]
    ;;hmm what if the operator is recursive?
    (generator (c/-resolve t opts) opts))
  
  HSequential
  (-generator [t opts]
    (assert (not (:drest t)) t)
    (assert (not (:repeat t)) t)
    (let [vg (->> (gen/tuple (apply gen/tuple (map #(generator % opts) (:types t)))
                             (if (:rest t)
                               (gen/vector (generator (:rest t) opts))
                               (gen/return nil))
                             (if (:drest t)
                               (assert nil 'TODO)
                               (gen/return nil)))
                  (gen/fmap
                    (fn [[vg rst dst]]
                      {:post [(vector? %)]}
                      (assert (nil? dst))
                      (-> vg
                          (cond->
                            (:rest t) (into rst))))))]
      (case (:kind t)
        :vector vg
        :seq (gen/fmap sequence vg)
        :sequential (gen/bind vg
                              (fn [v]
                                (gen/elements ((juxt identity sequence #(apply list %)) v))))
        :list (gen/fmap #(apply list %) vg))))

  HeterogeneousMap
  (-generator [{:keys [types optional absent-keys other-keys?] :as t} opts]
    (let [mandatory-generators (map (fn [[k v]]
                                      [(generator k opts)
                                       (generator v opts)])
                                    types)]
      (if (some #{-unreachable-generator} (apply concat mandatory-generators))
        -unreachable-generator
        (gen/fmap 
          (fn [[mandatory optional]]
            (-> {}
                (into mandatory)
                (into optional)))
          (gen/tuple (apply gen/tuple
                            (map (fn [[kg vg]]
                                   (gen/tuple kg vg))
                                 mandatory-generators))
                     (gen/fmap
                       (fn [os]
                         (if (seq os)
                           (gen'/subsequence os)
                           []))
                       (apply gen/tuple
                              (mapcat (fn [[k v]]
                                        (let [kg (generator k opts)
                                              vg (generator k opts)]
                                          (when (not ((some-fn #{-unreachable-generator})
                                                      kg vg))
                                            [(gen/tuple kg vg)])))
                                      optional))))))))
  
  F
  (-generator [t {::keys [f->gen f-occ-atom] :as opts}]
              (let [g (and f->gen (f->gen (:name t)))]
                (assert g (str "No generator for free variable " t))
                (swap! f-occ-atom update (:name t) (fnil inc 0))
                g))

  Mu
  (-generator [t opts]
    (let [f (r/make-F (gensym "mu"))
          container-gen (c/unfold-Mu-with t f opts)
          scalar-gen (generator (c/unfold-Mu-with t r/-nothing opts) opts)
          opts (update opts ::f-occ-atom #(or % (atom {})))]
      (gen/recursive-gen
        (fn [rec]
          (generator container-gen (-> opts
                                       (assoc-in [::f->gen (:name f)]
                                                 (gen/scale
                                                   #(let [div (get @(::f-occ-atom opts) (:name f) 1)]
                                                      (/ % (+ div (Math/pow (rand-int div) 2))))
                                                   rec)))))
        scalar-gen)))

  ;; TODO test
  TopFunction
  (-generator [_ opts]
    (gen/return (fn [& args] (throw (ex-info "Non-returning function" {})))))

  FnIntersection
  (-generator [t opts]
    (let [arity->checkers+gens (FnIntersection-checkers+gens t opts)]
      (gen/sized
        (fn [size]
          (gen/return
            (fn [& args]
              (let [{:keys [args-checker ret-gen] :as m} (or (arity->checkers+gens (count args))
                                                             (throw (ex-info (format "Bad number of arguments %s to type %s" (count args) (pr-str t))
                                                                             {})))]
                (args-checker args)
                (gen/generate ret-gen size))))))))
  Poly
  (-generator [t opts]
    ;; idea: override :F case in ast->pred to collect vals on input positions and assert vals on output positions
    (assert nil "TODO Poly gen")
              #_
    (let [nms (c/Poly-fresh-symbols* t)
          bbnds (c/Poly-bbnds* nms t opts)
          _ (assert (every? #{r/no-bounds} bbnds))
          body (c/Poly-body* nms t opts)]
      (assert nil 'TODO)
      (generator
        opts))))

(def ^:private load-delay (delay (t/load-if-needed)))

(defn generator
  "Return a test.check generator for type t. t may be either an internal
  type or top-level type syntax.

  Options:
  :alias-overrides   Map of overrides for defalias's. Keys are symbols and values
                     are functions taking the overriden type and opts and returns
                     a generator. If alias is recursive must extend ::name->gen.
                     eg., :alias-overrides {'clojure.core.typed/Int (fn [_ _] gen/large-integer)}"
  ([t] (generator t ((requiring-resolve 'typed.clj.runtime.env/clj-opts))))
  ([t opts]
   @load-delay
   (impl/with-impl impl/clojure
     (let [t (cond-> t
               (not (r/AnyType? t)) (prs/parse-type opts))]
       (assert (< (get (::trace-freq opts) t 0) 3)
               (str "Already seen three times: " (prs/unparse-type t (assoc opts ::vs/verbose-types true)) " " (class t)
                    " " (pr-str (::trace opts))))
       (-generator t (-> opts
                         (update ::trace (fnil conj []) t)
                         (update-in [::trace-freq t] (fnil inc 0))))))))

(defn check
  "Dev helper to generatively test whether value v has type t."
  [t v]
  (t/load-if-needed)
  (impl/with-clojure-impl
    (let [t (cond-> t
              (not (r/Type? t)) prs/parse-clj)
          pred (type-rep->pred t (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))) ::parity :input)]
      (boolean (pred v)))))
