;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.parse-unparse
  (:refer-clojure :exclude [requiring-resolve])
  (:require [clojure.core.typed.coerce-utils :as coerce]
            [typed.clojure :as t]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.contract-utils-platform-specific :as plat-con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]
            [typed.cljc.checker.constant-type :as const]
            [typed.cljc.analyzer.passes.uniquify :as uniquify]
            [typed.cljc.checker.filter-ops :as fl]
            [typed.cljc.checker.filter-rep :as f]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.hset-utils :as hset]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.path-rep :as pthrep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env-utils :as env-utils])
  (:import (typed.cljc.checker.type_rep NotType DifferenceType Intersection Union FnIntersection
                                        DottedPretype Function Regex RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Wildcard TypeOf Unchecked TopFunction B F Result AnyValue
                                        KwArgsSeq KwArgsArray TCError Extends JSNumber JSBoolean SymbolicClosure
                                        CLJSInteger ArrayCLJS JSNominal JSString TCResult AssocType MergeType
                                        GetType HSequential HSet JSUndefined JSNull JSSymbol JSObject
                                        JSObj Bounds MatchType Instance Satisfies)
           (typed.cljc.checker.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter NoFilter)
           (typed.cljc.checker.object_rep NoObject EmptyObject Path)
           (typed.cljc.checker.path_rep KeyPE CountPE ClassPE KeysPE ValsPE NthPE KeywordPE SeqPE)
           (clojure.lang Cons IPersistentList Symbol IPersistentVector)))

(env-utils/invalidate-parsed-types!)

(defprotocol IUnparseType 
  (unparse-type* [t]))
(defprotocol IUnparseObject
  (unparse-object [o]))
(defprotocol IUnparsePathElem
  (unparse-path-elem [p]))
(defprotocol IUnparseFilter
  (unparse-filter* [fl]))

(defonce ^:dynamic *parse-type-in-ns* nil)
(set-validator! #'*parse-type-in-ns* (some-fn nil? symbol? plat-con/namespace?))

(declare unparse-type unparse-filter unparse-filter-set unparse-TCResult)

; Types print by unparsing them
(do (defmethod print-method typed.cljc.checker.impl_protocols.TCType [s writer]
      (print-method (unparse-type s) writer))
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCType clojure.lang.IRecord)
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCType java.util.Map)
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCType clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.impl_protocols.TCAnyType [s writer]
      (print-method (unparse-type s) writer))
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCAnyType clojure.lang.IRecord)
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCAnyType java.util.Map)
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCAnyType clojure.lang.IPersistentMap)

    (defmethod print-method TCResult [s writer]
      (print-method (unparse-TCResult s) writer))
    (prefer-method print-method TCResult java.util.Map)
    (prefer-method print-method TCResult clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.impl_protocols.IFilter [s writer]
      (cond 
        (f/FilterSet? s) (print-method (unparse-filter-set s) writer)
        :else (print-method (unparse-filter s) writer)))
    (prefer-method print-method typed.cljc.checker.impl_protocols.IFilter clojure.lang.IRecord)
    (prefer-method print-method typed.cljc.checker.impl_protocols.IFilter java.util.Map)
    (prefer-method print-method typed.cljc.checker.impl_protocols.IFilter clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.impl_protocols.IRObject [s writer]
      (print-method (unparse-object s) writer))
    (prefer-method print-method typed.cljc.checker.impl_protocols.IRObject clojure.lang.IRecord)
    (prefer-method print-method typed.cljc.checker.impl_protocols.IRObject java.util.Map)
    (prefer-method print-method typed.cljc.checker.impl_protocols.IRObject clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.path_rep.IPathElem [s writer]
      (print-method (unparse-path-elem s) writer))
    (prefer-method print-method typed.cljc.checker.path_rep.IPathElem clojure.lang.IRecord)
    (prefer-method print-method typed.cljc.checker.path_rep.IPathElem java.util.Map)
    (prefer-method print-method typed.cljc.checker.path_rep.IPathElem clojure.lang.IPersistentMap)
    )

;; vim-fireplace calls cider.nrepl.pprint/fipp-pprint.
;; because of frustrating issues with nrepl in typed.dev.repl/-main, I haven't figured out how to disable
;; the specific cider middleware that triggers it. this hack prints types in a human readable format.
;; it's necessary to convert to edn before calling to circumvent fipp's defaults for printing maps.
;; this makes extending fipp.edn/IEdn useless for types.
(defn- massage-before-fipp-pprint [x]
  (let [contains-special-print? (volatile! nil)
        special-pprint (fn [t]
                         (cond
                           (or (instance? typed.cljc.checker.impl_protocols.TCType t)
                               (instance? typed.cljc.checker.impl_protocols.TCAnyType t))
                           [(unparse-type t)]
                           (instance? TCResult t) [(unparse-TCResult t)]
                           (instance? typed.cljc.checker.impl_protocols.IFilter t) [((if (f/FilterSet? t)
                                                                                       unparse-filter-set
                                                                                       unparse-filter)
                                                                                     t)]
                           (instance? typed.cljc.checker.impl_protocols.IRObject t) [(unparse-object t)]
                           (instance? typed.cljc.checker.path_rep.IPathElem t) [(unparse-path-elem t)]))
        pprint-types (fn pprint-types [x]
                       (if-some [[printed] (special-pprint x)]
                         (do (vreset! contains-special-print? true)
                             printed)
                         ((requiring-resolve 'clojure.walk/walk) pprint-types identity x)))
        res (pprint-types x)]
    (if @contains-special-print? res x)))

(when (= "true" (System/getProperty "typed.clj.checker.parse-unparse.fipp-override"))
  (when (try (require 'fipp.ednize 'fipp.edn 'cider.nrepl.pprint)
             true
             (catch java.io.FileNotFoundException _))
    (alter-var-root (resolve 'cider.nrepl.pprint/fipp-pprint)
                    (fn [fipp-pretty]
                      (fn
                        ([x writer] (-> x ((resolve `massage-before-fipp-pprint)) (fipp-pretty writer)))
                        ([x writer options] (-> x ((resolve `massage-before-fipp-pprint)) (fipp-pretty writer options)))))))
  (System/setProperty "typed.clj.checker.parse-unparse.fipp-override" "false"))

(defmacro with-parse-ns [sym & body]
  `(binding [*parse-type-in-ns* ~sym]
     ~@body))

(defn with-parse-ns* [sym f]
  {:pre [(symbol? sym)]}
  (binding [*parse-type-in-ns* sym]
    (f)))

(declare parse-type* ^:dynamic resolve-type-clj->sym ^:dynamic resolve-type-clj resolve-type-cljs)

(defn tsyn->env [s]
  (let [m (meta s)]
    (when ((every-pred :line :column) m)
      m)))

(defn with-tsyn-env* [tsyn f]
  (let [menv (tsyn->env tsyn)]
    (with-bindings (cond-> {}
                     menv (assoc #'vs/*current-env* menv))
      (f))))

(defmacro with-tsyn-env [tsyn & body]
  `(with-tsyn-env* ~tsyn #(let [res# (do ~@body)] res#)))

(defn prs-error
  ([msg] (prs-error msg nil))
  ([msg opt]
   (let [[_ tsyn :as tsyn?] (find opt ::tsyn)
         f #(err/int-error msg (into {:use-current-env true} opt))]
     (if tsyn?
       (with-tsyn-env tsyn
         (f))
       (f)))))

(defn parse-type [s]
  (let [env (or (tsyn->env s) vs/*current-env*)]
    (binding [vs/*no-simpl* true
              vs/*current-env* env]
      (try (-> (parse-type* s)
               (vary-meta update :origin (fnil conj []) {:type :ann
                                                         :syntax s
                                                         :env env}))
           (catch Throwable e
             ;(prn (err/any-tc-error? (ex-data e)))
             (if (err/any-tc-error? (ex-data e))
               (throw e)
               (err/int-error (format "parse-type error while parsing %s! Please report to help improve this error message."
                                      (pr-str s))
                              {:visible-cause e})))))))

(defn delay-parse-type [s]
  ((resolve `parse-type) s))

(defn parse-clj [s]
  (impl/with-clojure-impl
    (parse-type s)))

(defn parse-cljs [s]
  (impl/with-cljs-impl
    (parse-type s)))

(defmulti parse-type* class)
(defmulti parse-type-list 
  (fn [[n]]
    {:post [((some-fn nil? symbol?) %)]}
    (when (symbol? n)
      (or (impl/impl-case
            :clojure (resolve-type-clj->sym n)
            :cljs (or ('#{quote Array Array2} n)
                      (resolve-type-cljs n)))
          n))))

(defmulti parse-type-symbol
  (fn [n] 
    {:pre [(symbol? n)]}
    (or (impl/impl-case
          :clojure (resolve-type-clj->sym n)
          ;;FIXME logic is all tangled
          :cljs (resolve-type-cljs n))
        n)))

(def parsed-free-map? (con/hmap-c? :fname symbol?
                                   :bnd r/Kind?
                                   :variance r/variance?))

; parsing TFn, protocol, RClass binders
(defn ^:private parse-free-with-variance [f]
  {:post [(parsed-free-map? %)]}
  (if (symbol? f)
    {:fname f
     :bnd r/no-bounds
     :variance :invariant}
    (let [[n & {:keys [< > variance] :as opts}] f]
      (when (contains? opts :kind)
        (err/deprecated-warn "Kind annotation for TFn parameters"))
      (when-not (r/variance? variance)
        (prs-error (str "Invalid variance " (pr-str variance) " in free binder: " f)))
      {:fname n 
       :bnd (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))
       :variance variance})))

(defn parse-free-binder-with-variance [binder]
  {:post [(every? parsed-free-map? %)]}
  (reduce (fn [fs fsyn]
            {:pre [(every? parsed-free-map? fs)]
             :post [(every? parsed-free-map? %)]}
            ;(prn "parse-free-binder-with-variance" (map :fname fs))
            (conj fs
                  (free-ops/with-bounded-frees 
                    (zipmap (map (comp r/make-F :fname) fs)
                            (map :bnd fs))
                    (parse-free-with-variance fsyn))))
          [] binder))

; parsing All binders
;return a vector of [name bnds]
(defn parse-free [f default-kind]
  {:post [((con/hvector-c? symbol? r/Kind?) %)]}
  (let [validate-sym (fn [s]
                       (when-not (symbol? s)
                         (prs-error (str "Type variable must be a symbol, given: " (pr-str s))))
                       (when (namespace s)
                         (prs-error (str "Type variable must not be namespace qualified: " (pr-str s))))
                       (when (.contains (name s) ".")
                         (prs-error (str "Type variable must not contain dots (.): " (pr-str s))))
                       (when (#{"true" "nil" "false"} (name s))
                         (prs-error (str "Type variable must not be named true, false, or nil: " (pr-str s)))))]
    (if (symbol? f)
      (do (validate-sym f)
          [f (case default-kind
               :type r/no-bounds
               :dotted r/dotted-no-bounds)])
      (let [[n & opts] f
            _ (when-not (even? (count opts))
                (prs-error (str "Uneven number of options in type variable binder: " (pr-str opts))))
            {:keys [< > kind] :as opts} opts
            ks (set (keys kind))
            kind? (:kind ks)]
        (when kind?
          (when ((some-fn ks) :< :>)
            (prs-error "Cannot combine :kind with :< or :> in type variable binder")))
        (validate-sym n)
        (when (:variance opts)
          (prs-error "Variance not supported for variables introduced with All"))
        [n (if kind?
             (parse-type (:kind ks))
             (let [upper-or-nil (when (contains? opts :<)
                                  (parse-type <))
                   lower-or-nil (when (contains? opts :>)
                                  (parse-type >))]
               (c/infer-bounds upper-or-nil lower-or-nil)))]))))

(defn check-forbidden-rec [rec tbody]
  (letfn [(well-formed? [t]
            (and (not= rec t)
                 (if ((some-fn r/Intersection? r/Union?) t)
                   (every? well-formed? (:types t))
                   true)))]
    (when-not (well-formed? tbody)
      (prs-error (str "Recursive type not allowed here")))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec & [[free-symbol :as bnder] type 
                              :as args]]]
  (when-not (= 1 (count bnder))
    (prs-error "Rec type requires exactly one entry in binder"))
  (when-not (= 2 (count args))
    (prs-error "Wrong arguments to Rec"))
  (let [Mu* @(Mu*-var)
        _ (when-not (= 1 (count bnder)) 
            (prs-error "Only one variable allowed: Rec"))
        f (r/make-F free-symbol)
        body (free-ops/with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

(defn parse-CountRange [[_ & [n u :as args]]]
  (when-not (#{1 2} (count args))
    (prs-error "Wrong arguments to CountRange"))
  (when-not (integer? n)
    (prs-error "First argument to CountRange must be an integer"))
  (when-not (or (#{1} (count args))
                (integer? u))
    (prs-error "Second argument to CountRange must be an integer"))
  (r/make-CountRange n u))

(defmethod parse-type-list 'typed.clojure/CountRange [t] (parse-CountRange t))

(defn uniquify-local [sym]
  (get-in vs/*current-expr* [:env ::uniquify/locals-frame-val sym]))

(defmethod parse-type-list 'typed.clojure/TypeOf [[_ sym :as t]]
  (impl/assert-clojure)
  (when-not (= 2 (count t))
    (prs-error (str "Wrong number of arguments to TypeOf (" (count t) ")")))
  (when-not (symbol? sym)
    (prs-error "Argument to TypeOf must be a symbol."))
  (let [uniquified-local (uniquify-local sym)
        vsym (let [r (resolve-type-clj sym)]
               (when (var? r)
                 (coerce/var->symbol r)))]
    (if uniquified-local
      (let [t (ind/type-of-nofail uniquified-local)]
        (when-not t
          (prs-error (str "Could not resolve TypeOf for local " sym)))
        t)
      (do
        (when-not vsym
          (prs-error (str "Could not resolve TypeOf for var " sym)))
        (r/-type-of vsym)))))

(defn parse-ExactCount [[_ & [n :as args]]]
  (when-not (= 1 (count args))
    (prs-error "Wrong arguments to ExactCount"))
  (when-not (integer? n)
    (prs-error "First argument to ExactCount must be an integer"))
  (r/make-ExactCountRange n))

(defmethod parse-type-list 'typed.clojure/ExactCount [t] (parse-ExactCount t))

(defn- RClass-of-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.type-ctors) 'RClass-of)]
    (assert (var? v) "RClass-of unbound")
    v))

(defn predicate-for [on-type]
  (let [RClass-of @(RClass-of-var)]
    (r/make-FnIntersection
      (r/make-Function [r/-any] (impl/impl-case
                                  :clojure (RClass-of Boolean)
                                  :cljs    (r/JSBoolean-maker))
                       :filter (fl/-FS (fl/-filter on-type 0)
                                       (fl/-not-filter on-type 0))))))

(defn parse-Pred [[_ & [t-syn :as args]]]
  (when-not (= 1 (count args))
    (prs-error "Wrong arguments to predicate"))
  (predicate-for (parse-type t-syn)))

(defmethod parse-type-list 'typed.clojure/Pred [t] (parse-Pred t))

(defn parse-Not [[_ tsyn :as all]]
  (when-not (= (count all) 2) 
    (prs-error (str "Wrong arguments to Not (expected 1): " all)))
  (r/NotType-maker (parse-type tsyn)))
(defmethod parse-type-list 'typed.clojure/Not [frm] (parse-Not frm))

(defn parse-Difference [[_ tsyn & dsyns :as all]]
  (when-not (<= 3 (count all))
    (prs-error (str "Wrong arguments to Difference (expected at least 2): " all)))
  (apply r/-difference (parse-type tsyn) (mapv parse-type dsyns)))

(defmethod parse-type-list 'typed.clojure/Difference [t] (parse-Difference t))

(defmethod parse-type-list 'typed.clojure/Rec [syn] (parse-rec-type syn))

(defn parse-Assoc [[_ tsyn & entries :as all]]
  (when-not (<= 1 (count (next all)))
    (prs-error (str "Wrong arguments to Assoc: " all)))
  (let [{ellipsis-pos :..}
        (zipmap entries (range))

        [entries dentries] (split-at (if ellipsis-pos
                                       (dec ellipsis-pos)
                                       (count entries))
                                     entries)

        [drest-type _ drest-bnd] (when ellipsis-pos
                                   dentries)

        _ (when-not (-> entries count even?)
            (prs-error (str "Incorrect Assoc syntax: " all " , must have even number of key/val pair.")))

        _ (when-not (or (not ellipsis-pos)
                        (= (count dentries) 3))
            (prs-error (str "Incorrect Assoc syntax: " all " , Dotted rest entry must be 3 entries")))

        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (prs-error "Dotted bound must be symbol"))]
  (r/AssocType-maker (parse-type tsyn)
                     (into []
                           (comp (map parse-type)
                                 (partition-all 2))
                           entries)
                     (when ellipsis-pos
                       (let [bnd (free-ops/free-in-scope-bnds drest-bnd)
                             f (free-ops/free-in-scope drest-bnd)
                             _ (when-not (r/Regex? bnd)
                                 (prs-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))
                             _ (assert (r/F? f))]
                         (r/DottedPretype1-maker
                           ;with dotted bound in scope as free
                           (free-ops/with-bounded-frees {(r/make-F drest-bnd)
                                                         ((requiring-resolve 'typed.cljc.checker.cs-gen/homogeneous-dbound->bound)
                                                          bnd)}
                             (parse-type drest-type))
                           (:name f)))))))

(defmethod parse-type-list 'typed.clojure/Assoc [t] (parse-Assoc t))

(defn parse-Merge [[_ & tsyns]]
  (r/MergeType-maker (mapv parse-type tsyns)))

(defmethod parse-type-list 'typed.clojure/Merge [t] (parse-Merge t))

(defn parse-Instance [[_ tsyn :as all]]
  (impl/assert-clojure)
  (when-not (= 2 (count all))
    (prs-error (str "Wrong arguments to t/Instance: " (pr-str all))))
  (when-not (simple-symbol? tsyn)
    (prs-error (str "t/Instance must be passed a simple symbol, given: " (pr-str tsyn))))
  ((requiring-resolve 'typed.cljc.checker.type-ctors/RClass-of-with-unknown-params)
   tsyn))

(defmethod parse-type-list 'typed.clojure/Instance [t] (parse-Instance t))

(defn parse-Satisfies [[_ tsyn :as all]]
  (impl/assert-clojure) ;; TODO resolve protocol in cljs
  (when-not (= 2 (count all))
    (prs-error (str "Wrong arguments to t/Satisfies: " (pr-str all))))
  (when-not (symbol? tsyn)
    (prs-error (str "t/Satisfies must be passed a symbol, given: " (pr-str tsyn))))
  (let [protocol-var (resolve-type-clj tsyn)
        _ (when-not (var? protocol-var)
            (prs-error (str "t/Satisfies must be passed a symbol naming a protocol: "
                            tsyn " resolves to " (pr-str protocol-var))))]
    ((requiring-resolve 'typed.cljc.checker.type-ctors/Protocol-with-unknown-params)
     (symbol protocol-var))))

(defmethod parse-type-list 'typed.clojure/Satisfies [t] (parse-Satisfies t))

(defn parse-Get [[_ tsyn keysyn not-foundsyn :as all]]
  (when-not (#{2 3} (count (next all)))
    (prs-error (str "Wrong arguments to Get: " all)))
  (r/-get (parse-type tsyn)
          (parse-type keysyn)
          :not-found
          (when (= 3 (count (next all)))
            (parse-type not-foundsyn))))

(defmethod parse-type-list 'typed.clojure/Get [t] (parse-Get t))

; convert flattened kw arguments to vectors
(defn normalise-binder [bnds]
  (loop [bnds bnds
         out []]
    (cond
      (empty? bnds) out

      (vector? (first bnds)) (let [[s & rst] bnds]
                               (recur rst
                                      (conj out s)))
      :else
      (let [[sym & rst] bnds
            [group rst] (loop [bnds rst
                               out [sym]]
                          (if (keyword? (second bnds))
                            (let [_ (when-not (= 2 (count (take 2 bnds)))
                                      (prs-error (str "Keyword option " (second bnds)
                                                      " has no associated value")))
                                  [k v & rst] bnds]
                              (recur rst
                                     (conj out k v)))
                            [bnds out]))]
        (recur rst
               (conj out group))))))

(defn parse-normal-binder [bnds]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (con/hvector-c? symbol? r/Kind?) %)]}
                  (conj fs
                        (free-ops/with-bounded-frees (into {}
                                                           (map (fn [[n bnd]] [(r/make-F n) bnd]))
                                                           fs)
                          (parse-free fsyn :type))))
                [] bnds)]
    [frees-with-bnds nil]))

(defn parse-dotted-binder [bnds]
  {:pre [(vector? bnds)]}
  (let [[frees-with-bnds] (parse-normal-binder (-> bnds pop pop))
        dvar (parse-free (-> bnds pop peek) :dotted)]
    [frees-with-bnds dvar]))

(defn parse-unknown-binder [bnds]
  {:pre [((some-fn nil? vector?) bnds)]}
  (when bnds
    ((if (#{:... :.. '...} (peek bnds))
       parse-dotted-binder
       parse-normal-binder)
     bnds)))

(defn parse-All-binder [bnds]
  {:pre [(vector? bnds)]}
  (let [[positional kwargs] (split-with (complement keyword?) bnds)
        ;; allow trailing :.. in positional vars before kw args
        [positional kwargs] (if (#{:... :..} (first kwargs))
                              [(conj (vec positional) (first kwargs))
                               (next kwargs)]
                              [(vec positional) kwargs])
        dotted? (boolean (#{:... :.. '...} (peek positional)))
        _ (when-not (even? (count kwargs))
            (prs-error (str "Expected an even number of keyword options to All, given: " (vec kwargs))))
        _ (when (seq kwargs)
            (when-not (apply distinct? (map first (partition 2 kwargs)))
              (prs-error (str "Gave repeated keyword args to All: " (vec kwargs)))))
        {:keys [named] :as kwargs} kwargs
        _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
            (when (seq unsupported)
              (prs-error (str "Unsupported keyword argument(s) to All: " unsupported))))
        _ (when (contains? kwargs :named)
            (when-not (and (vector? named)
                           (every? symbol? named))
              (prs-error (str ":named keyword argument to All must be a vector of symbols, given: " (pr-str named)))))
        bnds* (if named
                (let [positional-no-dotted (cond-> positional
                                             dotted? (-> pop pop))
                      ;; fit :named variables between positional and dotted variable, because 
                      ;; PolyDots expects the dotted variable last.
                      bnds* (-> positional-no-dotted
                                (into named)
                                (into (when dotted?
                                        (subvec positional (- (count positional) 2)))))]
                  bnds*)
                positional)
        no-dots (cond-> bnds*
                  dotted? pop)
        _ (when (seq no-dots)
            (when-not (apply distinct? no-dots)
              (prs-error (str "Variables bound by All must be unique, given: " no-dots))))
        named-map (let [sym-to-pos (into {}
                                         (map-indexed #(vector %2 %1))
                                         no-dots)]
                    (select-keys sym-to-pos named))
        ;; TODO 
        ;; update usages of parse-unknown-binder to use parse-All-binder
        [frees-with-bnds dvar] ((if dotted?
                                  parse-dotted-binder
                                  parse-normal-binder)
                                bnds*)]
    {:frees-with-bnds frees-with-bnds
     :dvar dvar
     :named named-map}))

(defn parse-all-type*
  [{:keys [frees-with-bnds dvar named] :as _parsed-binder}
   type]
  ;(prn "parse-all-type*" type)
  (let [bfs (into {}
                  (map (fn [[n bnd]] [(r/make-F n) bnd]))
                  (cond-> frees-with-bnds
                    dvar (conj dvar)))
        body (free-ops/with-bounded-frees bfs
               (parse-type type))]
    (if dvar
      (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                   (map second (concat frees-with-bnds [dvar]))
                   body
                   :named named)
      (c/Poly* (map first frees-with-bnds)
               (map second frees-with-bnds)
               body
               :named named))))

;dispatch on last element of syntax in binder
(defn parse-all-type [bnds type]
  (when-not (vector? bnds)
    (prs-error (str "First argument to t/All must be a vector: " (pr-str bnds))))
  (parse-all-type* (parse-All-binder bnds) type))

(defmethod parse-type-list 'Extends
  [[_ extends & {:keys [without] :as opts} :as syn]]
  (when-not (empty? (set/difference (set (keys opts)) #{:without}))
    (prs-error (str "Invalid options to Extends:" (keys opts))))
  (when-not (vector? extends) 
    (prs-error (str "Extends takes a vector of types: " (pr-str syn))))
  (c/-extends (doall (map parse-type extends))
              :without (doall (map parse-type without))))

(defn parse-All [[_All_ bnds syn & more :as all]]
  ;(prn "All syntax" all)
  (when more
    (prs-error (str "Bad All syntax: " all)))
  (parse-all-type bnds syn))

(defmethod parse-type-list 'typed.clojure/All [t] (parse-All t))

(defn parse-Match [[_ target & patterns :as all]]
  (when-not (< 1 (count all))
    (prs-error "Missing target type in t/Match"))
  (let [patterns (loop [patterns patterns
                        out []]
                   (if (empty? patterns)
                     out
                     (let [[pattern-before-=> =>+rhs+patterns] (split-with (complement #{:->}) patterns)
                           _ (when (empty? pattern-before-=>)
                               (prs-error "Missing pattern before :-> in t/Match clause"))
                           _ (when (empty? =>+rhs+patterns)
                               (prs-error "Missing :-> in t/Match clause"))
                           rhs+patterns (next =>+rhs+patterns)
                           _ (when-not rhs+patterns
                               (prs-error "Missing type after :-> in t/Match"))
                           rhs (first rhs+patterns)
                           patterns (next rhs+patterns)
                           t (case (count pattern-before-=>)
                               1 (let [[pattern] pattern-before-=>]
                                   (parse-type [pattern :-> rhs]))
                               2 (let [[binder pattern] pattern-before-=>]
                                   (when-not (vector? binder)
                                     (prs-error "Binder before :=> must be a vector"))
                                   (parse-all-type* (parse-All-binder binder) [pattern :-> rhs]))
                               (prs-error "Bad number of arguments before :=> in t/Match"))]
                       (recur patterns (conj out t)))))]
    (r/MatchType-maker (parse-type target) patterns)))

(defmethod parse-type-list 'typed.clojure/Match [t] (parse-Match t))

(defn parse-union-type [[u & types]]
  (c/make-Union (doall (map parse-type types))))

(defmethod parse-type-list 'typed.clojure/U [syn] (parse-union-type syn))

; don't do any simplification of the intersection because some types might
; not be resolved
(defn parse-intersection-type [[i & types]]
  (c/make-Intersection (map parse-type types)))

(defmethod parse-type-list 'typed.clojure/I [syn] (parse-intersection-type syn))

(defn parse-Array 
  [[_ syn & none]]
  (when-not (empty? none)
    (prs-error "Expected 1 argument to Array"))
  (let [t (parse-type syn)]
    (impl/impl-case
      :clojure (let [jtype (if (r/RClass? t)
                             (r/RClass->Class t)
                             Object)]
                 (r/PrimitiveArray-maker jtype t t))
      :cljs (r/ArrayCLJS-maker t t))))

(defmethod parse-type-list 'Array [syn] (parse-Array syn))
(defmethod parse-type-list 'cljs.core.typed/Array [syn] (parse-Array syn))

(defn parse-ReadOnlyArray
  [[_ osyn & none]]
  (when-not (empty? none) 
    (prs-error "Expected 1 argument to ReadOnlyArray"))
  (let [o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object (r/Bottom) o)
      :cljs (r/ArrayCLJS-maker (r/Bottom) o))))

(defmethod parse-type-list 'ReadOnlyArray [syn] (parse-ReadOnlyArray syn))
(defmethod parse-type-list 'cljs.core.typed/ReadOnlyArray [syn] (parse-ReadOnlyArray syn))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none]]
  (when-not (empty? none) 
    (prs-error "Expected 2 arguments to Array2"))
  (let [i (parse-type isyn)
        o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object i o)
      :cljs (r/ArrayCLJS-maker i o))))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (impl/assert-clojure)
  (when-not (empty? none) 
    (prs-error "Expected 3 arguments to Array3"))
  (let [jrclass (c/fully-resolve-type (parse-type jsyn))
        _ (when-not (r/RClass? jrclass) 
            (prs-error "First argument to Array3 must be a Class"))]
    (r/PrimitiveArray-maker (r/RClass->Class jrclass) (parse-type isyn) (parse-type osyn))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply r/make-FnIntersection (mapcat parse-function types)))

(defn parse-Fn [[_ & types :as syn]]
  (when-not (seq types) 
    (prs-error (str "Must pass at least one arity to Fn: " (pr-str syn))))
  (when-not (every? vector? types) 
    (prs-error (str "Fn accepts vectors, given: " (pr-str syn))))
  (parse-fn-intersection-type syn))

(defmethod parse-type-list 'Fn [t] 
  (err/deprecated-plain-op 'Fn 'IFn)
  (parse-Fn t))
(defmethod parse-type-list 'typed.clojure/IFn [t] (parse-Fn t))

(defn parse-free-binder [[nme & {:keys [variance < > kind] :as opts}]]
  (when kind
    (prs-error ":kind not yet implemented"))
  (when-not (symbol? nme)
    (prs-error "First entry in free binder should be a name symbol"))
  {:nme nme :variance (or variance :invariant)
   :bound (r/-bounds
            ;upper
            (if (contains? opts :<)
              (parse-type <)
              r/-any)
            ;lower
            (if (contains? opts :>)
              (parse-type >)
              r/-nothing))})

(defn parse-tfn-binder [binder]
  {:pre [((some-fn symbol? vector?) binder)]
   :post [((con/hmap-c? :nme symbol? :variance (some-fn r/variance? #(= :infer %))
                        :bound r/Kind?)
           %)]}
  (let [[nme & opts-flat :as all] (cond-> binder
                                    (symbol? binder) vector)
        _ (when-not (symbol? nme)
            (prs-error "Must provide a name symbol to TFn"))
        _ (when-not (even? (count opts-flat))
            (prs-error (str "Uneven binder element passed to TFn binder: "
                            (pr-str binder))))
        {:keys [variance < >]
         :or {variance :infer}
         :as opts}
        (apply hash-map opts-flat)]
    (when-some [extra-keys (not-empty (disj (-> opts keys set) :variance :< :>))]
      (prs-error (str "Unknown t/TFn option: " (pr-str (first extra-keys))
                      ". Known options are :variance, :<, and :>.")))
    (when (contains? opts :kind)
      (err/deprecated-warn "Kind annotation for TFn parameters"))
    (when-some [[_ provided] (find opts :variance)]
      (when-not (r/variance? provided)
        (prs-error (str "Invalid variance: " (pr-str provided)))))
    {:nme nme :variance variance
     :bound (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))}))

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (when-not (= 3 (count tfn))
    (prs-error (str "Wrong number of arguments to TFn: " (pr-str tfn))))
  (let [;; variable bounds has all variables to the left of it in scope
        {:keys [free-maps]} (reduce (fn [{:keys [free-maps free-symbs]} binder]
                                      (when-not ((some-fn symbol? vector?) binder)
                                        (prs-error (str "TFn binder element must be a symbol or vector: " (pr-str binder))))
                                      (free-ops/with-free-symbols free-symbs
                                        {:free-maps (conj free-maps (parse-tfn-binder binder))
                                         :free-symbs (conj free-symbs (cond-> binder
                                                                        (vector? binder) first))}))
                                    {:free-maps []
                                     :free-symbs #{}}
                                    binder)
        bodyt (free-ops/with-bounded-frees (into {}
                                                 (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound]))
                                                 free-maps)
                (parse-type bodysyn))
        ;; at some point we should remove this requirement (this is checked by parse-tfn-binder)
        id (gensym)
        infer-variances? (some #(= :infer (:variance %)) free-maps)
        variances (let [prs-error (bound-fn* prs-error)]
                    (fn []
                      (let [currently-inferring-TypeFns vs/*currently-inferring-TypeFns*]
                        (if (currently-inferring-TypeFns id)
                          (if infer-variances?
                            (when (currently-inferring-TypeFns id)
                              (prs-error "Cannot infer variances on recursive t/TFn, please add :variance annotations"))
                            {:cache false
                             :variances (map :variance free-maps)})
                          (binding [vs/*currently-inferring-TypeFns* (conj currently-inferring-TypeFns id)]
                            (let [vs (free-ops/with-bounded-frees (into {} (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound]))
                                                                        free-maps)
                                       ((requiring-resolve 'typed.cljc.checker.frees/fv-variances) bodyt))]
                              {:cache true
                               :variances (mapv (fn [{:keys [nme variance]}]
                                                  (if-some [actual-v (vs nme)]
                                                    (if (and variance
                                                             (not= :infer variance)
                                                             (not= actual-v variance))
                                                      (prs-error (str "Type variable " nme " occurs with " (name actual-v) " variance "
                                                                      "when declared " (name variance)))
                                                      actual-v)
                                                    ;;not great support for :constant, use :invariant for now
                                                    ;:constant
                                                    :invariant))
                                                free-maps)}))))))]
    (c/TypeFn* (map :nme free-maps)
               variances
               (map :bound free-maps)
               bodyt
               {:meta {:env vs/*current-env*}})))

(defmethod parse-type-list 'typed.clojure/TFn [syn] (parse-type-fn syn))

(defn parse-Type-kind [syn]
  (cond
    (symbol? syn) r/no-bounds
    (seq? syn) (if-some [opts (next syn)]
                 (let [_ (when (odd? (count opts))
                           (prs-error "Uneven arguments passed to t/Type"))
                       popts (partition 2 opts)
                       _ (when-not (apply distinct? (map first popts))
                           (prs-error "Duplicate keys passed to t/Type"))
                       opts (apply hash-map opts)
                       _ (when (not-every? keyword? (keys opts))
                           (prs-error "Non-keyword key passed to t/Type"))
                       _ (when-some [extra-keys (disj (set (keys opts)) :< :>)]
                           (prs-error (str "Unknown keys passed to t/Type: "
                                           (str/join ", " (sort extra-keys)))))]
                   (r/-bounds (if-some [[_ upper] (find opts :<)]
                                (parse-type upper)
                                r/-any)
                              (if-some [[_ lower] (find opts :>)]
                                (parse-type lower)
                                r/-nothing)))
                 r/no-bounds)
    :else (err/int-error (str "unparseable: " syn))))

(defmethod parse-type-list 'typed.clojure/Type [syn] (parse-Type-kind syn))
(defmethod parse-type-symbol 'typed.clojure/Type [syn] (parse-Type-kind syn))

;; parse-HVec, parse-HSequential and parse-HSeq have many common patterns
;; so we reuse them
(defn parse-types-with-rest-drest [err-msg]
  (fn [syns]
    (let [_ (when-not (seqable? syns)
              (prs-error (str err-msg " " (pr-str syns))))
          syns (vec syns)
          rest? (#{:* '* :+} (peek syns))
          dotted? (and (#{:... :.. '...} (some-> (not-empty syns) pop peek))
                       (<= 3 (count syns)))
          _ (when (and rest? dotted?)
              (prs-error (str err-msg syns)))
          {:keys [fixed rest drest]}
          (cond
            rest?
            (let [fixed (mapv parse-type (-> syns pop pop))
                  rest (parse-type (-> syns pop peek))]
              {:fixed (cond-> fixed
                        (= :+ rest?) (conj rest))
               :rest rest})
            dotted?
            (let [fixed (mapv parse-type (-> syns pop pop pop))
                  [drest-type _dots_ drest-bnd :as dot-syntax] (take-last 3 syns)
                  ; should never fail, if the logic changes above it's probably
                  ; useful to keep around.
                  _ (when-not (= 3 (count dot-syntax))
                      (prs-error (str "Bad vector syntax: " dot-syntax)))
                  bnd (free-ops/free-in-scope-bnds drest-bnd)
                  f (free-ops/free-in-scope drest-bnd)
                  _ (when-not (r/Regex? bnd)
                      (prs-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
              {:fixed fixed
               :drest (r/DottedPretype1-maker
                        ;with dotted bound in scope as free
                        (free-ops/with-bounded-frees {(r/make-F drest-bnd)
                                                      ((requiring-resolve 'typed.cljc.checker.cs-gen/homogeneous-dbound->bound)
                                                       bnd)}
                          (parse-type drest-type))
                        (:name f))})
            :else {:fixed (mapv parse-type syns)})]
      {:fixed fixed
       :rest rest
       :drest drest})))

(def parse-hvec-types (parse-types-with-rest-drest
                        "Invalid heterogeneous vector syntax:"))
(def parse-hsequential-types (parse-types-with-rest-drest
                               "Invalid heterogeneous sequential syntax:"))
(def parse-hseq-types (parse-types-with-rest-drest
                        "Invalid heterogeneous seq syntax:"))
(def parse-hlist-types (parse-types-with-rest-drest
                         "Invalid heterogeneous list syntax:"))

(declare parse-object parse-filter-set)

(defn parse-heterogeneous* [parse-h*-types constructor]
  (fn [[_ syn & {:keys [filter-sets objects repeat] :or {repeat false}}]]
    (when-not (boolean? repeat)
      (prs-error (str ":repeat must be boolean")))
    (let [{:keys [fixed drest rest]} (parse-h*-types syn)]
      (constructor fixed
                   :filters (some->> filter-sets (mapv parse-filter-set))
                   :objects (some->> objects (mapv parse-object))
                   :drest drest
                   :rest rest
                   :repeat (true? repeat)))))

(def parse-HVec (parse-heterogeneous* parse-hvec-types r/-hvec))
(def parse-HSequential (parse-heterogeneous* parse-hsequential-types r/-hsequential))
(def parse-HSeq (parse-heterogeneous* parse-hseq-types r/-hseq))
(def parse-HList (parse-heterogeneous* parse-hseq-types (comp #(assoc % :kind :list)
                                                              r/-hsequential)))

(defmethod parse-type-list 'typed.clojure/HVec [t] (parse-HVec t))
(defmethod parse-type-list 'typed.clojure/HSequential [t] (parse-HSequential t))
(defmethod parse-type-list 'typed.clojure/HSeq [t] (parse-HSeq t))
(defmethod parse-type-list 'typed.clojure/HList [t]
  ;(prn `parse-hlist t)
  (parse-HList t))

(defn parse-HSet [[_ ts & {:keys [complete?] :or {complete? true}} :as args]]
  (let [bad (seq (remove hset/valid-fixed? ts))]
    (when-not (boolean? complete?)
      (prs-error (str "HSet's :complete? must be boolean")))
    (when bad
      (prs-error (str "Bad arguments to HSet: " (pr-str bad))))
    (r/-hset (into #{} (map r/-val) ts)
             :complete? complete?)))

(defmethod parse-type-list 'typed.clojure/HSet [t] (parse-HSet t))

(defn- syn-to-hmap [mandatory optional absent-keys complete?]
  (when mandatory
    (when-not (map? mandatory)
      (prs-error (str "Mandatory entries to HMap must be a map: " mandatory))))
  (when optional
    (when-not (map? optional)
      (prs-error (str "Optional entries to HMap must be a map: " optional))))
  (letfn [(mapt [m]
            (reduce-kv (fn [m k v]
                         (assoc m (r/-val k) (parse-type v)))
                       {} m))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (prs-error (str "HMap options contain duplicate key entries: "
                              "Mandatory: " (into {} mandatory) ", Optional: " (into {} optional) 
                              ", Absent: " (set absent-keys))))
          _ (when-not (every? keyword? (keys mandatory)) (prs-error "HMap's mandatory keys must be keywords"))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (prs-error "HMap's optional keys must be keywords"))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (prs-error "HMap's absent keys must be keywords"))
          absent-keys (into #{} (map r/-val) absent-keys)]
      (c/make-HMap :mandatory mandatory :optional optional 
                   :complete? complete? :absent-keys absent-keys))))

(defn parse-quoted-hvec [syn]
  (let [{:keys [fixed drest rest]} (parse-hvec-types syn)]
    (r/-hvec fixed
             :drest drest
             :rest rest)))

(defn quoted? [v]
  (and (seq? v)
       (= 2 (count v))
       (= 'quote (first v))))

(defmethod parse-type-list 'quote 
  [[_ syn :as all]]
  (when-not (quoted? all)
    (prs-error (str "Incorrect use of quote: " (pr-str all))))
  (cond
    (-> all meta :fake-quote) (parse-type syn)
    ((some-fn number? keyword? symbol? string?) syn) (r/-val syn)
    (vector? syn) (parse-quoted-hvec syn)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false)
    :else (prs-error (str "Invalid use of quote: " (pr-str syn)))))

(defn allow-regex [t]
  (cond-> t
    (instance? clojure.lang.IObj t)
    (vary-meta assoc ::allow-regex true)))

(defn regex-allowed? [t]
  (-> t meta ::allow-regex boolean))

(declare parse-in-ns)

(defn multi-frequencies 
  "Like frequencies, but only returns frequencies greater
  than one"
  [coll]
  (into {}
        (filter (fn [[_ freq]] (< 1 freq)))
        (frequencies coll)))

(defn parse-HMap [[_HMap_ & flat-opts :as all]]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (err/deprecated-warn
                                 "(HMap {}) syntax has changed, use (HMap :mandatory {})")
                               (first flat-opts))
        flat-opts (cond-> flat-opts
                    deprecated-mandatory next)
        _ (when-not (even? (count flat-opts))
            (prs-error (str "Uneven keyword arguments to HMap: " (pr-str all))))
        flat-keys (sequence
                    (comp (partition-all 2)
                          (map first))
                    flat-opts)
        _ (when-not (every? keyword? flat-keys)
            (prs-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                            #_#_" in: " (pr-str all))))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (prs-error (str "Repeated keyword argument to HMap: " (pr-str k)))))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (prs-error (str "Unsupported HMap keyword argument: " (pr-str k))))
        _ (when (and deprecated-mandatory mandatory)
            (prs-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete?)))

(defmethod parse-type-list 'HMap [t] (parse-HMap t))
(defmethod parse-type-list 'typed.clojure/HMap [t] (parse-HMap t))

(defn parse-JSObj [[_JSObj_ types :as all]]
  (let [_ (when-not (= 2 (count all))
            (prs-error (str "Bad syntax to JSObj: " (pr-str all))))
        _ (when-not (every? keyword? (keys types))
            (prs-error (str "JSObj requires keyword keys, given " (pr-str (class (first (remove keyword? (keys types))))))))
        parsed-types (update-vals types parse-type)]
    (r/JSObj-maker parsed-types)))

(defmethod parse-type-list 'typed.clojure/JSObj [t] (parse-JSObj t))

(def ^:private cljs-ns #((requiring-resolve 'typed.cljs.checker.util/cljs-ns)))

(defn parse-in-ns []
  {:post [(symbol? %)]}
  (or *parse-type-in-ns*
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (cljs-ns))))

(def ns-rewrites-clj {'clojure.core.typed 'typed.clojure})
(def ^:private ns-unrewrites-clj (set/map-invert ns-rewrites-clj))

(defn ^:dynamic resolve-type-clj
  "Returns a var, class or nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn var? class? nil?) %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-some [ns (find-ns nsym)]
      (or (when-some [res (ns-resolve ns sym)]
            (or (when-some [rewrite-nsym (when (var? res)
                                           (ns-rewrites-clj (some-> res symbol namespace symbol)))]
                  (find-var (symbol (name rewrite-nsym) (-> res symbol name))))
                res))
          (when-some [alias-sym (some-> ((ns-aliases ns)
                                         (some-> (namespace sym)
                                                 symbol))
                                        ns-name)]
            (find-var (symbol (name (ns-rewrites-clj alias-sym alias-sym))
                              (name sym)))))
      (err/int-error (str "Cannot find namespace: " sym)))))

(defn- resolve-type-alias-clj
  "Returns a symbol if sym maps to a type alias, otherwise nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol? nil?) %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)
        nsp (some-> (namespace sym) symbol)]
    (if-let [ns (find-ns nsym)]
      (when-let [qual (if nsp
                        (some-> (or ((ns-aliases ns) nsp)
                                    (find-ns nsp))
                                ns-name)
                        (ns-name ns))]
        (let [_ (assert (and (symbol? qual)
                             (not (namespace qual))))
              qual (ns-rewrites-clj qual qual)
              qsym (symbol (name qual) (name sym))]
          (when (contains? (nme-env/name-env) qsym)
            qsym)))
      (err/int-error (str "Cannot find namespace: " sym)))))

(defn ^:dynamic resolve-type-clj->sym
  [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (impl/assert-clojure)
  (let [nsym (parse-in-ns)]
    (if-some [ns (find-ns nsym)]
      (or (when (special-symbol? sym)
            sym)
          ('#{Array ReadOnlyArray Array2 Array3 Assoc HMap Extends} sym)
          (when-some [res (ns-resolve ns sym)]
            (or (when (var? res)
                  (if-some [rewrite-nsym (ns-rewrites-clj (some-> res symbol namespace symbol))]
                    (symbol (name rewrite-nsym) (-> res symbol name))
                    (coerce/var->symbol res)))
                (when (class? res)
                  (coerce/Class->symbol res))))
          (when-some [alias-sym (or (some-> ((ns-aliases ns)
                                             (some-> (namespace sym)
                                                     symbol))
                                            ns-name)
                                    (some-> sym namespace symbol))]
            (symbol (name (ns-rewrites-clj alias-sym alias-sym))
                    (name sym)))
          (let [sym-nsym (or (some-> sym namespace symbol)
                             nsym)]
            (symbol (name (ns-rewrites-clj sym-nsym sym-nsym)) (name sym))))
      (err/int-error (str "Cannot find namespace: " sym)))))

(def ^:private ns-rewrites-cljs {'cljs.core.typed 'typed.clojure})
(def ^:private ns-unrewrites-cljs (set/map-invert ns-rewrites-cljs))

;; ignores both clj and cljs namespace graph (other than to resolve aliases)
;; TODO reconcile clj/cljs type resolution. neither should really be interning vars (breaking change for clj).
(defn resolve-type-cljs
  "Returns a qualified symbol or nil"
  [sym]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol?
                    nil?)
           %)]}
  (impl/assert-cljs)
  (let [nsym (parse-in-ns)
        ;; TODO does this handle imports?
        res (or ((requiring-resolve 'typed.cljs.checker.util/resolve-var) nsym sym)
                (when-some [maybe-alias (some-> sym namespace symbol)]
                  (symbol 
                    (name
                      (let [alias-sym (get ((requiring-resolve 'typed.cljs.checker.util/get-aliases)
                                            nsym)
                                           maybe-alias
                                           maybe-alias)]
                        (ns-rewrites-cljs alias-sym alias-sym)))
                    (name sym)))
                (symbol (name (ns-rewrites-cljs nsym nsym))
                        (name sym)))]
    res))

(defn parse-Value [[_Value_ syn :as all]]
  (when-not (= 2 (count all))
    (prs-error (str "Incorrect number of arguments to Value, " (count all)
                    ", expected 2: " all)))
  (impl/impl-case
    :clojure (const/constant-type syn)
    :cljs (cond
            ((some-fn symbol? keyword? nil?) syn)
              (r/-val syn)
            :else (assert nil (str "FIXME CLJS parse Value: " (pr-str syn))))))

(defmethod parse-type-list 'typed.clojure/Val [t] (parse-Value t))
(defmethod parse-type-list 'typed.clojure/Value [t] (parse-Value t))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (when-not (= #{}
               (set/intersection (set (keys optional))
                                 (set (keys mandatory))))
    (prs-error (str "Optional and mandatory keyword arguments should be disjoint: "
                    (set/intersection (set (keys optional))
                                      (set (keys mandatory))))))
  (let [optional (into {}
                       (map (fn [[k v]]
                              (do (when-not (keyword? k) (prs-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                                  [(r/-val k) (parse-type v)])))
                       optional)
        mandatory (into {}
                        (map (fn [[k v]]
                               (do (when-not (keyword? k) (prs-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                                   [(r/-val k) (parse-type v)])))
                        mandatory)]
    (apply c/Un (for [opts (map #(into {} %) (comb/subsets optional))
                      :let [m (into mandatory opts)
                            kss (comb/permutations (keys m))]
                      ks kss
                      k (r/-hseq (mapcat #(find m %) ks))]
                  k))))

(declare unparse-type)

(defn parse-type-list-default 
  [[n & args :as syn]]
  (let [op (parse-type n)]
    ;(prn "tapp op" op)
    (when-not ((some-fn r/Name? r/TypeFn? r/F? r/B? r/Poly?) op)
      (prs-error (str "Invalid operator to type application: " syn)))
    (with-meta (r/TApp-maker op (mapv parse-type args))
               {:syn syn
                :env vs/*current-env*})))

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (parse-type-list-default syn))

(defmethod parse-type* Cons [l] (parse-type-list l))
(defmethod parse-type* IPersistentList [l] 
  (parse-type-list l))

(defn parse-Any [sym]
  (assert (not (-> sym meta :clojure.core.typed/infer))
          "^:clojure.core.typed/infer Any support has been removed. Use t/Infer")
  r/-any)

(defmethod parse-type-symbol 'typed.clojure/Any [s] (parse-Any s))
(defmethod parse-type-symbol 'typed.clojure/Infer [s] r/-wild)
(defmethod parse-type-symbol 'typed.clojure/TCError [t] (r/TCError-maker))
(defmethod parse-type-symbol 'typed.clojure/Nothing [_] (r/Bottom))
(defmethod parse-type-symbol 'typed.clojure/AnyFunction [_] (r/TopFunction-maker))

;; hmmm...a psuedo primitive?
(defmethod parse-type-symbol 'typed.clojure/CLJSInteger [_]
  (impl/assert-cljs 'typed.clojure/CLJSInteger)
  (r/CLJSInteger-maker))
(defmethod parse-type-symbol 'typed.clojure/JSnumber [_]
  (impl/assert-cljs 'typed.clojure/JSnumber)
  (r/JSNumber-maker))
(defmethod parse-type-symbol 'typed.clojure/JSboolean [_]
  (impl/assert-cljs 'typed.clojure/JSboolean)
  (r/JSBoolean-maker))
#_ ;; js/Object
(defmethod parse-type-symbol 'typed.clojure/JSobject [_]
  (impl/assert-cljs 'typed.clojure/JSobject)
  (r/JSObject-maker))
(defmethod parse-type-symbol 'typed.clojure/JSstring [_]
  (impl/assert-cljs 'typed.clojure/JSstring)
  (r/JSString-maker))
(defmethod parse-type-symbol 'typed.clojure/JSundefined [_]
  (impl/assert-cljs 'typed.clojure/JSundefined)
  (r/JSUndefined-maker))
(defmethod parse-type-symbol 'typed.clojure/JSnull [_]
  (impl/assert-cljs 'typed.clojure/JSnull)
  (r/JSNull-maker))
(defmethod parse-type-symbol 'typed.clojure/JSsymbol [_]
  (impl/assert-cljs 'typed.clojure/JSsymbol)
  (r/JSSymbol-maker))

(defn clj-primitives-fn []
  (let [RClass-of @(RClass-of-var)]
    {'byte (RClass-of 'byte)
     'short (RClass-of 'short)
     'int (RClass-of 'int)
     'long (RClass-of 'long)
     'float (RClass-of 'float)
     'double (RClass-of 'double)
     'boolean (RClass-of 'boolean)
     'char (RClass-of 'char)
     'void r/-nil}))

(defn ^:dynamic parse-type-symbol-default
  [sym]
  (let [primitives (impl/impl-case
                     :clojure (clj-primitives-fn)
                     :cljs {})
        free (when (symbol? sym) 
               (free-ops/free-in-scope sym))
        rsym (when-not free
               (impl/impl-case
                 :clojure (let [res (when (symbol? sym)
                                      (resolve-type-clj sym))]
                            (cond
                              (class? res) (coerce/Class->symbol res)
                              (var? res) (coerce/var->symbol res)
                              ;; name doesn't resolve, try declared protocol or datatype
                              ;; in the current namespace
                              :else (or (resolve-type-alias-clj sym)
                                        (let [ns (parse-in-ns)
                                              dprotocol (if (namespace sym)
                                                          sym
                                                          (symbol (str ns) (str sym)))
                                              ddatatype (if (some #{\.} (str sym))
                                                          sym
                                                          (symbol (str (munge ns)) (str sym)))]
                                          (cond
                                            (nme-env/declared-protocol? dprotocol) dprotocol
                                            (nme-env/declared-datatype? ddatatype) ddatatype)))))
                 :cljs (when (symbol? sym)
                         (resolve-type-cljs sym))))
        _ (assert ((some-fn symbol? nil?) rsym))]
    ;(prn `parse-type-symbol-default sym rsym)
    (or free
        (primitives sym)
        (parse-type-symbol sym)
        (cond
          rsym (r/Name-maker rsym)
          :else (prs-error (str "Cannot resolve type: " (pr-str sym)
                                "\nHint: Is " (pr-str sym) " in scope in namespace"
                                " `" (parse-in-ns) "`?"))))))

(defmethod parse-type-symbol :default
  [sym]
  nil)

(defmethod parse-type* Symbol [l] (parse-type-symbol-default l))
(defmethod parse-type* Boolean [v] (if v r/-true r/-false)) 
(defmethod parse-type* nil [_] r/-nil)

(declare parse-path-elem parse-filter*)

(defn parse-filter [f]
  (cond
    (= 'tt f) f/-top
    (= 'ff f) f/-bot
    (= 'no-filter f) f/-no-filter
    (not (seq? f)) (prs-error (str "Malformed filter expression: " (pr-str f)))

    (when-some [op (first f)]
      (and (simple-symbol? op)
           (or (= 'or op)
               ;; clojure-clr treats pipes in symbols as special
               (= "|" (name op)))))
    (apply fl/-or (mapv parse-filter (next f)))

    :else (parse-filter* f)))

(defn parse-object-path [{:keys [id path]}]
  (when-not (f/name-ref? id)
    (prs-error (str "Must pass natural number or symbol as id: " (pr-str id))))
  (orep/-path (when path (mapv parse-path-elem path)) id))

(defn parse-object [obj]
  (case obj
    empty orep/-empty
    no-object orep/-no-object
    (parse-object-path obj)))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (when-not (map? fsyn)
    (prs-error "Filter set must be a map"))
  (when-some [extra (not-empty (set/difference (set (keys fsyn)) #{:then :else}))]
    (prs-error (str "Invalid filter set options: " extra)))
  (fl/-FS (if (contains? fsyn :then)
            (parse-filter then)
            f/-top)
          (if (contains? fsyn :else)
            (parse-filter else)
            f/-top)))

(defmulti parse-filter* 
  #(when (coll? %)
     (first %)))

(defmethod parse-filter* :default
  [syn]
  (prs-error (str "Malformed filter expression: " (pr-str syn))))

(defmethod parse-filter* 'is
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (prs-error (str "Wrong number of arguments to is")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-filter t nme p)))

(defmethod parse-filter* '!
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (prs-error (str "Wrong number of arguments to !")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-not-filter t nme p)))

(defmethod parse-filter* '& [[_ & fsyns]]
  (apply fl/-and (mapv parse-filter fsyns)))

(defmethod parse-filter* 'when
  [[_ & [a c :as args] :as all]]
  (when-not (= 2 (count args))
    (prs-error (str "Wrong number of arguments to when: " all)))
  (fl/-imp (parse-filter a) (parse-filter c)))

;FIXME clean up the magic. eg. handle (Class foo bar) as an error
(defmulti parse-path-elem
  #(cond
     (symbol? %) %
     (coll? %) (first %)
     :else 
       (prs-error (str "Malformed path element: " (pr-str %)))))

(defmethod parse-path-elem :default [syn]
  (prs-error (str "Malformed path element: " (pr-str syn))))

(defmethod parse-path-elem 'Class [sym]
  (when-not (= 'Class sym)
    (prs-error "Class path element must be a symbol"))
  (pthrep/ClassPE-maker))

(defmethod parse-path-elem 'Count [sym]
  (when-not (= 'Count sym)
    (prs-error "Count path element must be a symbol"))
  (pthrep/CountPE-maker))

(defmethod parse-path-elem 'Keys [sym]
  (when-not (= 'Keys sym)
    (prs-error "Keys path element must be a symbol"))
  (pthrep/KeysPE-maker))

(defmethod parse-path-elem 'Vals [sym]
  (when-not (= 'Vals sym)
    (prs-error "Vals path element must be a symbol"))
  (pthrep/ValsPE-maker))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (when-not (= 1 (count all))
    (prs-error "Wrong arguments to Key"))
  (pthrep/-kpe ksyn))

(defmethod parse-path-elem 'Nth
  [[_ & [idx :as all]]]
  (when-not (= 1 (count all))
    (prs-error "Wrong arguments to Nth"))
  (pthrep/NthPE-maker idx))

(defmethod parse-path-elem 'Seq
  [[_ & [idx :as all]]]
  (when-not (= 1 (count all))
    (prs-error "Wrong arguments to Nth"))
  (pthrep/NthPE-maker idx))

(defmethod parse-path-elem 'Keyword [sym]
  (when-not (= 'Keyword sym)
    (prs-error "Keyword path element must be a symbol"))
  (pthrep/KeywordPE-maker))

(defmethod parse-path-elem 'Seq [sym]
  (when-not (= 'Seq sym)
    (prs-error "Seq path element must be a symbol"))
  (pthrep/SeqPE-maker))

(defn- parse-kw-map [m]
  {:post [((con/hash-c? r/Value? r/Type?) %)]}
  (into {}
        (map (fn [[k v]]
               [(r/-val k) (parse-type v)]))
        m))

(defn distribute-regex-HSequential [t repeat?]
  {:pre [(r/Regex? t)]}
  (case (:kind t)
    :cat (do (when (some r/Regex? (:types t))
               (prs-error (str "Regex not supported in t/cat in HSequential position: " (:kind t))))
             (r/-hsequential (:types t) :repeat repeat?))
    (prs-error (str "Regex not supported in HSequential position: " (:kind t)))))

(defn push-HSequential->regex [{:keys [repeat types] :as t}]
  {:pre [(r/HSequential? t)]}
  (r/regex (vec types) :cat))

(defn distribute-regex-arities [ts rng]
  {:pre [(r/Result? rng)
         (vector? ts)
         (every? (some-fn r/Type? r/Regex? r/DottedPretype? r/KwArgs?) ts)]
   :post [(every? r/Function? %)]}
  (if (empty? ts)
    [(r/make-Function ts rng)]
    (let [last-t (dec (count ts))
          handle-regex (fn handle-regex [t final-pos?]
                         {:pre [((some-fn r/Type? r/Regex? r/DottedPretype? r/KwArgs?) t)
                                (boolean? final-pos?)]
                          :post [(every? (partial every? #(case (:kind %)
                                                            (:fixed :rest) (r/Type? (:type %))
                                                            :regex ((some-fn r/Regex? r/DottedPretype?) (:type %))
                                                            :pdot ((every-pred r/DottedPretype?
                                                                               (comp r/HSequential? :pre-type))
                                                                   (:type %))
                                                            :drest (r/DottedPretype? (:type %))
                                                            :prest (r/HSequential? (:type %))
                                                            :kws (r/KwArgs? (:type %))))
                                         %)]}
                         (let [handle-regex #(handle-regex % final-pos?)]
                           (cond
                             (r/Regex? t)
                             (case (:kind t)
                               :cat (reduce (fn [tts t]
                                              (into [] (comp (mapcat (fn [ts]
                                                                       (map #(into ts %)
                                                                            (handle-regex t))))
                                                             (distinct))
                                                    tts))
                                            [[]] (:types t))
                               :alt (into [] (comp (mapcat handle-regex) (distinct)) (:types t))
                               :? (into [[]] (handle-regex (-> t :types first)))
                               (:+ :*) (if (not final-pos?)
                                         [[{:kind :regex
                                            :type t}]]
                                         (let [inner (-> t :types first)]
                                           (if (r/Regex? inner)
                                             (case (:kind inner)
                                               ;; prest
                                               :cat [[{:kind :prest
                                                       :type (distribute-regex-HSequential inner true)}]]
                                               [[{:kind :regex
                                                  :type t}]])
                                             [(-> []
                                                  (cond-> (= :+ (:kind t)) (conj {:kind :fixed
                                                                                  :type inner}))
                                                  (conj {:kind :rest
                                                         :type inner}))]))))

                             (r/KwArgs? t)
                             [[{:kind :kws
                                :type t}]]

                             (r/DottedPretype? t)
                             (cond
                               (not final-pos?)
                               [[{:kind :regex
                                  :type t}]]

                               (r/Regex? (:pre-type t))
                               [[{:kind :pdot
                                  :type (update t :pre-type distribute-regex-HSequential true)}]]

                               :else 
                               [[{:kind :drest
                                  :type t}]])

                             :else
                             [[{:kind :fixed
                                :type t}]])))
          ;_ (prn "ts" ts)
          grouped (reduce (fn [tts [i t]]
                            (let [rights (handle-regex t (= i last-t))]
                              (into [] (comp (mapcat (fn [ts]
                                                       (mapv #(into ts %) rights)))
                                             (distinct))
                                    tts)))
                          [[]] (map-indexed vector ts))]
      ;(prn "grouped" grouped)
      (->> grouped
           (mapv
             (fn [arity]
               {:pre [(vector? arity)]}
               (let [kind-groups (into [] (partition-by :kind) arity)
                     ngroups (count kind-groups)
                     kind-order (mapv (comp :kind first) kind-groups)
                     first-kind (first kind-order)
                     fixed (if (= :fixed first-kind)
                             (mapv :type (first kind-groups))
                             [])
                     final-kind (peek kind-order)]
                 (if (or (some (comp #{:regex} :kind) arity)
                         (< 2 ngroups)
                         (and (= 2 ngroups)
                              (not= :fixed first-kind)))
                   (r/make-Function [] rng :regex (r/regex (mapv #(case (:kind %)
                                                                    (:fixed :regex :drest) (:type %)
                                                                    :pdot (update (:type %) :pre-type push-HSequential->regex)
                                                                    :prest (r/regex [(push-HSequential->regex (:type %))] :*)
                                                                    :rest (r/regex [(:type %)] :*))
                                                                 arity)
                                                           :cat))
                   (let [final-group (peek kind-groups)
                         final-type (when final-kind
                                      (when (not= :fixed final-kind)
                                        (when (not= 1 (count final-group))
                                          (prs-error (str "Not allowed more than one final regex in function domain")))
                                        (-> final-group first :type)))]
                     (r/make-Function fixed
                                      rng
                                      :rest (when (= :rest final-kind) final-type)
                                      :drest (when (= :drest final-kind) final-type)
                                      :prest (when (= :prest final-kind) final-type)
                                      :pdot (when (= :pdot final-kind) final-type)
                                      :kws (when (= :kws final-kind) final-type)))))))))))

(def ^:private is-arrow #{'-> :->})

(defn parse-cat [ts]
  {:pre [(vector? ts)]}
  (loop [cat-dom []
         [d1 d2 d3 :as to-process] ts]
    (if (empty? to-process)
      cat-dom
      (case d1
        & (case (count to-process)
            1 (prs-error "Must provide syntax after &")
            2 (if (map? d2)
                (do (err/deprecated-warn "[& {} :-> ] function syntax is deprecated. Use [& :optional {} :-> ]")
                    (recur (conj cat-dom (r/-kw-args :optional (parse-kw-map d2)))
                           (subvec to-process 2)))
                (prs-error "Must provide key-value syntax after &"))
            (let [[cat-dom to-process] (loop [mandatory nil
                                              optional nil
                                              [d1 d2 :as to-process] (subvec to-process 1)]
                                         (if (empty? to-process)
                                           [(conj cat-dom (r/-kw-args :mandatory (or mandatory {})
                                                                      :optional (or optional {})))
                                            to-process]
                                           (case d1
                                             :mandatory (do (when mandatory
                                                              (prs-error ":mandatory only allowed once per function type."))
                                                            (when (< (count to-process) 2)
                                                              (prs-error "Missing type after :mandatory in function type."))
                                                            (when-not (map? d2)
                                                              (prs-error (str "Expected map after :mandatory in function type: " (pr-str d2))))
                                                            (recur (parse-kw-map d2) optional (subvec to-process 2)))
                                             :optional (do (when optional
                                                             (prs-error ":optional only allowed once per function type."))
                                                           (when (< (count to-process) 2)
                                                             (prs-error "Missing type after :optional in function type."))
                                                           (recur mandatory (parse-kw-map d2) (subvec to-process 2)))
                                             ;; force & to go last in function syntax for now
                                             (prs-error (str "Unknown option after & for keyword arguments: " d1)))))]
              (recur cat-dom to-process)))
        (case d2
          (:? :+ :* * <*) (if (or (not= '* d2)
                                  (and (= 2 (count to-process))
                                       (do (err/deprecated-warn "* function syntax is deprecated and only supported as final arguments. Use :* instead.")
                                           true)))
                            (recur (conj cat-dom (case d2
                                                   <* (r/regex [(-> d1 allow-regex parse-type push-HSequential->regex)] :*)
                                                   (:? :+ :* *) (r/regex [(-> d1 allow-regex parse-type)] (keyword d2))))
                                   (subvec to-process 2))
                            (recur (conj cat-dom (-> d1 allow-regex parse-type))
                                   (subvec to-process 1)))
          (:.. :... ... <...) (if (or (= :.. d2)
                                      (when (not= '<... d2)
                                        (err/deprecated-warn (str d2 " function syntax is deprecated and only supported as final arguments. Use :.. instead.")))
                                      (= 3 (count to-process)))
                                (let [drest-bnd d3
                                      _ (when-not (simple-symbol? drest-bnd)
                                          (prs-error (str "Bound after " d2 " must be simple symbol: " (pr-str drest-bnd))))
                                      bnd (free-ops/free-in-scope-bnds drest-bnd)
                                      f (free-ops/free-in-scope drest-bnd)
                                      _ (when-not (r/Regex? bnd)
                                          (prs-error (str "Bound " (pr-str drest-bnd) " after " d2 " is not in scope as a dotted variable")))]
                                  (recur (conj cat-dom (r/DottedPretype1-maker
                                                         (cond-> (free-ops/with-bounded-frees {(r/make-F drest-bnd)
                                                                                               ((requiring-resolve 'typed.cljc.checker.cs-gen/homogeneous-dbound->bound)
                                                                                                bnd)}
                                                                   (-> d1 allow-regex parse-type))
                                                           (= '<... d2) push-HSequential->regex)
                                                         (:name f)))
                                         (subvec to-process 3)))
                                (recur (conj cat-dom (-> d1 allow-regex parse-type))
                                       (subvec to-process 1)))

          (recur (conj cat-dom (-> d1 allow-regex parse-type))
                 (subvec to-process 1)))))))

(defn parse-Regex [[_ & ts :as this] kind]
  (when-not (regex-allowed? this)
    (prs-error (str "Regex type not allowed here: " this)))
  (case kind
    (:* :+ :?) (when-not (= 1 (count ts))
                 (prs-error (str (name kind) " regex takes 1 type, given: " (count ts))))
    (:alt :or :cat) nil)
  (r/regex (if (= :cat kind)
             (parse-cat (vec ts))
             (mapv (comp parse-type* allow-regex) ts))
           kind))

(defmethod parse-type-list 'typed.clojure/+ [t] (parse-Regex t :+))
(defmethod parse-type-list 'typed.clojure/* [t] (parse-Regex t :*))
(defmethod parse-type-list 'typed.clojure/? [t] (parse-Regex t :?))
(defmethod parse-type-list 'typed.clojure/alt [t] (parse-Regex t :alt))
(defmethod parse-type-list 'typed.clojure/cat [t] (parse-Regex t :cat))

;;TODO use [arg1 :- type1, arg2 :- type2 :-> out] syntax to scope objects in range
(defn parse-function [f]
  {:pre [(vector? f)]
   :post [(seq %)
          (every? r/Function? %)]}
  (let [[before-arrow after-arrow] (split-with (complement is-arrow) f)
        _ (when (empty? after-arrow)
            (prs-error (str "Missing arrow in function type: " f)))
        cat-dom (parse-cat (vec before-arrow))
        _ (when (empty? (next after-arrow))
            (prs-error (str "Missing type after arrow in function type: " f)))
        rng (parse-type (second after-arrow))]
    (loop [filters nil
           object nil
           [d1 d2 :as to-process] (vec (nnext after-arrow))]
      (assert (vector? to-process) (pr-str to-process))
      (if (empty? to-process)
        (distribute-regex-arities
          cat-dom
          (r/make-Result rng filters object))
        (case d1
          :filters (do (when filters
                         (prs-error ":filters only allowed once per function type."))
                       (when (< (count to-process) 2)
                         (prs-error "Missing filter set after :filters in function type."))
                       (recur (parse-filter-set d2) object (subvec to-process 2)))
          :object (do (when object
                        (prs-error ":object only allowed once per function type."))
                      (when (< (count to-process) 2)
                        (prs-error "Missing object after :objects in function type."))
                      (recur filters (parse-object d2) (subvec to-process 2)))
          (prs-error (str "Invalid function keyword option/s: " (pr-str d1))))))))

(defmethod parse-type* IPersistentVector
  [f]
  (apply r/make-FnIntersection (parse-function f)))

(defmethod parse-type* :default
  [k]
  (prs-error (str "Bad type syntax: " (pr-str k)
                  (when ((some-fn symbol? keyword?) k)
                    (str "\n\nHint: Value types should be preceded by a quote or wrapped in the Value constructor." 
                         " eg. '" (pr-str k) " or (Value " (pr-str k)")")))))

(comment
  (parse-clj `(t/All [s#] [s# :-> s#]))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse

;; Unparsing types are generally agnostic to the current implementation.
;; Special types are unparsed under clojure.core.typed in the :unknown
;; implementation. All other types are verbosely printed under :unknown.

(defonce ^:dynamic *unparse-type-in-ns* nil)
(set-validator! #'*unparse-type-in-ns* (some-fn nil? symbol?))

(defn unparse-in-ns []
  {:post [((some-fn nil? symbol?) %)]}
  (or *unparse-type-in-ns*
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (cljs-ns)
        :unknown nil)))

(defmacro with-unparse-ns [sym & body]
  `(binding [*unparse-type-in-ns* ~sym]
     ~@body))

(defn alias-in-ns
  "Returns an alias for namespace nsym in namespace ns, or nil if none."
  [ns nsym]
  {:pre [(plat-con/namespace? ns)
         (simple-symbol? nsym)]
   :post [((some-fn nil? symbol?) %)]}
  (impl/assert-clojure)
  (some (fn [[alias ans]]
          (let [ans-sym (ns-name ans)]
            (when (or (= nsym (ns-unrewrites-clj ans-sym))
                      (= nsym ans-sym)
                      (and (= (ns-name nsym) 'typed.clojure)
                           (= (ns-name ans) 'clojure.core.typed)))
              alias)))
        ;; prefer shorter, lexicographically earlier aliases
        (sort-by (juxt (comp count name key) key)
                 (ns-aliases ns))))

(defn core-lang-Class-sym [clsym]
  {:pre [(symbol? clsym)]
   :post [((some-fn nil? symbol?) %)]}
  (when (.startsWith (str clsym) "clojure.lang.")
    (symbol (.getSimpleName (Class/forName (str clsym))))))

(defn Class-symbol-intern
  "If symbol named by class is interned in ns by its
  simple name, return the simple name."
  [clsym ns]
  {:pre [(plat-con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (when (nil? (namespace clsym))
    (let [clstr (name clsym)
          last-dot (.lastIndexOf clstr ".")]
      (when (<= 0 last-dot (- (count clstr) 2))
        (let [short-name (symbol (subs clstr (inc last-dot)))]
          (when-some [mapping (ns-map ns)]
            (when (mapping short-name)
              short-name)))))))

(defn var-symbol-intern 
  "Returns a symbol interned in ns for var symbol, or nil if none.

  (var-symbol-intern 'clojure.core/symbol (find-ns 'clojure.core))
  ;=> 'symbol
  (var-symbol-intern 'bar (find-ns 'clojure.core))
  ;=> nil"
  [sym ns]
  {:pre [(symbol? sym)
         (plat-con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (let [mapping (ns-map ns)
        sym-simple-name (name sym)
        sym-simple (symbol sym-simple-name)
        sym-ns-name (namespace sym)]
    (reduce-kv (fn [_ isym var]
                 (when (and (var? var)
                            (= (.sym ^clojure.lang.Var var) sym-simple)
                            (= (str (.ns ^clojure.lang.Var var)) sym-ns-name))
                   (reduced isym)))
               nil
               mapping)))

(defn unparse-Name-symbol-in-ns [sym]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  ;(prn "unparse-Name-symbol-in-ns" sym)
  (if-let [ns (and (not vs/*verbose-types*)
                   (some-> (unparse-in-ns) find-ns))]
    (impl/impl-case
      :clojure
      (or ; use an import name
          (Class-symbol-intern sym ns)
          ; implicitly imported classes are special
          (core-lang-Class-sym sym)
          ; use unqualified name if interned
          (when (namespace sym)
            (or (var-symbol-intern sym ns)
                ; use aliased ns if not interned, but ns is aliased
                (when-let [alias (alias-in-ns ns (symbol (namespace sym)))]
                  (symbol (str alias) (name sym)))))
          ; otherwise use fully qualified name
          sym)
      :cljs sym
      :unknown sym)
    sym))

(t/ann unparse-type [r/AnyType :-> t/Any])
(defn unparse-type [t]
  ; quick way of giving a Name that the user is familiar with
  ;(prn "unparse-type" (class t))
  (or (when-not vs/*verbose-types*
        (-> t meta :source-Name))
      (unparse-type* t)))

(defn unp [t] (prn (unparse-type t)))

(defn unparse-F [f]
  {:pre [(r/F? f)]}
  (if (:unique-tvars vs/*verbose-types*)
    (:name f)
    (r/F-original-name f)))

(defn unparse-DottedPretype [{:keys [pre-type name] :as t} flat?]
  {:pre [(r/DottedPretype? t)]}
  (let [p (unparse-type pre-type)
        n (cond-> name
            (symbol? name) (-> r/make-F unparse-F))]
    (if flat?
      [p :.. n]
      (list 'DottedPretype p n))))

(extend-protocol IUnparseType
  Top
  (unparse-type* [t] (unparse-Name-symbol-in-ns `t/Any))
  Wildcard 
  (unparse-type* [t] (unparse-Name-symbol-in-ns `t/Infer))
;; TODO qualify vsym in current ns
  TypeOf 
  (unparse-type* [{:keys [vsym] :as t}] (list (unparse-Name-symbol-in-ns `t/TypeOf) vsym))
  Unchecked 
  (unparse-type* [{:keys [vsym] :as t}]
    (if vsym
      (list 'Unchecked vsym)
    'Unchecked))
  TCError 
  (unparse-type* [_] (unparse-Name-symbol-in-ns `t/TCError))
  Name 
  (unparse-type* [{:keys [id]}] (unparse-Name-symbol-in-ns id))
  AnyValue 
  (unparse-type* [_] (unparse-Name-symbol-in-ns `t/AnyValue))

  DottedPretype
  (unparse-type* [t] (unparse-DottedPretype t false))

  CountRange 
  (unparse-type* [{:keys [lower upper]}]
    (cond
      (= lower upper) (list (unparse-Name-symbol-in-ns `t/ExactCount)
                            lower)
      :else (list* (unparse-Name-symbol-in-ns `t/CountRange)
                   lower
                   (when upper [upper]))))

  App 
  (unparse-type* [{:keys [rator rands]}]
    (list* (unparse-type rator) (mapv unparse-type rands)))

  TApp 
  (unparse-type* [{:keys [rator rands] :as tapp}]
    (cond 
      ;perform substitution if obvious
      ;(TypeFn? rator) (unparse-type (resolve-tapp tapp))
      :else (list* (unparse-type rator) (mapv unparse-type rands))))

  Result
  (unparse-type* [{:keys [t]}] (unparse-type t))

  F
  (unparse-type* [f] (unparse-F f))

  PrimitiveArray
  (unparse-type* 
    [{:keys [jtype input-type output-type]}]
    (cond 
      (and (= input-type output-type)
           (= Object jtype))
      (list 'Array (unparse-type input-type))

      (= Object jtype)
      (list 'Array2 (unparse-type input-type) (unparse-type output-type))

      :else
      (list 'Array3 (coerce/Class->symbol jtype)
            (unparse-type input-type) (unparse-type output-type))))

  B
  (unparse-type* 
    [{:keys [idx]}]
    (list 'B idx))

  Union
  (unparse-type* 
    [{types :types :as u}]
    (cond
      ; Prefer the user provided Name for this type. Needs more thinking?
      ;(-> u meta :from-name) (-> u meta :from-name)
      (seq types) (list* (unparse-Name-symbol-in-ns `t/U) (mapv unparse-type types))
      :else (unparse-Name-symbol-in-ns `t/Nothing)))

  FnIntersection
  (unparse-type* 
    [{types :types}]
    ; use vector sugar where appropriate
    ; note: sugar is expected by unparse of t/Match
    (if (= 1 (count types))
      (unparse-type (first types))
      (list* (unparse-Name-symbol-in-ns `t/IFn)
             (mapv unparse-type types))))

  Intersection
  (unparse-type* 
    [{types :types}]
    (list* (unparse-Name-symbol-in-ns `t/I)
           (mapv unparse-type types)))

  DifferenceType
  (unparse-type* 
    [{:keys [type without]}]
    (list* (unparse-Name-symbol-in-ns `t/Difference)
           (unparse-type* type)
           (mapv unparse-type without)))

  NotType
  (unparse-type* 
    [{:keys [type]}]
    (list (unparse-Name-symbol-in-ns `t/Not) (unparse-type type)))

  TopFunction 
  (unparse-type* [_] (unparse-Name-symbol-in-ns `t/AnyFunction)))

(defn- unparse-kw-map [m]
  {:pre [((con/hash-c? r/Value? r/Type?) m)]}
  (reduce-kv (fn [m k v]
               (assoc m (:val k) (unparse-type v)))
             {} m))

(defn unparse-result [{:keys [t fl o] :as rng}]
  {:pre [(r/Result? rng)]}
  (cond-> [(unparse-type t)]
    (not-every? (some-fn f/TopFilter? f/NoFilter?) [(:then fl) (:else fl)])
    (conj :filters (unparse-filter-set fl))

    (not ((some-fn orep/NoObject? orep/EmptyObject?) o))
    (conj :object (unparse-object o))))

(defn unparse-bound [name]
  {:pre [((some-fn symbol? nat-int?) name)]}
  (if (symbol? name)
    (-> name r/make-F unparse-F)
    `(~'B ~name)))

(defn unparse-regex [{:keys [kind types]} flatten?]
  (let [ts (into [] (mapcat (fn [t]
                              (cond
                                (r/DottedPretype? t) (unparse-DottedPretype t true)
                                (r/Regex? t) (case [kind (:kind t)]
                                               ([:cat :*]
                                                [:cat :+]
                                                [:cat :?]) [(-> t :types first unparse-type) (:kind t)]
                                               [(unparse-type t)])
                                :else [(unparse-type t)])))
                 types)]
    (cond->> ts
      (not (and flatten? (= :cat kind)))
      (list* (unparse-Name-symbol-in-ns (symbol "typed.clojure" (name kind)))))))

(extend-protocol IUnparseType
  SymbolicClosure
  (unparse-type* 
    [{:keys [fexpr smallest-type]}]
    (cond->> (unparse-type* smallest-type)
      vs/*verbose-types*
      (list 'SymbolicClosure (binding [*print-length* 5
                                       *print-level* 5]
                               (pr-str (:form fexpr))))))

  Regex
  (unparse-type* [t] (unparse-regex t false))

  Function
  (unparse-type*
    [{:keys [dom rng kws rest drest prest pdot regex]}]
    {:post [(vector? %)]}
    (let [before-arrow
          (if regex
            (unparse-regex regex true)
            (cond-> (mapv unparse-type dom)
              rest (conj (unparse-type rest) :*)
              drest (into (let [{:keys [pre-type name]} drest]
                            [(unparse-type pre-type)
                             :..
                             (unparse-bound name)]))
              kws (into (let [{:keys [optional mandatory]} kws]
                          (list* '&
                                 (concat
                                   (when (seq mandatory)
                                     [:mandatory (unparse-kw-map mandatory)])
                                   (when (seq optional)
                                     [:optional (unparse-kw-map optional)])))))
              prest (conj (unparse-type prest) '<*)
              pdot (into (let [{:keys [pre-type name]} pdot]
                           [(unparse-type pre-type)
                            '<...
                            (unparse-bound name)]))))]
      (-> before-arrow
          (conj :->)
          (into (unparse-result rng))))))

(extend-protocol IUnparseType
  Protocol
  (unparse-type* 
    [{:keys [the-var poly?]}]
    (let [s (unparse-Name-symbol-in-ns the-var)]
      (if poly?
        (list* s (mapv unparse-type poly?))
        s)))

  DataType
  (unparse-type* 
    [{:keys [the-class poly?]}]
    (if poly?
      (list* (unparse-Name-symbol-in-ns the-class) (mapv unparse-type poly?))
      (unparse-Name-symbol-in-ns the-class)))

  RClass
  (unparse-type* 
    [{:keys [the-class poly?] :as r}]
    (if (empty? poly?)
      (unparse-Name-symbol-in-ns the-class)
      (list* (unparse-Name-symbol-in-ns the-class) (mapv unparse-type poly?))))

  Mu
  (unparse-type* 
    [m]
    (let [nme (-> (c/Mu-fresh-symbol* m) r/make-F unparse-F)
          body (c/Mu-body* nme m)]
      (list (unparse-Name-symbol-in-ns `t/Rec) [nme] (unparse-type body))))

  MatchType
  (unparse-type* 
    [m]
    (list* (unparse-Name-symbol-in-ns `t/Match)
           (unparse-type (:target m))
           (doall
             (mapcat (fn [t]
                       (let [tsyn (unparse-type t)]
                         ;; must be either [pattern :-> result] or (All binder [pattern :-> result])
                         (if (vector? tsyn)
                           (do (assert (= 3 (count tsyn)))
                               tsyn)
                           (let [_ (assert (= 3 (count tsyn)))
                                 [_All binder tsyn] tsyn]
                             (assert (and (vector? tsyn) (= 3 (count tsyn))))
                             (into [binder] tsyn)))))
                     (:clauses m)))))
  Instance
  (unparse-type*
    [m]
    (list (unparse-Name-symbol-in-ns `t/Instance)
          (unparse-Name-symbol-in-ns (:the-class m))))
  Satisfies
  (unparse-type*
    [m]
    (list (unparse-Name-symbol-in-ns `t/Satisfies)
          (unparse-Name-symbol-in-ns (:the-var m)))))

(defn Bounds->vector [{:keys [upper-bound lower-bound] :as bnds}]
  {:pre [(r/Bounds? bnds)]}
  (cond-> []
    (not (r/Top? upper-bound)) (conj :< (unparse-type upper-bound))
    (not (r/Bottom? lower-bound)) (conj :> (unparse-type lower-bound))))

(defn unparse-poly-bounds-entry [name bnds]
  {:pre [(r/Bounds? bnds)]}
  (let [name (-> name r/make-F unparse-F)]
    (if (= r/no-bounds bnds)
      name
      (into [name] (Bounds->vector bnds)))))

(defn unparse-poly-dotted-bounds-entry [free-name bbnd]
  (assert (= r/dotted-no-bounds bbnd)) ;; TODO do something interesting
  [(-> free-name r/make-F unparse-F) :..])

(defn unparse-poly-binder [dotted? free-names bbnds named]
  (let [named-remappings (apply sorted-map (interleave (vals named) (keys named)))
        {:keys [fixed-inb named-inb]} (group-by (fn [[i]]
                                                  (if (named-remappings i)
                                                    :named-inb
                                                    :fixed-inb))
                                                (map vector
                                                     (range)
                                                     free-names
                                                     bbnds))
        [fixed-inb dotted-inb] (if dotted?
                                 ((juxt pop peek) fixed-inb)
                                 [fixed-inb nil])
        unp-inb (fn [[_ free-name bbnd]]
                  (unparse-poly-bounds-entry free-name bbnd))
        binder (into (mapv unp-inb fixed-inb)
                     (concat
                       (when-let [[_ free-name bbnd] dotted-inb]
                         (unparse-poly-dotted-bounds-entry free-name bbnd))
                       (when named-inb
                         [:named (mapv unp-inb named-inb)])))]
    binder))

(extend-protocol IUnparseType
  Extends
  (unparse-type* 
    [{:keys [extends without]}]
    (list* 'Extends
           (mapv unparse-type extends)
           (when (seq without)
             [:without (mapv unparse-type without)])))

  Poly
  (unparse-type* 
    [{:keys [nbound named kind] :as p}]
    (case kind
      :Poly (let [free-names (c/Poly-fresh-symbols* p)
                  ;_ (prn "Poly unparse" free-names (map meta free-names))
                  bbnds (c/Poly-bbnds* free-names p)
                  binder (unparse-poly-binder false free-names bbnds named)
                  body (c/Poly-body* free-names p)]
              (list (unparse-Name-symbol-in-ns `t/All) binder (unparse-type body)))
      :PolyDots (let [free-names (vec (c/PolyDots-fresh-symbols* p))
                      bbnds (c/PolyDots-bbnds* free-names p)
                      binder (unparse-poly-binder true free-names bbnds named)
                      body (c/PolyDots-body* free-names p)]
                  (list (unparse-Name-symbol-in-ns `t/All) binder (unparse-type body))))))

;(ann unparse-typefn-bounds-entry [t/Sym Bounds Variance -> Any])
(defn unparse-typefn-bounds-entry [name bnds v]
  {:pre [(r/Bounds? bnds)]}
  (let [name (-> name r/make-F unparse-F)]
    (into [name :variance v]
          (when (not= r/no-bounds bnds)
            (Bounds->vector bnds)))))

(extend-protocol IUnparseType
  TypeFn
  (unparse-type* 
    [{:keys [nbound] :as p}]
    (let [free-names (c/TypeFn-fresh-symbols* p)
          bbnds (c/TypeFn-bbnds* free-names p)
          binder (mapv unparse-typefn-bounds-entry free-names bbnds (:variances p))
          body (c/TypeFn-body* free-names p)]
      (list (unparse-Name-symbol-in-ns `t/TFn) binder (unparse-type body))))

  Bounds
  (unparse-type* 
    [{:keys [upper-bound lower-bound] :as bnds}]
    (if (= r/no-bounds bnds)
      (unparse-Name-symbol-in-ns `t/Type)
      (list* (unparse-Name-symbol-in-ns `t/Type)
             (Bounds->vector bnds))))

  Value
  (unparse-type* 
    [v]
    (if ((some-fn r/Nil? r/True? r/False?) v)
      (:val v)
      (list (unparse-Name-symbol-in-ns `t/Val) (:val v)))))

(defn- unparse-map-of-types [m]
  (reduce-kv (fn [m k v]
               (assert (r/Value? k) k)
               (assoc m (:val k) (unparse-type v)))
              {} m))

(extend-protocol IUnparseType
  HeterogeneousMap
  (unparse-type* 
    [v]
    (list* (unparse-Name-symbol-in-ns `t/HMap)
           (concat
             ; only elide if other information is present
             (when (or (seq (:types v))
                       (not (or (seq (:optional v))
                                (seq (:absent-keys v))
                                (c/complete-hmap? v))))
               [:mandatory (unparse-map-of-types (:types v))])
             (when (seq (:optional v))
               [:optional (unparse-map-of-types (:optional v))])
             (when-let [ks (and (not (c/complete-hmap? v))
                                (seq (:absent-keys v)))]
               [:absent-keys (set (map :val ks))])
             (when (c/complete-hmap? v)
               [:complete? true])))))

(defn unparse-KwArgs-trailing [v]
  {:pre [(r/KwArgs? v)]}
  (concat
    (when (seq (:optional v))
      [:optional (unparse-map-of-types (:optional v))])
    (when (seq (:mandatory v))
      [:mandatory (unparse-map-of-types (:mandatory v))])
    (when (:complete? v)
      [:complete? (:complete? v)])))

(defn unparse-heterogeneous* [sym {:keys [types rest drest fs objects repeat] :as v}]
  (let [first-part (concat
                     (map unparse-type (:types v))
                     (when rest [(unparse-type rest) '*])
                     (when drest [(unparse-type (:pre-type drest))
                                  :..
                                  (unparse-bound (:name drest))]))]
    (list* sym
           (vec first-part)
           (concat
             (when repeat
               [:repeat true])
             (when-not (every? #{(fl/-FS f/-top f/-top)} fs)
               [:filter-sets (mapv unparse-filter-set fs)])
             (when-not (every? #{orep/-empty} objects)
               [:objects (mapv unparse-object objects)])))))

(extend-protocol IUnparseType
  HSequential 
  (unparse-type* [v]
    (unparse-heterogeneous*
      (case (:kind v)
        :list (unparse-Name-symbol-in-ns `t/HList)
        :vector (unparse-Name-symbol-in-ns `t/HVec)
        :seq (unparse-Name-symbol-in-ns `t/HSeq)
        :sequential (unparse-Name-symbol-in-ns `t/HSequential))
      v))

  HSet
  (unparse-type* 
    [{:keys [fixed] :as v}]
    {:pre [(every? r/Value? fixed)]}
    (list (unparse-Name-symbol-in-ns `t/HSet) (set (map :val fixed))))

  KwArgsArray
  (unparse-type* 
    [{v :kw-args-regex}]
    (list* 'KwArgsArray (unparse-KwArgs-trailing v)))

  KwArgsSeq
  (unparse-type* 
    [{v :kw-args-regex}]
    (list* 'KwArgsSeq (unparse-KwArgs-trailing v)))

  AssocType
  (unparse-type* 
    [{:keys [target entries dentries]}]
    (list* (unparse-Name-symbol-in-ns `t/Assoc)
           (unparse-type target)
           (into
             (mapv unparse-type (apply concat entries))
             (when dentries [(unparse-type (:pre-type dentries))
                             :..
                             (unparse-bound (:name dentries))]))))

  MergeType
  (unparse-type* 
    [{:keys [types]}]
    (list* (unparse-Name-symbol-in-ns `t/Merge)
           (mapv unparse-type types)))

  GetType
  (unparse-type* 
    [{:keys [target key not-found]}]
    (list* (unparse-Name-symbol-in-ns `t/Get)
           (unparse-type target)
           (unparse-type key)
           (when (not= r/-nil not-found)
             [(unparse-type not-found)])))

; CLJS Types

  JSNumber 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSnumber))
  JSBoolean 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSboolean))
  JSObject 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSobject))
  CLJSInteger 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/CLJSInteger))
  JSString 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSstring))
  JSSymbol 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSsymbol))
  JSUndefined 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSundefined))
  JSNull 
  (unparse-type* [_] (unparse-Name-symbol-in-ns 'typed.clojure/JSnull))
  JSObj 
  (unparse-type* [t] (list (unparse-Name-symbol-in-ns 'typed.clojure/JSObj)
                           (zipmap (keys (:types t))
                                   (map unparse-type (vals (:types t))))))

  ArrayCLJS
  (unparse-type* 
    [{:keys [input-type output-type]}]
    (cond 
      (= input-type output-type) (list 'Array (unparse-type input-type))
      :else (list 'Array2 (unparse-type input-type) (unparse-type output-type))))

  JSNominal
  (unparse-type* 
    [{:keys [name poly?]}]
    (let [sym (symbol name)]
      (if (seq poly?)
        (list* sym (mapv unparse-type poly?))
        sym))))

; Objects

(extend-protocol IUnparseObject
  EmptyObject 
  (unparse-object [_] 'empty)
  NoObject 
  (unparse-object [_] 'no-object)
  Path 
  (unparse-object [{:keys [path id]}] (conj {:id id} (when (seq path) [:path (mapv unparse-path-elem path)]))))

; Path elems

(extend-protocol IUnparsePathElem
  KeyPE 
  (unparse-path-elem [t] (list 'Key (:val t)))
  CountPE 
  (unparse-path-elem [t] 'Count)
  ClassPE 
  (unparse-path-elem [t] 'Class)
  NthPE 
  (unparse-path-elem [t] (list 'Nth (:idx t)))
  KeysPE 
  (unparse-path-elem [t] 'Keys)
  ValsPE 
  (unparse-path-elem [t] 'Vals)
  KeywordPE 
  (unparse-path-elem [t] 'Keyword)
  SeqPE 
  (unparse-path-elem [t] 'Seq))

; Filters

(defn unparse-filter [f]
  (unparse-filter* f))

(defn unparse-filter-set [{:keys [then else] :as fs}]
  {:pre [(f/FilterSet? fs)]}
  {:then (unparse-filter then)
   :else (unparse-filter else)})

(extend-protocol IUnparseFilter
  TopFilter 
  (unparse-filter* [f] 'tt)
  BotFilter 
  (unparse-filter* [f] 'ff)
  NoFilter 
  (unparse-filter* [f] 'no-filter)

  TypeFilter
  (unparse-filter* 
    [{:keys [type path id]}]
    (concat (list 'is (unparse-type type) id)
            (when (seq path)
              [(mapv unparse-path-elem path)])))

  NotTypeFilter
  (unparse-filter* 
    [{:keys [type path id]}]
    (concat (list '! (unparse-type type) id)
            (when (seq path)
              [(mapv unparse-path-elem path)])))

  AndFilter 
  (unparse-filter* [{:keys [fs]}] (apply list '& (map unparse-filter fs)))
  OrFilter 
  (unparse-filter* [{:keys [fs]}] (apply list 'or (map unparse-filter fs)))

  ImpFilter
  (unparse-filter* 
    [{:keys [a c]}]
    (list 'when (unparse-filter a) (unparse-filter c))))

;[TCResult -> Any]
(defn unparse-TCResult [r]
  (let [t (unparse-type (r/ret-t r))
        fs (unparse-filter-set (r/ret-f r))
        o (unparse-object (r/ret-o r))]
    (if (and (= (fl/-FS f/-top f/-top) (r/ret-f r))
             (= (r/ret-o r) orep/-empty))
      t
      (if (= (r/ret-o r) orep/-empty)
        [t fs]
        [t fs o]))))

(defn unparse-TCResult-in-ns [r ns]
  {:pre [((some-fn plat-con/namespace? symbol?) ns)]}
  (binding [*unparse-type-in-ns* (if (symbol? ns)
                                   ns
                                   (ns-name ns))]
    (unparse-TCResult r)))

(extend-protocol IUnparseType
  TCResult
  (unparse-type* [v] (unparse-TCResult v)))
