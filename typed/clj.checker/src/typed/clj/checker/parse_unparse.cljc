;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:typed.clojure ^:no-doc typed.clj.checker.parse-unparse
  (:refer-clojure :exclude [#?(:clj requiring-resolve) type])
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
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])
            [typed.cljc.checker.constant-type :as const]
            [typed.cljc.analyzer.passes.uniquify :as uniquify]
            [typed.cljc.checker.proposition-ops :as fl]
            [typed.cljc.checker.proposition-rep :as f]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.hset-utils :as hset]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.path-rep :as pthrep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r]
            [typed.cljc.runtime.env-utils :as env-utils]
            [typed.cljc.runtime.env :as env])
  (:import (typed.cljc.checker.type_rep NotType Intersection Union FnIntersection
                                        DottedPretype Function Regex RClass TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top Wildcard TypeOf Unchecked TopFunction B F Result
                                        TopKwArgsSeq KwArgsSeq KwArgsArray TCError JSNumber JSBoolean SymbolicClosure
                                        CLJSInteger ArrayCLJS JSNominal JSString TCResult AssocType MergeType
                                        GetType TopHSequential HSequential HSet JSUndefined JSNull JSSymbol JSObject
                                        JSObj Bounds MatchType Instance Satisfies)
           (typed.cljc.checker.proposition_rep TopProposition BotProposition TypeProposition NotTypeProposition AndProposition
                                               OrProposition ImpProposition NoProposition)
           (typed.cljc.checker.object_rep NoObject EmptyObject Path Lexical)
           (typed.cljc.checker.path_rep KeyPE CountPE ClassPE KeysPE ValsPE NthPE KeywordPE SeqPE)
           (clojure.lang Cons IPersistentList Symbol IPersistentVector)))

(env-utils/invalidate-parsed-types!)

(defprotocol IUnparseType 
  (unparse-type* [t opts]))
(defprotocol IUnparseObject
  (unparse-object [o opts]))
(defprotocol IUnparsePathElem
  (unparse-path-elem [p opts]))
(defprotocol IUnparseProposition
  (unparse-proposition* [fl opts]))

(declare unparse-type unparse-proposition unparse-proposition-set unparse-TCResult)

(defn- ->unparse-opts []
  (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
         ::unparse-type-in-ns (ns-name *ns*)))

; Types print by unparsing them
(do (defmethod print-method typed.cljc.checker.impl_protocols.TCType [s writer]
      (print-method (unparse-type s (->unparse-opts)) writer))
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCType clojure.lang.IRecord)
    #?(:clj (prefer-method print-method typed.cljc.checker.impl_protocols.TCType java.util.Map))
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCType clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.impl_protocols.TCAnyType [s writer]
      (print-method (unparse-type s (->unparse-opts)) writer))
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCAnyType clojure.lang.IRecord)
    #?(:clj (prefer-method print-method typed.cljc.checker.impl_protocols.TCAnyType java.util.Map))
    (prefer-method print-method typed.cljc.checker.impl_protocols.TCAnyType clojure.lang.IPersistentMap)

    (defmethod print-method TCResult [s writer]
      (print-method (unparse-TCResult s (->unparse-opts)) writer))
    #?(:clj (prefer-method print-method TCResult java.util.Map))
    (prefer-method print-method TCResult clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.impl_protocols.IProposition [s writer]
      (cond 
        (f/PropositionSet? s) (print-method (unparse-proposition-set s (->unparse-opts)) writer)
        :else (print-method (unparse-proposition s (->unparse-opts)) writer)))
    (prefer-method print-method typed.cljc.checker.impl_protocols.IProposition clojure.lang.IRecord)
    #?(:clj (prefer-method print-method typed.cljc.checker.impl_protocols.IProposition java.util.Map))
    (prefer-method print-method typed.cljc.checker.impl_protocols.IProposition clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.impl_protocols.IRObject [s writer]
      (print-method (unparse-object s (->unparse-opts)) writer))
    (prefer-method print-method typed.cljc.checker.impl_protocols.IRObject clojure.lang.IRecord)
    #?(:clj (prefer-method print-method typed.cljc.checker.impl_protocols.IRObject java.util.Map))
    (prefer-method print-method typed.cljc.checker.impl_protocols.IRObject clojure.lang.IPersistentMap)

    (defmethod print-method typed.cljc.checker.path_rep.IPathElem [s writer]
      (print-method (unparse-path-elem s (->unparse-opts)) writer))
    (prefer-method print-method typed.cljc.checker.path_rep.IPathElem clojure.lang.IRecord)
    #?(:clj (prefer-method print-method typed.cljc.checker.path_rep.IPathElem java.util.Map))
    (prefer-method print-method typed.cljc.checker.path_rep.IPathElem clojure.lang.IPersistentMap)
    )

;; vim-fireplace calls cider.nrepl.pprint/fipp-pprint.
;; because of frustrating issues with nrepl in typed.dev.repl/-main, I haven't figured out how to disable
;; the specific cider middleware that triggers it. this hack prints types in a human readable format.
;; it's necessary to convert to edn before calling to circumvent fipp's defaults for printing maps.
;; this makes extending fipp.edn/IEdn useless for types.
(defn- massage-before-fipp-pprint [x]
  (let [opts (->unparse-opts)
        contains-special-print? (volatile! nil)
        special-pprint (fn [t]
                         (cond
                           (or (instance? typed.cljc.checker.impl_protocols.TCType t)
                               (instance? typed.cljc.checker.impl_protocols.TCAnyType t))
                           [(unparse-type t opts)]
                           (instance? TCResult t) [(unparse-TCResult t opts)]
                           (instance? typed.cljc.checker.impl_protocols.IProposition t) [((if (f/PropositionSet? t)
                                                                                            unparse-proposition-set
                                                                                            unparse-proposition)
                                                                                          t
                                                                                          opts)]
                           (instance? typed.cljc.checker.impl_protocols.IRObject t) [(unparse-object t opts)]
                           (instance? typed.cljc.checker.path_rep.IPathElem t) [(unparse-path-elem t opts)]))
        pprint-types (fn pprint-types [x]
                       (if-some [[printed] (special-pprint x)]
                         (do (vreset! contains-special-print? true)
                             printed)
                         ((requiring-resolve 'clojure.walk/walk) pprint-types identity x)))
        res (pprint-types x)]
    (if @contains-special-print? res x)))

#?(:clj
   (when (= "true" (System/getProperty "typed.clj.checker.parse-unparse.fipp-override"))
     (when (try (require 'fipp.ednize 'fipp.edn 'cider.nrepl.pprint)
                true
                (catch java.io.FileNotFoundException _))
       (alter-var-root (resolve 'cider.nrepl.pprint/fipp-pprint)
                       (fn [fipp-pretty]
                         (fn
                           ([x writer] (-> x ((resolve `massage-before-fipp-pprint)) (fipp-pretty writer)))
                           ([x writer options] (-> x ((resolve `massage-before-fipp-pprint)) (fipp-pretty writer options)))))))
     (System/setProperty "typed.clj.checker.parse-unparse.fipp-override" "false")))

(declare parse-type* resolve-type-clj->sym resolve-type-clj resolve-type-cljs)

(defn tsyn->env [s]
  (let [m (meta s)]
    (when ((every-pred :line :column) m)
      m)))

(defn prs-error
  ([msg opts] (prs-error msg nil opts))
  ([msg opt opts]
   (let [[_ tsyn :as tsyn?] (find opts ::parsing-tsyn)
         menv (when tsyn? (tsyn->env tsyn))]
     (err/int-error (str msg (when tsyn? (str " while parsing type " (pr-str tsyn))))
                    (into {:use-current-env true} opt)
                    (cond-> opts
                      menv (assoc ::vs/current-env menv))))))

(declare parse-in-ns)

(defn parse-type [s opts]
  (let [env (or (tsyn->env s) (::vs/current-env opts))
        opts (-> opts
                 (assoc ::vs/current-env env)
                 (assoc ::parsing-tsyn s))]
    (try (parse-type* s opts)
         (catch #?(:cljr System.Exception :default Throwable) e
           ;(prn (err/any-tc-error? (ex-data e)))
           (if (err/any-tc-error? (ex-data e))
             (throw e)
             (err/int-error (format "parse-type error while parsing %s! Please report to help improve this error message."
                                    (pr-str s))
                            {:visible-cause e}
                            opts))))))

(defn delay-parse-type [s opts]
  ((resolve `parse-type) s opts))

(defn parse-clj
  ([s] (parse-clj s (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                           ::parse-type-in-ns (ns-name *ns*))))
  ([s opts] (parse-type s opts)))

(def ^:private cljs-ns #((requiring-resolve 'typed.cljs.checker.util/cljs-ns)))

(defn parse-cljs
  ([s] (parse-cljs s (assoc ((requiring-resolve 'typed.cljs.runtime.env/cljs-opts))
                            ::parse-type-in-ns (cljs-ns))))
  ([s opts] (parse-type s opts)))

(defmulti parse-type* (fn [s opts] (class s)))
(defmulti parse-type-list 
  (fn [[n] opts]
    {:post [((some-fn nil? symbol?) %)]}
    (when (symbol? n)
      (or (impl/impl-case opts
            :clojure (resolve-type-clj->sym n opts)
            :cljs (or ('#{quote Array Array2} n)
                      (resolve-type-cljs n opts)))
          n))))

(defmulti parse-type-symbol
  (fn [n opts]
    {:pre [(symbol? n)]}
    (or (impl/impl-case opts
          :clojure (resolve-type-clj->sym n opts)
          ;;FIXME logic is all tangled
          :cljs (resolve-type-cljs n opts))
        n)))

(def parsed-free-map? (con/hmap-c? :fname symbol?
                                   :bnd r/Kind?
                                   :variance r/variance?))

; parsing TFn, protocol, RClass binders
(defn ^:private parse-free-with-variance [f opts]
  {:post [(parsed-free-map? %)]}
  (if (symbol? f)
    {:fname f
     :bnd r/no-bounds
     :variance :invariant}
    (let [[n & {:keys [< > variance] :as popts}] f]
      (when (contains? popts :kind)
        (err/deprecated-warn "Kind annotation for TFn parameters" opts))
      (when-not (r/variance? variance)
        (prs-error (str "Invalid variance " (pr-str variance) " in free binder: " f) opts))
      {:fname n 
       :bnd (let [upper-or-nil (when (contains? popts :<)
                                 (parse-type < opts))
                  lower-or-nil (when (contains? popts :>)
                                 (parse-type > opts))]
              (c/infer-bounds upper-or-nil lower-or-nil opts))
       :variance variance})))

(defn parse-free-binder-with-variance [binder opts]
  {:post [(every? parsed-free-map? %)]}
  (reduce (fn [fs fsyn]
            {:pre [(every? parsed-free-map? fs)]
             :post [(every? parsed-free-map? %)]}
            ;(prn "parse-free-binder-with-variance" (map :fname fs))
            (conj fs
                  (let [opts (free-ops/with-bounded-frees opts
                               (mapv :fname fs)
                               (mapv :bnd fs))]
                    (parse-free-with-variance fsyn opts))))
          [] binder))

; parsing All binders
;return a vector of [name bnds]
(defn parse-free [f default-kind opts]
  {:post [((con/hvector-c? symbol? r/Kind?) %)]}
  (let [validate-sym (fn [s]
                       (when-not (symbol? s)
                         (prs-error (str "Type variable must be a symbol, given: " (pr-str s)) opts))
                       (when (namespace s)
                         (prs-error (str "Type variable must not be namespace qualified: " (pr-str s)) opts))
                       (when (.contains (name s) ".")
                         (prs-error (str "Type variable must not contain dots (.): " (pr-str s)) opts))
                       (when (#{"true" "nil" "false"} (name s))
                         (prs-error (str "Type variable must not be named true, false, or nil: " (pr-str s)) opts)))]
    (if (symbol? f)
      (do (validate-sym f)
          [f (case default-kind
               :type r/no-bounds
               :dotted r/dotted-no-bounds)])
      (let [[n & popts] f
            _ (when-not (even? (count popts))
                (prs-error (str "Uneven number of options in type variable binder: " (pr-str popts)) opts))
            {:keys [< > kind] :as popts} popts
            ks (set (keys kind))
            kind? (:kind ks)]
        (when kind?
          (when ((some-fn ks) :< :>)
            (prs-error "Cannot combine :kind with :< or :> in type variable binder" opts)))
        (validate-sym n)
        (when (:variance popts)
          (prs-error "Variance not supported for variables introduced with All" opts))
        [n (if kind?
             (parse-type (:kind ks) opts)
             (let [upper-or-nil (when (contains? popts :<)
                                  (parse-type < opts))
                   lower-or-nil (when (contains? popts :>)
                                  (parse-type > opts))]
               (c/infer-bounds upper-or-nil lower-or-nil opts)))]))))

(defn check-forbidden-rec [rec tbody opts]
  (letfn [(well-formed? [t]
            (and (not= rec t)
                 (if ((some-fn r/Intersection? r/Union?) t)
                   (every? well-formed? (:types t))
                   true)))]
    (when-not (well-formed? tbody)
      (prs-error (str "Recursive type not allowed here") opts))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec & [[free-symbol :as bnder] type :as args]] opts]
  (when-not (= 1 (count bnder))
    (prs-error "Rec type requires exactly one entry in binder" opts))
  (when-not (= 2 (count args))
    (prs-error "Wrong arguments to Rec" opts))
  (let [Mu* @(Mu*-var)
        _ (when-not (= 1 (count bnder)) 
            (prs-error "Only one variable allowed: Rec" opts))
        _ (when-not (simple-symbol? free-symbol)
            (prs-error (str "Binder for Rec must contain simple symbol: " (pr-str bnder)) opts))
        f (r/make-F free-symbol)
        opts (free-ops/with-bounded-frees opts {f r/no-bounds})
        body (parse-type type opts)
        _ (check-forbidden-rec f body opts)]
    (Mu* (:name f) body opts)))

(defn parse-CountRange [[_ & [n u :as args]] opts]
  (when-not (<= 1 (count args) 2)
    (prs-error "Wrong arguments to CountRange" opts))
  (when-not (integer? n)
    (prs-error "First argument to CountRange must be an integer" opts))
  (when-not (or (= 1 (count args))
                (integer? u))
    (prs-error "Second argument to CountRange must be an integer" opts))
  (r/make-CountRange n u))

(defmethod parse-type-list 'typed.clojure/CountRange [t opts] (parse-CountRange t opts))

(defn uniquify-local [sym {::vs/keys [current-expr] :as opts}]
  (get-in current-expr [:env ::uniquify/locals-frame-val sym]))

(defmethod parse-type-list 'typed.clojure/TypeOf [[_ sym :as t] opts]
  (impl/assert-clojure opts)
  (when-not (= 2 (count t))
    (prs-error (str "Wrong number of arguments to TypeOf (" (count t) ")") opts))
  (when-not (symbol? sym)
    (prs-error "Argument to TypeOf must be a symbol." opts))
  (let [uniquified-local (uniquify-local sym opts)
        vsym (let [r (resolve-type-clj sym opts)]
               (when (var? r)
                 (coerce/var->symbol r)))]
    (if uniquified-local
      (let [t (ind/type-of-nofail uniquified-local opts)]
        (when-not t
          (prs-error (str "Could not resolve TypeOf for local " sym) opts))
        t)
      (do
        (when-not vsym
          (prs-error (str "Could not resolve TypeOf for var " sym) opts))
        (r/-type-of vsym)))))

(defn parse-ExactCount [[_ & [n :as args]] opts]
  (when-not (= 1 (count args))
    (prs-error "Wrong arguments to ExactCount" opts))
  (when-not (integer? n)
    (prs-error "First argument to ExactCount must be an integer" opts))
  (r/make-ExactCountRange n))

(defmethod parse-type-list 'typed.clojure/ExactCount [t opts] (parse-ExactCount t opts))

(defn- RClass-of-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.type-ctors) 'RClass-of)]
    (assert (var? v) "RClass-of unbound")
    v))

(defn predicate-for [on-type opts]
  (r/make-FnIntersection
    (r/make-Function [r/-any] (impl/impl-case opts
                                :clojure (r/Name-maker 'java.lang.Boolean)
                                :cljs    (r/JSBoolean-maker))
                     :filter (fl/-FS (fl/-proposition on-type 0)
                                     (fl/-not-proposition on-type 0)))))

(defn parse-Pred [[_ & [t-syn :as args]] opts]
  (when-not (= 1 (count args))
    (prs-error "Wrong arguments to predicate" opts))
  (predicate-for (parse-type t-syn opts) opts))

(defmethod parse-type-list 'typed.clojure/Pred [t opts] (parse-Pred t opts))

(defn parse-Not [[_ tsyn :as all] opts]
  (when-not (= (count all) 2) 
    (prs-error (str "Wrong arguments to Not (expected 1): " all) opts))
  (r/NotType-maker (parse-type tsyn opts)))
(defmethod parse-type-list 'typed.clojure/Not [frm opts] (parse-Not frm opts))

(defn parse-Difference [[_ tsyn & dsyns :as all] opts]
  (when-not (<= 3 (count all))
    (prs-error (str "Wrong arguments to Difference (expected at least 2): " all) opts))
  (c/make-Intersection (cons (parse-type tsyn opts)
                             (doall (map #(r/NotType-maker (parse-type % opts)) dsyns)))
                       opts))

(defmethod parse-type-list 'typed.clojure/Difference [t opts] (parse-Difference t opts))

(defmethod parse-type-list 'typed.clojure/Rec [syn opts] (parse-rec-type syn opts))

(defn parse-Assoc [[_ tsyn & entries :as all] opts]
  (when-not (<= 1 (count (next all)))
    (prs-error (str "Wrong arguments to Assoc: " all) opts))
  (let [{ellipsis-pos :..}
        (zipmap entries (range))

        [entries dentries] (split-at (if ellipsis-pos
                                       (dec ellipsis-pos)
                                       (count entries))
                                     entries)

        [drest-type _ drest-bnd] (when ellipsis-pos
                                   dentries)

        _ (when-not (-> entries count even?)
            (prs-error (str "Incorrect Assoc syntax: " all " , must have even number of key/val pair.") opts))

        _ (when-not (or (not ellipsis-pos)
                        (= (count dentries) 3))
            (prs-error (str "Incorrect Assoc syntax: " all " , Dotted rest entry must be 3 entries") opts))

        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (prs-error "Dotted bound must be symbol" opts))]
  (r/AssocType-maker (parse-type tsyn opts)
                     (into []
                           (comp (map #(parse-type % opts))
                                 (partition-all 2))
                           entries)
                     (when ellipsis-pos
                       (let [bnd (free-ops/free-in-scope-bnds drest-bnd opts)
                             f (free-ops/free-in-scope drest-bnd opts)
                             _ (when-not (r/Regex? bnd)
                                 (prs-error (str (pr-str drest-bnd) " is not in scope as a dotted variable") opts))
                             _ (assert (r/F? f))]
                         (r/DottedPretype1-maker
                           ;with dotted bound in scope as free
                           (let [opts (free-ops/with-bounded-frees opts
                                        {(r/make-F drest-bnd)
                                         ((requiring-resolve 'typed.cljc.checker.cs-gen/homogeneous-dbound->bound)
                                          bnd opts)})]
                             (parse-type drest-type opts))
                           (:name f)))))))

(defmethod parse-type-list 'typed.clojure/Assoc [t opts] (parse-Assoc t opts))

(defn parse-Merge [[_ & tsyns] opts]
  (r/MergeType-maker (mapv #(parse-type % opts) tsyns)))

(defmethod parse-type-list 'typed.clojure/Merge [t opts] (parse-Merge t opts))

(defn parse-Instance [[_ tsyn :as all] opts]
  (impl/assert-clojure opts)
  (when-not (= 2 (count all))
    (prs-error (str "Wrong arguments to t/Instance: " (pr-str all)) opts))
  (when-not (simple-symbol? tsyn)
    (prs-error (str "t/Instance must be passed a simple symbol, given: " (pr-str tsyn)) opts))
  (let [^#?(:cljr Type :default Class) cls (resolve-type-clj tsyn opts)]
    (when-not (class? cls)
      (prs-error (str "t/Instance must resolve to a class: " (pr-str tsyn)) opts))
    (-> cls #?(:cljr .FullName :default .getName) symbol r/Instance-maker)))

(defmethod parse-type-list 'typed.clojure/Instance [t opts] (parse-Instance t opts))

(defn parse-Satisfies [[_ tsyn :as all] opts]
  (impl/assert-clojure opts) ;; TODO resolve protocol in cljs
  (when-not (= 2 (count all))
    (prs-error (str "Wrong arguments to t/Satisfies: " (pr-str all)) opts))
  (when-not (symbol? tsyn)
    (prs-error (str "t/Satisfies must be passed a symbol, given: " (pr-str tsyn)) opts))
  (let [protocol-var (resolve-type-clj tsyn opts)
        _ (when-not (var? protocol-var)
            (prs-error (str "t/Satisfies must be passed a symbol naming a protocol: "
                            tsyn " resolves to " (pr-str protocol-var)) opts))]
    ((requiring-resolve 'typed.cljc.checker.type-ctors/Protocol-with-unknown-params)
     (symbol protocol-var)
     opts)))

(defmethod parse-type-list 'typed.clojure/Satisfies [t opts] (parse-Satisfies t opts))

(defn parse-Get [[_ tsyn keysyn not-foundsyn :as all] opts]
  (when-not (#{2 3} (count (next all)))
    (prs-error (str "Wrong arguments to Get: " all) opts))
  (r/-get (parse-type tsyn opts)
          (parse-type keysyn opts)
          :not-found
          (when (= 3 (count (next all)))
            (parse-type not-foundsyn opts))))

(defmethod parse-type-list 'typed.clojure/Get [t opts] (parse-Get t opts))

; convert flattened kw arguments to vectors
#_
(defn normalise-binder [bnds opts]
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
                                                      " has no associated value") opts))
                                  [k v & rst] bnds]
                              (recur rst
                                     (conj out k v)))
                            [bnds out]))]
        (recur rst
               (conj out group))))))

(defn parse-normal-binder [bnds opts]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (con/hvector-c? symbol? r/Kind?) %)]}
                  (conj fs
                        (let [opts (free-ops/with-bounded-frees opts
                                     (into {}
                                           (map (fn [[n bnd]] [(r/make-F n) bnd]))
                                           fs))]
                          (parse-free fsyn :type opts))))
                [] bnds)]
    [frees-with-bnds nil]))

(defn parse-dotted-binder [bnds opts]
  {:pre [(vector? bnds)]}
  (let [[frees-with-bnds] (parse-normal-binder (-> bnds pop pop) opts)
        dvar (parse-free (-> bnds pop peek) :dotted opts)]
    [frees-with-bnds dvar]))

(defn parse-unknown-binder [bnds opts]
  {:pre [((some-fn nil? vector?) bnds)]}
  (when bnds
    (when (#{:... '...} (peek bnds))
      (prs-error (str (peek bnds) " syntax has changed to :..") opts))
    ((if (= :.. (peek bnds))
       parse-dotted-binder
       parse-normal-binder)
     bnds
     opts)))

(defn parse-All-binder [bnds opts]
  {:pre [(vector? bnds)]}
  (let [[positional kwargs] (split-with (complement keyword?) bnds)
        ;; allow trailing :.. in positional vars before kw args
        _ (when (= :... (first kwargs))
            (prs-error ":... syntax has changed to :.." opts))
        [positional kwargs] (if (= :.. (first kwargs))
                              [(conj (vec positional) (first kwargs))
                               (next kwargs)]
                              [(vec positional) kwargs])
        _ (when (#{:... '...} (peek positional))
            (prs-error (str (peek positional) " syntax has changed to :..") opts))
        dotted? (= :.. (peek positional))
        _ (when-not (even? (count kwargs))
            (prs-error (str "Expected an even number of keyword options to All, given: " (vec kwargs)) opts))
        _ (when (seq kwargs)
            (when-not (apply distinct? (map first (partition 2 kwargs)))
              (prs-error (str "Gave repeated keyword args to All: " (vec kwargs)) opts)))
        {:keys [named] :as kwargs} kwargs
        _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
            (when (seq unsupported)
              (prs-error (str "Unsupported keyword argument(s) to All: " unsupported) opts)))
        _ (when (contains? kwargs :named)
            (when-not (and (vector? named)
                           (every? symbol? named))
              (prs-error (str ":named keyword argument to All must be a vector of symbols, given: " (pr-str named)) opts)))
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
              (prs-error (str "Variables bound by All must be unique, given: " no-dots) opts)))
        named-map (let [sym-to-pos (into {}
                                         (map-indexed #(vector %2 %1))
                                         no-dots)]
                    (select-keys sym-to-pos named))
        ;; TODO 
        ;; update usages of parse-unknown-binder to use parse-All-binder
        [frees-with-bnds dvar] ((if dotted?
                                  parse-dotted-binder
                                  parse-normal-binder)
                                bnds*
                                opts)]
    {:frees-with-bnds frees-with-bnds
     :dvar dvar
     :named named-map}))

(defn parse-all-type*
  [{:keys [frees-with-bnds dvar named] :as _parsed-binder} type opts]
  ;(prn "parse-all-type*" type)
  (let [bfs (into {}
                  (map (fn [[n bnd]] [(r/make-F n) bnd]))
                  (cond-> frees-with-bnds
                    dvar (conj dvar)))
        opts (free-ops/with-bounded-frees opts bfs)
        body (parse-type type opts)]
    (if dvar
      (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                   (map second (concat frees-with-bnds [dvar]))
                   body
                   {:named named}
                   opts)
      (c/Poly* (map first frees-with-bnds)
               (map second frees-with-bnds)
               body
               {:named named}
               opts))))

;dispatch on last element of syntax in binder
(defn parse-all-type [bnds type opts]
  (when-not (vector? bnds)
    (prs-error (str "First argument to t/All must be a vector: " (pr-str bnds)) opts))
  (parse-all-type* (parse-All-binder bnds opts) type opts))

(defmethod parse-type-list 'Extends
  [[_ extends & {:keys [without] :as popts} :as syn] opts]
  (when-not (empty? (set/difference (set (keys popts)) #{:without}))
    (prs-error (str "Invalid options to Extends:" (keys popts)) opts))
  (when-not (vector? extends) 
    (prs-error (str "Extends takes a vector of types: " (pr-str syn)) opts))
  (c/make-Intersection (concat (doall (map #(parse-type % opts) extends))
                               (doall (map #(r/NotType-maker (parse-type % opts)) without)))
                       opts))

(defn parse-All [[_All_ bnds syn & more :as all] opts]
  ;(prn "All syntax" all)
  (when more
    (prs-error (str "Bad All syntax: " all) opts))
  (parse-all-type bnds syn opts))

(defmethod parse-type-list 'typed.clojure/All [t opts] (parse-All t opts))

(defn parse-Match [[_ target & patterns :as all] opts]
  (when-not (< 1 (count all))
    (prs-error "Missing target type in t/Match" opts))
  (let [patterns (loop [patterns patterns
                        out []]
                   (if (empty? patterns)
                     out
                     (let [[pattern-before-=> =>+rhs+patterns] (split-with (complement #{:->}) patterns)
                           _ (when (empty? pattern-before-=>)
                               (prs-error "Missing pattern before :-> in t/Match clause" opts))
                           _ (when (empty? =>+rhs+patterns)
                               (prs-error "Missing :-> in t/Match clause" opts))
                           rhs+patterns (next =>+rhs+patterns)
                           _ (when-not rhs+patterns
                               (prs-error "Missing type after :-> in t/Match" opts))
                           rhs (first rhs+patterns)
                           patterns (next rhs+patterns)
                           t (case (count pattern-before-=>)
                               1 (let [[pattern] pattern-before-=>]
                                   (parse-type [pattern :-> rhs] opts))
                               2 (let [[binder pattern] pattern-before-=>]
                                   (when-not (vector? binder)
                                     (prs-error "Binder before :=> must be a vector" opts))
                                   (parse-all-type* (parse-All-binder binder opts) [pattern :-> rhs] opts))
                               (prs-error "Bad number of arguments before :=> in t/Match" opts))]
                       (recur patterns (conj out t)))))]
    (r/MatchType-maker (parse-type target opts) patterns)))

(defmethod parse-type-list 'typed.clojure/Match [t opts] (parse-Match t opts))

(defn parse-union-type [[u & types] opts]
  (c/make-Union (doall (map #(parse-type % opts) types)) opts))

(defmethod parse-type-list 'typed.clojure/U [syn opts] (parse-union-type syn opts))

; don't do any simplification of the intersection because some types might
; not be resolved
(defn parse-intersection-type [[i & types] opts]
  (c/make-Intersection (map #(parse-type % opts) types) opts))

(defmethod parse-type-list 'typed.clojure/I [syn opts] (parse-intersection-type syn opts))

(defn parse-Array
  [[_ syn & none] opts]
  (when-not (empty? none)
    (prs-error "Expected 1 argument to Array" opts))
  (let [t (parse-type syn opts)]
    (impl/impl-case opts
      :clojure (let [jtype (if (r/RClass? t)
                             (r/RClass->Class t)
                             Object)]
                 (r/PrimitiveArray-maker jtype t t))
      :cljs (r/ArrayCLJS-maker t t))))

(defmethod parse-type-list 'Array [syn opts] (parse-Array syn opts))
(defmethod parse-type-list 'cljs.core.typed/Array [syn opts] (parse-Array syn opts))

(defn parse-ReadOnlyArray
  [[_ osyn & none] opts]
  (when-not (empty? none) 
    (prs-error "Expected 1 argument to ReadOnlyArray" opts))
  (let [o (parse-type osyn opts)]
    (impl/impl-case opts
      :clojure (r/PrimitiveArray-maker Object (r/Bottom) o)
      :cljs (r/ArrayCLJS-maker (r/Bottom) o))))

(defmethod parse-type-list 'ReadOnlyArray [syn opts] (parse-ReadOnlyArray syn opts))
(defmethod parse-type-list 'cljs.core.typed/ReadOnlyArray [syn opts] (parse-ReadOnlyArray syn opts))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none] opts]
  (when-not (empty? none) 
    (prs-error "Expected 2 arguments to Array2" opts))
  (let [i (parse-type isyn opts)
        o (parse-type osyn opts)]
    (impl/impl-case opts
      :clojure (r/PrimitiveArray-maker Object i o)
      :cljs (r/ArrayCLJS-maker i o))))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none] opts]
  (impl/assert-clojure opts)
  (when-not (empty? none) 
    (prs-error "Expected 3 arguments to Array3" opts))
  (let [jrclass (c/fully-resolve-type (parse-type jsyn opts) opts)
        _ (when-not (r/RClass? jrclass) 
            (prs-error "First argument to Array3 must be a Class" opts))]
    (r/PrimitiveArray-maker (r/RClass->Class jrclass) (parse-type isyn opts) (parse-type osyn opts))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types] opts]
  (r/FnIntersection-maker (into [] (mapcat #(parse-function % opts)) types)))

(defn parse-Fn [[_ & types :as syn] opts]
  (when-not (seq types) 
    (prs-error (str "Must pass at least one arity to Fn: " (pr-str syn)) opts))
  (when-not (every? vector? types) 
    (prs-error (str "Fn accepts vectors, given: " (pr-str syn)) opts))
  (parse-fn-intersection-type syn opts))

(defmethod parse-type-list 'Fn [t opts]
  (err/deprecated-plain-op 'Fn 'IFn opts)
  (parse-Fn t opts))
(defmethod parse-type-list 'typed.clojure/IFn [t opts] (parse-Fn t opts))

(defn parse-free-binder [[nme & {:keys [variance < > kind] :as popts}] opts]
  (when kind
    (prs-error ":kind not yet implemented" opts))
  (when-not (symbol? nme)
    (prs-error "First entry in free binder should be a name symbol" opts))
  {:nme nme :variance (or variance :invariant)
   :bound (r/-bounds
            ;upper
            (if (contains? popts :<)
              (parse-type < opts)
              r/-any)
            ;lower
            (if (contains? popts :>)
              (parse-type > opts)
              r/-nothing))})

(defn parse-tfn-binder [binder opts]
  {:pre [((some-fn symbol? vector?) binder)]
   :post [((con/hmap-c? :nme symbol? :variance (some-fn r/variance? #(= :infer %))
                        :bound r/Kind?)
           %)]}
  (let [[nme & opts-flat :as all] (cond-> binder
                                    (symbol? binder) vector)
        _ (when-not (symbol? nme)
            (prs-error "Must provide a name symbol to TFn" opts))
        _ (when-not (even? (count opts-flat))
            (prs-error (str "Uneven binder element passed to TFn binder: "
                            (pr-str binder)) opts))
        {:keys [variance < >]
         :or {variance :infer}
         :as popts}
        (apply hash-map opts-flat)]
    (when-some [extra-keys (not-empty (disj (-> popts keys set) :variance :< :>))]
      (prs-error (str "Unknown t/TFn option: " (pr-str (first extra-keys))
                      ". Known options are :variance, :<, and :>.") opts))
    (when (contains? popts :kind)
      (err/deprecated-warn "Kind annotation for TFn parameters" opts))
    (when-some [[_ provided] (find popts :variance)]
      (when-not (r/variance? provided)
        (prs-error (str "Invalid variance: " (pr-str provided)) opts)))
    {:nme nme :variance variance
     :bound (let [upper-or-nil (when (contains? popts :<)
                                 (parse-type < opts))
                  lower-or-nil (when (contains? popts :>)
                                 (parse-type > opts))]
              (c/infer-bounds upper-or-nil lower-or-nil opts))}))

(defn parse-type-fn
  [[_ binder bodysyn :as tfn] opts]
  (when-not (= 3 (count tfn))
    (prs-error (str "Wrong number of arguments to TFn: " (pr-str tfn)) opts))
  (let [;; variable bounds has all variables to the left of it in scope
        {:keys [free-maps]} (reduce (fn [{:keys [free-maps free-symbs]} binder]
                                      (when-not ((some-fn symbol? vector?) binder)
                                        (prs-error (str "TFn binder element must be a symbol or vector: " (pr-str binder)) opts))
                                      (let [opts (free-ops/with-free-symbols opts free-symbs)]
                                        {:free-maps (conj free-maps (parse-tfn-binder binder opts))
                                         :free-symbs (conj free-symbs (cond-> binder
                                                                        (vector? binder) first))}))
                                    {:free-maps []
                                     :free-symbs #{}}
                                    binder)
        bodyt (let [opts (free-ops/with-bounded-frees opts
                           (into {}
                                 (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound]))
                                 free-maps))]
                (parse-type bodysyn opts))
        ;; at some point we should remove this requirement (this is checked by parse-tfn-binder)
        id (gensym "currently-inferring-TypeFns")
        infer-variances? (some #(= :infer (:variance %)) free-maps)
        variances (fn []
                    (let [currently-inferring-TypeFns vs/*currently-inferring-TypeFns*]
                      (if (currently-inferring-TypeFns id)
                        (if infer-variances?
                          (prs-error "Cannot infer variances on recursive t/TFn, please add :variance annotations" opts)
                          {:cache false
                           :variances (map :variance free-maps)})
                        (binding [vs/*currently-inferring-TypeFns* (conj currently-inferring-TypeFns id)]
                          (let [vs (let [opts (free-ops/with-bounded-frees opts
                                                (into {} (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound]))
                                                      free-maps))]
                                     ((requiring-resolve 'typed.cljc.checker.frees/fv-variances) bodyt opts))]
                            {:cache true
                             :variances (mapv (fn [{:keys [nme variance]}]
                                                (if-some [actual-v (vs nme)]
                                                  (if (and variance
                                                           (not= :infer variance)
                                                           (not= actual-v variance))
                                                    (prs-error (str "Type variable " nme " occurs with " (name actual-v) " variance "
                                                                    "when declared " (name variance)) opts)
                                                    actual-v)
                                                  ;;not great support for :constant, use :invariant for now
                                                  ;:constant
                                                  :invariant))
                                              free-maps)})))))]
    (c/TypeFn* (map :nme free-maps)
               variances
               (map :bound free-maps)
               bodyt
               {:meta {:env (::vs/current-env opts)}}
               opts)))

(defmethod parse-type-list 'typed.clojure/TFn [syn opts] (parse-type-fn syn opts))

(defn parse-Type-kind [syn opts]
  (cond
    (symbol? syn) r/no-bounds
    (seq? syn) (if-some [popts (next syn)]
                 (let [_ (when (odd? (count popts))
                           (prs-error "Uneven arguments passed to t/Type" opts))
                       popts (partition 2 popts)
                       _ (when-not (apply distinct? (map first popts))
                           (prs-error "Duplicate keys passed to t/Type" opts))
                       popts (apply hash-map popts)
                       _ (when (not-every? keyword? (keys popts))
                           (prs-error "Non-keyword key passed to t/Type" opts))
                       _ (when-some [extra-keys (disj (set (keys popts)) :< :>)]
                           (prs-error (str "Unknown keys passed to t/Type: "
                                           (str/join ", " (sort extra-keys))) opts))]
                   (r/-bounds (if-some [[_ upper] (find popts :<)]
                                (parse-type upper opts)
                                r/-any)
                              (if-some [[_ lower] (find popts :>)]
                                (parse-type lower opts)
                                r/-nothing)))
                 r/no-bounds)
    :else (err/int-error (str "unparseable: " syn) opts)))

(defmethod parse-type-list 'typed.clojure/Type [syn opts] (parse-Type-kind syn opts))
(defmethod parse-type-symbol 'typed.clojure/Type [syn opts] (parse-Type-kind syn opts))

;; parse-HVec, parse-HSequential and parse-HSeq have many common patterns
;; so we reuse them
(defn parse-types-with-rest-drest [err-msg]
  (fn [syns opts]
    (let [_ (when-not (seqable? syns)
              (prs-error (str err-msg " " (pr-str syns)) opts))
          syns (vec syns)
          rest? (#{:* '* :+} (peek syns))
          dotted? (and (#{:... :.. '...} (some-> (not-empty syns) pop peek))
                       (<= 3 (count syns)))
          _ (when (and rest? dotted?)
              (prs-error (str err-msg syns) opts))
          {:keys [fixed rest drest]}
          (cond
            rest?
            (let [fixed (mapv #(parse-type % opts) (-> syns pop pop))
                  rest (parse-type (-> syns pop peek) opts)]
              {:fixed (cond-> fixed
                        (= :+ rest?) (conj rest))
               :rest rest})
            dotted?
            (let [fixed (mapv #(parse-type % opts) (-> syns pop pop pop))
                  [drest-type _dots_ drest-bnd :as dot-syntax] (take-last 3 syns)
                  ; should never fail, if the logic changes above it's probably
                  ; useful to keep around.
                  _ (when-not (= 3 (count dot-syntax))
                      (prs-error (str "Bad vector syntax: " dot-syntax) opts))
                  bnd (free-ops/free-in-scope-bnds drest-bnd opts)
                  f (free-ops/free-in-scope drest-bnd opts)
                  _ (when-not (r/Regex? bnd)
                      (prs-error (str (pr-str drest-bnd) " is not in scope as a dotted variable") opts))]
              {:fixed fixed
               :drest (r/DottedPretype1-maker
                        ;with dotted bound in scope as free
                        (let [opts (free-ops/with-bounded-frees opts
                                     {(r/make-F drest-bnd)
                                      ((requiring-resolve 'typed.cljc.checker.cs-gen/homogeneous-dbound->bound)
                                       bnd opts)})]
                          (parse-type drest-type opts))
                        (:name f))})
            :else {:fixed (mapv #(parse-type % opts) syns)})]
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

(declare parse-object parse-proposition-set)

(defn parse-heterogeneous* [parse-h*-types constructor]
  (fn [[_ syn & {:keys [proposition-sets objects repeat] :or {repeat false}}] opts]
    (when-not (boolean? repeat)
      (prs-error (str ":repeat must be boolean") opts))
    (let [{:keys [fixed drest rest]} (parse-h*-types syn opts)]
      (constructor fixed
                   {:filters (some->> proposition-sets (mapv #(parse-proposition-set % opts)))
                    :objects (some->> objects (mapv #(parse-object % opts)))
                    :drest drest
                    :rest rest
                    :repeat (true? repeat)}
                   opts))))

(def parse-HVec (parse-heterogeneous* parse-hvec-types #'r/-hvec))
(def parse-HSequential (parse-heterogeneous* parse-hsequential-types #'r/-hsequential))
(def parse-HSeq (parse-heterogeneous* parse-hseq-types #'r/-hseq))
(def parse-HList (parse-heterogeneous* parse-hseq-types (comp #(assoc % :kind :list)
                                                              #'r/-hsequential)))

(defmethod parse-type-list 'typed.clojure/HVec [t opts] (parse-HVec t opts))
(defmethod parse-type-list 'typed.clojure/HSequential [t opts] (parse-HSequential t opts))
(defmethod parse-type-list 'typed.clojure/HSeq [t opts] (parse-HSeq t opts))
(defmethod parse-type-list 'typed.clojure/HList [t opts] (parse-HList t opts))

(defn parse-HSet [[_ ts & {:keys [complete?] :or {complete? true}} :as args] opts]
  (let [bad (seq (remove hset/valid-fixed? ts))]
    (when-not (boolean? complete?)
      (prs-error (str "HSet's :complete? must be boolean") opts))
    (when bad
      (prs-error (str "Bad arguments to HSet: " (pr-str bad)) opts))
    (r/-hset (into #{} (map r/-val) ts)
             :complete? complete?)))

(defmethod parse-type-list 'typed.clojure/HSet [t opts] (parse-HSet t opts))

(defn- syn-to-hmap [mandatory optional absent-keys complete? opts]
  (when mandatory
    (when-not (map? mandatory)
      (prs-error (str "Mandatory entries to HMap must be a map: " mandatory) opts)))
  (when optional
    (when-not (map? optional)
      (prs-error (str "Optional entries to HMap must be a map: " optional) opts)))
  (letfn [(mapt [m]
            (reduce-kv (fn [m k v]
                         (assoc m (r/-val k) (parse-type v opts)))
                       {} m))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (prs-error (str "HMap options contain duplicate key entries: "
                              "Mandatory: " (into {} mandatory) ", Optional: " (into {} optional) 
                              ", Absent: " (set absent-keys)) opts))
          _ (when-not (every? keyword? (keys mandatory)) (prs-error "HMap's mandatory keys must be keywords" opts))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (prs-error "HMap's optional keys must be keywords" opts))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (prs-error "HMap's absent keys must be keywords" opts))
          absent-keys (into #{} (map r/-val) absent-keys)]
      (c/make-HMap opts {:mandatory mandatory :optional optional 
                         :complete? complete? :absent-keys absent-keys}))))

(defn parse-quoted-hvec [syn opts]
  (let [{:keys [fixed drest rest]} (parse-hvec-types syn opts)]
    (r/-hvec fixed
             {:drest drest
              :rest rest}
             opts)))

(defn quoted? [v]
  (and (seq? v)
       (= 2 (count v))
       (= 'quote (first v))))

(defmethod parse-type-list 'quote 
  [[_ syn :as all] opts]
  (when-not (quoted? all)
    (prs-error (str "Incorrect use of quote: " (pr-str all)) opts))
  (cond
    (-> all meta :fake-quote) (parse-type syn opts)
    ((some-fn number? keyword? symbol? string?) syn) (r/-val syn)
    (vector? syn) (parse-quoted-hvec syn opts)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false opts)
    :else (prs-error (str "Invalid use of quote: " (pr-str syn)) opts)))

(defn allow-regex [t]
  (cond-> t
    (instance? clojure.lang.IObj t)
    (vary-meta assoc ::allow-regex true)))

(defn regex-allowed? [t]
  (-> t meta ::allow-regex boolean))

(defn multi-frequencies 
  "Like frequencies, but only returns frequencies greater
  than one"
  [coll]
  (into {}
        (filter (fn [[_ freq]] (< 1 freq)))
        (frequencies coll)))

(defn parse-HMap [[_HMap_ & flat-opts :as all] opts]
  (let [supported-options #{:optional :mandatory :absent-keys :complete?}
        ; support deprecated syntax (HMap {}), which is now (HMap :mandatory {})
        deprecated-mandatory (when (map? (first flat-opts))
                               (err/deprecated-warn
                                 "(HMap {}) syntax has changed, use (HMap :mandatory {})" opts)
                               (first flat-opts))
        flat-opts (cond-> flat-opts
                    deprecated-mandatory next)
        _ (when-not (even? (count flat-opts))
            (prs-error (str "Uneven keyword arguments to HMap: " (pr-str all)) opts))
        flat-keys (sequence
                    (comp (partition-all 2)
                          (map first))
                    flat-opts)
        _ (when-not (every? keyword? flat-keys)
            (prs-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                            #_#_" in: " (pr-str all)) opts))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (prs-error (str "Repeated keyword argument to HMap: " (pr-str k)) opts)))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (prs-error (str "Unsupported HMap keyword argument: " (pr-str k)) opts))
        _ (when (and deprecated-mandatory mandatory)
            (prs-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap") opts))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete? opts)))

(defmethod parse-type-list 'HMap [t opts] (parse-HMap t opts))
(defmethod parse-type-list 'typed.clojure/HMap [t opts] (parse-HMap t opts))

(defn parse-JSObj [[_JSObj_ types :as all] opts]
  (let [_ (when-not (= 2 (count all))
            (prs-error (str "Bad syntax to JSObj: " (pr-str all)) opts))
        _ (when-not (every? keyword? (keys types))
            (prs-error (str "JSObj requires keyword keys, given " (pr-str (class (first (remove keyword? (keys types)))))) opts))
        parsed-types (update-vals types #(parse-type % opts))]
    (r/JSObj-maker parsed-types)))

(defmethod parse-type-list 'typed.clojure/JSObj [t opts] (parse-JSObj t opts))

(defn parse-in-ns [{::keys [parse-type-in-ns] :as opts}]
  {:post [(symbol? %)]}
  (or parse-type-in-ns
      (assert nil "parse-type-in-ns unbound")
      (impl/impl-case opts
        :clojure (ns-name *ns*)
        :cljs (cljs-ns))))

(def ns-rewrites-clj {'clojure.core.typed 'typed.clojure})
(def ^:private ns-unrewrites-clj (set/map-invert ns-rewrites-clj))

(defn -resolve-type-clj
  "Returns a var, class or nil"
  [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn var? class? nil?) %)]}
  (impl/assert-clojure opts)
  (let [nsym (parse-in-ns opts)]
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
      (err/int-error (str "Cannot find namespace: " sym) opts))))

(defn resolve-type-clj
  "Returns a var, class or nil"
  [sym {::keys [resolve-type-clj]
        :or {resolve-type-clj -resolve-type-clj}
        :as opts}]
  (resolve-type-clj sym opts))

(defn- resolve-type-alias-clj
  "Returns a symbol if sym maps to a type alias, otherwise nil"
  [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol? nil?) %)]}
  (impl/assert-clojure opts)
  (let [nsym (parse-in-ns opts)
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
          (when (contains? (nme-env/name-env (env/checker opts)) qsym)
            qsym)))
      (err/int-error (str "Cannot find namespace: " sym) opts))))

(defn -resolve-type-clj->sym
  [sym opts]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  (impl/assert-clojure opts)
  (let [nsym (parse-in-ns opts)]
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
      (err/int-error (str "Cannot find namespace: " nsym) opts))))

(defn resolve-type-clj->sym
  [sym {::keys [resolve-type-clj->sym]
        :or {resolve-type-clj->sym -resolve-type-clj->sym}
        :as opts}]
  (resolve-type-clj->sym sym opts))

(def ^:private ns-rewrites-cljs {'cljs.core.typed 'typed.clojure})
(def ^:private ns-unrewrites-cljs (set/map-invert ns-rewrites-cljs))

;; ignores both clj and cljs namespace graph (other than to resolve aliases)
;; TODO reconcile clj/cljs type resolution. neither should really be interning vars (breaking change for clj).
(defn resolve-type-cljs
  "Returns a qualified symbol or nil"
  [sym opts]
  {:pre [(symbol? sym)]
   :post [((some-fn symbol?
                    nil?)
           %)]}
  (impl/assert-cljs opts)
  (let [nsym (parse-in-ns opts)
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

(defn parse-Value [[_Value_ syn :as all] opts]
  (when-not (= 2 (count all))
    (prs-error (str "Incorrect number of arguments to Value, " (count all)
                    ", expected 2: " all) opts))
  (impl/impl-case opts
    :clojure (const/constant-type syn false opts)
    :cljs (cond
            ((some-fn symbol? keyword? nil?) syn)
              (r/-val syn)
            :else (assert nil (str "FIXME CLJS parse Value: " (pr-str syn))))))

(defmethod parse-type-list 'typed.clojure/Val [t opts] (parse-Value t opts))
(defmethod parse-type-list 'typed.clojure/Value [t opts] (parse-Value t opts))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}] opts]
  (when-not (= #{}
               (set/intersection (set (keys optional))
                                 (set (keys mandatory))))
    (prs-error (str "Optional and mandatory keyword arguments should be disjoint: "
                    (set/intersection (set (keys optional))
                                      (set (keys mandatory)))) opts))
  (let [optional (into {}
                       (map (fn [[k v]]
                              (do (when-not (keyword? k) (prs-error (str "Keyword argument keys must be keywords: " (pr-str k)) opts))
                                  [(r/-val k) (parse-type v opts)])))
                       optional)
        mandatory (into {}
                        (map (fn [[k v]]
                               (do (when-not (keyword? k) (prs-error (str "Keyword argument keys must be keywords: " (pr-str k)) opts))
                                   [(r/-val k) (parse-type v opts)])))
                        mandatory)]
    (c/Un (for [mopts (map #(into {} %) (comb/subsets optional))
                :let [m (into mandatory mopts)
                      kss (comb/permutations (keys m))]
                ks kss
                k (r/-hseq (mapcat #(find m %) ks) {} opts)]
            k)
          opts)))

(declare unparse-type)

(defn parse-type-list-default 
  [[n & args :as syn] opts]
  (let [op (parse-type n opts)]
    ;(prn "tapp op" op)
    (when-not ((some-fn r/Name? r/TypeFn? r/F? r/B? r/Poly?) op)
      (prs-error (str "Invalid operator to type application: " syn) opts))
    (with-meta (r/TApp-maker op (mapv #(parse-type % opts) args))
               {:syn syn
                :env (::vs/current-env opts)})))

(defmethod parse-type-list :default 
  [syn opts]
  (parse-type-list-default syn opts))

(defmethod parse-type* Cons [l opts] (parse-type-list l opts))
(defmethod parse-type* IPersistentList [l opts] (parse-type-list l opts))

(defn parse-Any [sym]
  (assert (not (-> sym meta :clojure.core.typed/infer))
          "^:clojure.core.typed/infer Any support has been removed. Use t/Infer")
  r/-any)

(defmethod parse-type-symbol 'typed.clojure/Any [s opts] (parse-Any s))
(defmethod parse-type-symbol 'typed.clojure/Infer [s opts] r/-wild)
(defmethod parse-type-symbol 'typed.clojure/TCError [t opts] (r/TCError-maker))
(defmethod parse-type-symbol 'typed.clojure/Nothing [_ opts] (r/Bottom))
(defmethod parse-type-symbol 'typed.clojure/AnyFunction [_ opts] (r/TopFunction-maker))

;; hmmm...a psuedo primitive?
(defmethod parse-type-symbol 'typed.clojure/CLJSInteger [_ opts]
  (impl/assert-cljs 'typed.clojure/CLJSInteger opts)
  (r/CLJSInteger-maker))
(defmethod parse-type-symbol 'typed.clojure/JSnumber [_ opts]
  (impl/assert-cljs 'typed.clojure/JSnumber opts)
  (r/JSNumber-maker))
(defmethod parse-type-symbol 'typed.clojure/JSboolean [_ opts]
  (impl/assert-cljs 'typed.clojure/JSboolean opts)
  (r/JSBoolean-maker))
#_ ;; js/Object
(defmethod parse-type-symbol 'typed.clojure/JSobject [_ opts]
  (impl/assert-cljs 'typed.clojure/JSobject opts)
  (r/JSObject-maker))
(defmethod parse-type-symbol 'typed.clojure/JSstring [_ opts]
  (impl/assert-cljs 'typed.clojure/JSstring opts)
  (r/JSString-maker))
(defmethod parse-type-symbol 'typed.clojure/JSundefined [_ opts]
  (impl/assert-cljs 'typed.clojure/JSundefined opts)
  (r/JSUndefined-maker))
(defmethod parse-type-symbol 'typed.clojure/JSnull [_ opts]
  (impl/assert-cljs 'typed.clojure/JSnull opts)
  (r/JSNull-maker))
(defmethod parse-type-symbol 'typed.clojure/JSsymbol [_ opts]
  (impl/assert-cljs 'typed.clojure/JSsymbol opts)
  (r/JSSymbol-maker))

(defn clj-primitives-fn [opts]
  (let [RClass-of @(RClass-of-var)]
    (fn [sym]
      (case sym
        byte (RClass-of 'byte opts)
        short (RClass-of 'short opts)
        int (RClass-of 'int opts)
        long (RClass-of 'long opts)
        float (RClass-of 'float opts)
        double (RClass-of 'double opts)
        boolean (RClass-of 'boolean opts)
        char (RClass-of 'char opts)
        void r/-nil
        nil))))

(defn -parse-type-symbol-default
  [sym opts]
  (let [primitives (impl/impl-case opts
                     :clojure (clj-primitives-fn opts)
                     :cljs (constantly nil))
        free (when (symbol? sym) 
               (free-ops/free-in-scope sym opts))
        rsym (when-not free
               (impl/impl-case opts
                 :clojure (let [res (when (symbol? sym)
                                      (resolve-type-clj sym opts))]
                            (cond
                              (class? res) (coerce/Class->symbol res)
                              (var? res) (coerce/var->symbol res)
                              ;; name doesn't resolve, try declared protocol or datatype
                              ;; in the current namespace
                              :else (or (resolve-type-alias-clj sym opts)
                                        (let [ns (parse-in-ns opts)
                                              dprotocol (if (namespace sym)
                                                          sym
                                                          (symbol (str ns) (str sym)))
                                              ddatatype (if (some #{\.} (str sym))
                                                          sym
                                                          (symbol (str (munge ns)) (str sym)))]
                                          (cond
                                            (nme-env/declared-protocol? dprotocol opts) dprotocol
                                            (nme-env/declared-datatype? ddatatype opts) ddatatype)))))
                 :cljs (when (symbol? sym)
                         (resolve-type-cljs sym opts))))
        _ (assert ((some-fn symbol? nil?) rsym))]
    ;(prn `parse-type-symbol-default sym rsym)
    (or free
        (primitives sym)
        (parse-type-symbol sym opts)
        (cond
          rsym (r/Name-maker rsym)
          :else (prs-error (str "Cannot resolve type: " (pr-str sym)
                                "\nHint: Is " (pr-str sym) " in scope in namespace"
                                " `" (parse-in-ns opts) "`?") opts)))))

(defn parse-type-symbol-default
  [sym {::keys [parse-type-symbol-default]
        :or {parse-type-symbol-default -parse-type-symbol-default}
        :as opts}]
  (parse-type-symbol-default sym opts))

(defmethod parse-type-symbol :default
  [sym opts]
  nil)

(defmethod parse-type* Symbol [l opts] (parse-type-symbol-default l opts))
(defmethod parse-type* Boolean [v opts] (if v r/-true r/-false)) 
(defmethod parse-type* nil [_ opts] r/-nil)

(declare parse-path-elem parse-proposition*)

(defn parse-proposition [f opts]
  (cond
    (= 'tt f) f/-top
    (= 'ff f) f/-bot
    (= 'no-proposition f) f/-no-proposition
    (not (seq? f)) (prs-error (str "Malformed filter expression: " (pr-str f)) opts)

    (when-some [op (first f)]
      (and (simple-symbol? op)
           (or (= 'or op)
               ;; clojure-clr treats pipes in symbols as special
               (= "|" (name op)))))
    (fl/-or (mapv #(parse-proposition % opts) (next f)) opts)

    :else (parse-proposition* f opts)))

(defn parse-object-path [{:keys [id path]} opts]
  (when-not (f/name-ref? id)
    (prs-error (str "Must pass natural number or symbol as id: " (pr-str id)) opts))
  (let [;; Emit deprecation warning for integer IDs but keep them as integers
        _ (when (integer? id)
            (when-not (and (not (neg? id)) (integer? id))
              (prs-error (str "Integer object id must be non-negative: " (pr-str id)) opts))
            ;; DEPRECATED: using raw int for object id
            (err/deprecated-warn (str "DEPRECATED: Using integer " id " for object id. Use named variables instead.") opts))]
    (orep/-path (when path (mapv #(parse-path-elem % opts) path)) id)))

(defn parse-object [obj opts]
  (case obj
    empty orep/-empty
    no-object orep/-no-object
    (parse-object-path obj opts)))

(defn parse-proposition-set [{:keys [then else] :as fsyn} opts]
  (when-not (map? fsyn)
    (prs-error "Proposition set must be a map" opts))
  (when-some [extra (not-empty (set/difference (set (keys fsyn)) #{:then :else}))]
    (prs-error (str "Invalid filter set options: " extra) opts))
  (fl/-FS (if (contains? fsyn :then)
            (parse-proposition then opts)
            f/-top)
          (if (contains? fsyn :else)
            (parse-proposition else opts)
            f/-top)))

(defmulti parse-proposition* 
  (fn [s opts]
    (when (coll? s)
      (first s))))

(defmethod parse-proposition* :default
  [syn opts]
  (prs-error (str "Malformed filter expression: " (pr-str syn)) opts))

(defmethod parse-proposition* 'is
  [[_ & [tsyn nme psyns :as all]] opts]
  (when-not (<= 2 (count all) 3)
    (prs-error (str "Wrong number of arguments to is") opts))
  (let [t (parse-type tsyn opts)
        p (when (= 3 (count all))
            (mapv #(parse-path-elem % opts) psyns))]
    (fl/-proposition t nme p)))

(defmethod parse-proposition* '!
  [[_ & [tsyn nme psyns :as all]] opts]
  (when-not (<= 2 (count all) 3)
    (prs-error (str "Wrong number of arguments to !") opts))
  (let [t (parse-type tsyn opts)
        p (when (= 3 (count all))
            (mapv #(parse-path-elem % opts) psyns))]
    (fl/-not-proposition t nme p)))

(defmethod parse-proposition* '& [[_ & fsyns] opts]
  (fl/-and (mapv #(parse-proposition % opts) fsyns) opts))

(defmethod parse-proposition* 'when
  [[_ & [a c :as args] :as all] opts]
  (when-not (= 2 (count args))
    (prs-error (str "Wrong number of arguments to when: " all) opts))
  (fl/-imp (parse-proposition a opts) (parse-proposition c opts)))

;FIXME clean up the magic. eg. handle (Class foo bar) as an error
(defmulti parse-path-elem
  (fn [% opts]
    (cond
      (symbol? %) %
      (coll? %) (first %)
      :else 
      (prs-error (str "Malformed path element: " (pr-str %)) opts))))

(defmethod parse-path-elem :default [syn opts]
  (prs-error (str "Malformed path element: " (pr-str syn)) opts))

(defmethod parse-path-elem 'Class [sym opts]
  (when-not (= 'Class sym)
    (prs-error "Class path element must be a symbol" opts))
  (pthrep/ClassPE-maker))

(defmethod parse-path-elem 'Count [sym opts]
  (when-not (= 'Count sym)
    (prs-error "Count path element must be a symbol" opts))
  (pthrep/CountPE-maker))

(defmethod parse-path-elem 'Keys [sym opts]
  (when-not (= 'Keys sym)
    (prs-error "Keys path element must be a symbol" opts))
  (pthrep/KeysPE-maker))

(defmethod parse-path-elem 'Vals [sym opts]
  (when-not (= 'Vals sym)
    (prs-error "Vals path element must be a symbol" opts))
  (pthrep/ValsPE-maker))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]] opts]
  (when-not (= 1 (count all))
    (prs-error "Wrong arguments to Key" opts))
  (pthrep/-kpe ksyn))

(defmethod parse-path-elem 'Nth
  [[_ & [idx :as all]] opts]
  (when-not (= 1 (count all))
    (prs-error "Wrong arguments to Nth" opts))
  (pthrep/NthPE-maker idx))

(defmethod parse-path-elem 'Seq
  [[_ & [idx :as all]] opts]
  (when-not (= 1 (count all))
    (prs-error "Wrong arguments to Nth" opts))
  (pthrep/NthPE-maker idx))

(defmethod parse-path-elem 'Keyword [sym opts]
  (when-not (= 'Keyword sym)
    (prs-error "Keyword path element must be a symbol" opts))
  (pthrep/KeywordPE-maker))

(defmethod parse-path-elem 'Seq [sym opts]
  (when-not (= 'Seq sym)
    (prs-error "Seq path element must be a symbol" opts))
  (pthrep/SeqPE-maker))

(defn- parse-kw-map [m opts]
  {:post [((con/hash-c? r/Value? r/Type?) %)]}
  (into {}
        (map (fn [[k v]]
               [(r/-val k) (parse-type v opts)]))
        m))

(defn distribute-regex-HSequential [t repeat? opts]
  {:pre [(r/Regex? t)]}
  (case (:kind t)
    :cat (do (when (some r/Regex? (:types t))
               (prs-error (str "Regex not supported in t/cat in HSequential position: " (:kind t)) opts))
             (r/-hsequential (:types t) {:repeat repeat?} opts))
    (prs-error (str "Regex not supported in HSequential position: " (:kind t)) opts)))

(defn push-HSequential->regex [{:keys [repeat types] :as t}]
  {:pre [(r/HSequential? t)]}
  (r/regex (vec types) :cat))

(defn distribute-regex-arities [ts rng opts]
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
                                                       :type (distribute-regex-HSequential inner true opts)}]]
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
                                  :type (update t :pre-type distribute-regex-HSequential true opts)}]]

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
                                          (prs-error (str "Not allowed more than one final regex in function domain") opts))
                                        (-> final-group first :type)))]
                     (r/make-Function fixed
                                      rng
                                      :rest (when (= :rest final-kind) final-type)
                                      :drest (when (= :drest final-kind) final-type)
                                      :prest (when (= :prest final-kind) final-type)
                                      :pdot (when (= :pdot final-kind) final-type)
                                      :kws (when (= :kws final-kind) final-type)))))))))))

(def ^:private is-arrow #{'-> :->})

(defn parse-cat [ts opts]
  {:pre [(vector? ts)]}
  (loop [cat-dom []
         [d1 d2 d3 :as to-process] ts]
    (if (empty? to-process)
      cat-dom
      (case d1
        & (case (count to-process)
            1 (prs-error "Must provide syntax after &" opts)
            2 (if (map? d2)
                (do (err/deprecated-warn "[& {} :-> ] function syntax is deprecated. Use [& :optional {} :-> ]" opts)
                    (recur (conj cat-dom (r/-kw-args :optional (parse-kw-map d2 opts)))
                           (subvec to-process 2)))
                (prs-error "Must provide key-value syntax after &" opts))
            (let [[cat-dom to-process] (loop [mandatory nil
                                              optional nil
                                              [d1 d2 :as to-process] (subvec to-process 1)]
                                         (if (empty? to-process)
                                           [(conj cat-dom (r/-kw-args :mandatory (or mandatory {})
                                                                      :optional (or optional {})))
                                            to-process]
                                           (case d1
                                             :mandatory (do (when mandatory
                                                              (prs-error ":mandatory only allowed once per function type." opts))
                                                            (when (< (count to-process) 2)
                                                              (prs-error "Missing type after :mandatory in function type." opts))
                                                            (when-not (map? d2)
                                                              (prs-error (str "Expected map after :mandatory in function type: " (pr-str d2)) opts))
                                                            (recur (parse-kw-map d2 opts) optional (subvec to-process 2)))
                                             :optional (do (when optional
                                                             (prs-error ":optional only allowed once per function type." opts))
                                                           (when (< (count to-process) 2)
                                                             (prs-error "Missing type after :optional in function type." opts))
                                                           (recur mandatory (parse-kw-map d2 opts) (subvec to-process 2)))
                                             ;; force & to go last in function syntax for now
                                             (prs-error (str "Unknown option after & for keyword arguments: " d1) opts))))]
              (recur cat-dom to-process)))
        (case d2
          (:? :+ :* * <*) (if (or (not= '* d2)
                                  (and (= 2 (count to-process))
                                       (do (err/deprecated-warn "* function syntax is deprecated and only supported as final arguments. Use :* instead."
                                                                opts)
                                           true)))
                            (recur (conj cat-dom (case d2
                                                   <* (r/regex [(-> d1 allow-regex (parse-type opts) push-HSequential->regex)] :*)
                                                   (:? :+ :* *) (r/regex [(-> d1 allow-regex (parse-type opts))] (keyword d2))))
                                   (subvec to-process 2))
                            (recur (conj cat-dom (-> d1 allow-regex (parse-type opts)))
                                   (subvec to-process 1)))
          (:... ...) (prs-error (str d1 " syntax has chnaged to :..") opts)
          (:.. <...) (if (or (= :.. d2)
                             (= 3 (count to-process)))
                       (let [drest-bnd d3
                             _ (when-not (simple-symbol? drest-bnd)
                                 (prs-error (str "Bound after " d2 " must be simple symbol: " (pr-str drest-bnd)) opts))
                             bnd (free-ops/free-in-scope-bnds drest-bnd opts)
                             f (free-ops/free-in-scope drest-bnd opts)
                             _ (when-not (r/Regex? bnd)
                                 (prs-error (str "Bound " (pr-str drest-bnd) " after " d2 " is not in scope as a dotted variable") opts))]
                         (recur (conj cat-dom (r/DottedPretype1-maker
                                                (cond-> (let [opts (free-ops/with-bounded-frees opts
                                                                     {(r/make-F drest-bnd)
                                                                      ((requiring-resolve 'typed.cljc.checker.cs-gen/homogeneous-dbound->bound)
                                                                       bnd opts)})]
                                                          (-> d1 allow-regex (parse-type opts)))
                                                  (= '<... d2) push-HSequential->regex)
                                                (:name f)))
                                (subvec to-process 3)))
                       (recur (conj cat-dom (-> d1 allow-regex (parse-type opts)))
                              (subvec to-process 1)))

          (recur (conj cat-dom (-> d1 allow-regex (parse-type opts)))
                 (subvec to-process 1)))))))

(defn parse-Regex [[_ & ts :as this] kind opts]
  (when-not (regex-allowed? this)
    (prs-error (str "Regex type not allowed here: " this) opts))
  (case kind
    (:* :+ :?) (when-not (= 1 (count ts))
                 (prs-error (str (name kind) " regex takes 1 type, given: " (count ts)) opts))
    (:alt :or :cat) nil)
  (r/regex (if (= :cat kind)
             (parse-cat (vec ts) opts)
             (mapv (comp #(parse-type* % opts) allow-regex) ts))
           kind))

(defmethod parse-type-list 'typed.clojure/+   [t opts] (parse-Regex t :+   opts))
(defmethod parse-type-list 'typed.clojure/*   [t opts] (parse-Regex t :*   opts))
(defmethod parse-type-list 'typed.clojure/?   [t opts] (parse-Regex t :?   opts))
(defmethod parse-type-list 'typed.clojure/alt [t opts] (parse-Regex t :alt opts))
(defmethod parse-type-list 'typed.clojure/cat [t opts] (parse-Regex t :cat opts))

;;TODO use [arg1 :- type1, arg2 :- type2 :-> out] syntax to scope objects in range
(defn parse-function [f opts]
  {:pre [(vector? f)]
   :post [(seq %)
          (every? r/Function? %)]}
  (let [[before-arrow after-arrow] (split-with (complement is-arrow) f)
        _ (when (empty? after-arrow)
            (prs-error (str "Missing arrow in function type: " f) opts))
        cat-dom (parse-cat (vec before-arrow) opts)
        _ (when (empty? (next after-arrow))
            (prs-error (str "Missing type after arrow in function type: " f) opts))
        rng (parse-type (second after-arrow) opts)]
    (loop [filters nil
           object nil
           [d1 d2 :as to-process] (vec (nnext after-arrow))]
      (assert (vector? to-process) (pr-str to-process))
      (if (empty? to-process)
        (distribute-regex-arities
          cat-dom
          (r/make-Result rng filters object)
          opts)
        (case d1
          :filters (do (when filters
                         (prs-error ":filters only allowed once per function type." opts))
                       (when (< (count to-process) 2)
                         (prs-error "Missing filter set after :filters in function type." opts))
                       (recur (parse-proposition-set d2 opts) object (subvec to-process 2)))
          :object (do (when object
                        (prs-error ":object only allowed once per function type." opts))
                      (when (< (count to-process) 2)
                        (prs-error "Missing object after :objects in function type." opts))
                      (recur filters (parse-object d2 opts) (subvec to-process 2)))
          (prs-error (str "Invalid function keyword option/s: " (pr-str d1)) opts))))))

(defmethod parse-type* IPersistentVector
  [f opts]
  (apply r/make-FnIntersection (parse-function f opts)))

(defmethod parse-type* :default
  [k opts]
  (prs-error (str "Bad type syntax: " (pr-str k)
                  (when ((some-fn symbol? keyword?) k)
                    (str "\n\nHint: Value types should be preceded by a quote or wrapped in the Value constructor." 
                         " eg. '" (pr-str k) " or (Value " (pr-str k)")"))) opts))

(comment
  (parse-clj `(t/All [s#] [s# :-> s#]))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unparse

;; Unparsing types are generally agnostic to the current implementation.
;; Special types are unparsed under clojure.core.typed in the :unknown
;; implementation. All other types are verbosely printed under :unknown.

(defn unparse-in-ns [{::keys [unparse-type-in-ns] :as opts}]
  {:post [((some-fn nil? symbol?) %)]}
  (or unparse-type-in-ns
      (throw (Exception. "unparse-type-in-ns unbound"))
      (impl/impl-case opts
        :clojure (ns-name *ns*)
        :cljs (cljs-ns)
        :unknown nil)))

(defn with-unparse-ns [opts sym]
  (assoc opts ::unparse-type-in-ns sym))
(defn with-parse-ns [opts sym]
  (assoc opts ::parse-type-in-ns sym))

(defn alias-in-ns
  "Returns an alias for namespace nsym in namespace ns, or nil if none."
  [ns nsym opts]
  {:pre [(plat-con/namespace? ns)
         (simple-symbol? nsym)]
   :post [((some-fn nil? symbol?) %)]}
  (impl/assert-clojure opts)
  (reduce-kv (fn [earliest alias ans]
               (let [ans-sym (ns-name ans)]
                 (if (or (= nsym (ns-unrewrites-clj ans-sym))
                         (= nsym ans-sym)
                         (and (= nsym 'typed.clojure)
                              (= ans-sym 'clojure.core.typed)))
                   (if earliest
                     (if (or (< (-> alias name count)
                                (-> earliest name count))
                             (pos? (compare earliest alias)))
                       alias
                       earliest)
                     alias)
                   earliest)))
             nil (ns-aliases ns)))

(defn core-lang-Class-sym [clsym]
  {:pre [(symbol? clsym)]
   :post [((some-fn nil? symbol?) %)]}
  (when #?(:cljr (.StartsWith (str clsym) "clojure.lang.")
           :default (.startsWith (str clsym) "clojure.lang."))
    (symbol #?(:cljr (.Name (Type/GetType (str clsym)))
               :default (.getSimpleName (Class/forName (str clsym)))))))

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
            (when-some [^#?(:cljr Type :default Class) cls (mapping short-name)]
              (when (= clstr #?(:cljr (.FullName cls) :default (.getName cls)))
                short-name))))))))

(defn protocol-var-symbol-intern
  "Returns a symbol interned in ns for symbol naming a protocol var, or nil if none."
  [sym ns]
  {:pre [(qualified-symbol? sym)
         (plat-con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (let [simple-sym (-> sym name symbol)]
    (when-some [mapping (ns-map ns)]
      (let [v (mapping simple-sym)]
        (when (and (var? v)
                   (= sym (symbol v)))
          simple-sym)))))

(defn unparse-Name-symbol-in-ns [sym {::vs/keys [verbose-types] :as opts}]
  {:pre [(symbol? sym)]
   :post [(symbol? %)]}
  ;(prn "unparse-Name-symbol-in-ns" sym)
  (if-let [ns (and (not verbose-types)
                   (some-> (unparse-in-ns opts) find-ns))]
    (impl/impl-case opts
      :clojure
      (or ; use an import name
          (Class-symbol-intern sym ns)
          ; implicitly imported classes are special
          (core-lang-Class-sym sym)
          ; use unqualified name if interned
          (when (namespace sym)
            (or (protocol-var-symbol-intern sym ns)
                ; use aliased ns if not interned, but ns is aliased
                (when-let [alias (alias-in-ns ns (symbol (namespace sym)) opts)]
                  (symbol (str alias) (name sym)))))
          ; otherwise use fully qualified name
          sym)
      :cljs sym
      :unknown sym)
    sym))

(t/ann unparse-type [r/AnyType t/Any :-> t/Any])
(defn unparse-type [t {::vs/keys [verbose-types] :as opts}]
  ; quick way of giving a Name that the user is familiar with
  ;(prn "unparse-type" (class t))
  (or (when-not verbose-types
        (-> t meta :source-Name))
      (unparse-type* (or (some-> t meta :pretty (get t) :no-simpl deref)
                         t)
                     opts)))

(defn unp [t opts] (prn (unparse-type t opts)))

(defn unparse-F [f {::vs/keys [verbose-types] :as opts}]
  {:pre [(r/F? f)]}
  (if (:unique-tvars verbose-types)
    (:name f)
    (r/F-original-name f)))

(defn unparse-DottedPretype [{:keys [pre-type name] :as t} flat? opts]
  {:pre [(r/DottedPretype? t)]}
  (let [p (unparse-type pre-type opts)
        n (cond-> name
            (symbol? name) (-> r/make-F (unparse-F opts)))]
    (if flat?
      [p :.. n]
      (list 'DottedPretype p n))))

(extend-protocol IUnparseType
  Top
  (unparse-type* [t opts] (unparse-Name-symbol-in-ns `t/Any opts))
  Wildcard 
  (unparse-type* [t opts] (unparse-Name-symbol-in-ns `t/Infer opts))
;; TODO qualify vsym in current ns
  TypeOf 
  (unparse-type* [{:keys [vsym] :as t} opts] (list (unparse-Name-symbol-in-ns `t/TypeOf opts) vsym))
  Unchecked 
  (unparse-type* [{:keys [vsym] :as t} opts]
    (if vsym
      (list 'Unchecked vsym)
    'Unchecked))
  TCError 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns `t/TCError opts))
  Name 
  (unparse-type* [{:keys [id]} opts] (unparse-Name-symbol-in-ns id opts))

  DottedPretype
  (unparse-type* [t opts] (unparse-DottedPretype t false opts))

  CountRange 
  (unparse-type* [{:keys [lower upper]} opts]
    (cond
      (= lower upper) (list (unparse-Name-symbol-in-ns `t/ExactCount opts)
                            lower)
      :else (list* (unparse-Name-symbol-in-ns `t/CountRange opts)
                   lower
                   (when upper [upper]))))

  TApp 
  (unparse-type* [{:keys [rator rands] :as tapp} opts]
    (cond 
      ;perform substitution if obvious
      ;(TypeFn? rator) (unparse-type (resolve-tapp tapp) opts)
      :else (list* (unparse-type rator opts) (mapv #(unparse-type % opts) rands))))

  Result
  (unparse-type* [{:keys [t]} opts] (unparse-type t opts))

  F
  (unparse-type* [f opts] (unparse-F f opts))

  PrimitiveArray
  (unparse-type* 
    [{:keys [jtype input-type output-type]} opts]
    (cond 
      (and (= input-type output-type)
           (= Object jtype))
      (list 'Array (unparse-type input-type opts))

      (= Object jtype)
      (list 'Array2 (unparse-type input-type opts) (unparse-type output-type opts))

      :else
      (list 'Array3 (coerce/Class->symbol jtype)
            (unparse-type input-type opts) (unparse-type output-type opts))))

  B
  (unparse-type* 
    [{:keys [idx]} opts]
    (list 'B idx))

  Union
  (unparse-type* 
    [{types :types :as u} opts]
    (cond
      ; Prefer the user provided Name for this type. Needs more thinking?
      ;(-> u meta :from-name) (-> u meta :from-name)
      (seq types) (list* (unparse-Name-symbol-in-ns `t/U opts) (mapv #(unparse-type % opts) types))
      :else (unparse-Name-symbol-in-ns `t/Nothing opts)))

  FnIntersection
  (unparse-type* 
    [{types :types} opts]
    ; use vector sugar where appropriate
    ; note: sugar is expected by unparse of t/Match
    (if (= 1 (count types))
      (unparse-type (first types) opts)
      (list* (unparse-Name-symbol-in-ns `t/IFn opts)
             (mapv #(unparse-type % opts) types))))

  Intersection
  (unparse-type* 
    [{types :types} opts]
    (list* (unparse-Name-symbol-in-ns `t/I opts)
           (mapv #(unparse-type % opts) types)))

  NotType
  (unparse-type*
    [{:keys [type]} opts]
    (list (unparse-Name-symbol-in-ns `t/Not opts) (unparse-type type opts)))

  TopFunction 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns `t/AnyFunction opts)))

(defn- unparse-kw-map [m opts]
  {:pre [((con/hash-c? r/Value? r/Type?) m)]}
  (reduce-kv (fn [m k v]
               (assoc m (:val k) (unparse-type v opts)))
             {} m))

(defn unparse-result [{:keys [t fl o] :as rng} opts]
  {:pre [(r/Result? rng)]}
  (cond-> [(unparse-type t opts)]
    (not-every? (some-fn f/TopProposition? f/NoProposition?) [(:then fl) (:else fl)])
    (conj :filters (unparse-proposition-set fl opts))

    (not ((some-fn orep/NoObject? orep/EmptyObject?) o))
    (conj :object (unparse-object o opts))))

(defn unparse-bound [name opts]
  {:pre [((some-fn symbol? nat-int?) name)]}
  (if (symbol? name)
    (-> name r/make-F (unparse-F opts))
    `(~'B ~name)))

(defn unparse-regex [{:keys [kind types]} flatten? opts]
  (let [ts (into [] (mapcat (fn [t]
                              (cond
                                (r/DottedPretype? t) (unparse-DottedPretype t true)
                                (r/Regex? t) (case [kind (:kind t)]
                                               ([:cat :*]
                                                [:cat :+]
                                                [:cat :?]) [(-> t :types first (unparse-type opts)) (:kind t)]
                                               [(unparse-type t opts)])
                                :else [(unparse-type t opts)])))
                 types)]
    (cond->> ts
      (not (and flatten? (= :cat kind)))
      (list* (unparse-Name-symbol-in-ns (symbol "typed.clojure" (name kind)) opts)))))

(extend-protocol IUnparseType
  SymbolicClosure
  (unparse-type* 
    [{:keys [fexpr smallest-type]} {::vs/keys [verbose-types] :as opts}]
    (cond->> (unparse-type smallest-type opts)
      verbose-types
      (list 'SymbolicClosure (binding [*print-length* 5
                                       *print-level* 5]
                               (pr-str (:form fexpr))))))

  Regex
  (unparse-type* [t opts] (unparse-regex t false opts))

  Function
  (unparse-type*
    [{:keys [dom rng kws rest drest prest pdot regex]} opts]
    {:post [(vector? %)]}
    (let [before-arrow
          (if regex
            (unparse-regex regex true opts)
            (cond-> (mapv #(unparse-type % opts) dom)
              rest (conj (unparse-type rest opts) :*)
              drest (into (let [{:keys [pre-type name]} drest]
                            [(unparse-type pre-type opts)
                             :..
                             (unparse-bound name opts)]))
              kws (into (let [{:keys [optional mandatory]} kws]
                          (list* '&
                                 (concat
                                   (when (seq mandatory)
                                     [:mandatory (unparse-kw-map mandatory opts)])
                                   (when (seq optional)
                                     [:optional (unparse-kw-map optional opts)])))))
              prest (conj (unparse-type prest opts) '<*)
              pdot (into (let [{:keys [pre-type name]} pdot]
                           [(unparse-type pre-type opts)
                            '<...
                            (unparse-bound name opts)]))))]
      (-> before-arrow
          (conj :->)
          (into (unparse-result rng opts))))))

(extend-protocol IUnparseType
  Protocol
  (unparse-type* 
    [{:keys [the-var poly?]} opts]
    (let [s (unparse-Name-symbol-in-ns the-var opts)]
      (if poly?
        (list* s (mapv #(unparse-type % opts) poly?))
        s)))

  DataType
  (unparse-type* 
    [{:keys [the-class poly?]} opts]
    (if poly?
      (list* (unparse-Name-symbol-in-ns the-class opts) (mapv #(unparse-type % opts) poly?))
      (unparse-Name-symbol-in-ns the-class opts)))

  RClass
  (unparse-type* 
    [{:keys [the-class poly?] :as r} opts]
    (if (empty? poly?)
      (unparse-Name-symbol-in-ns the-class opts)
      (list* (unparse-Name-symbol-in-ns the-class opts) (mapv #(unparse-type % opts) poly?))))

  Mu
  (unparse-type* 
    [m opts]
    (let [nme (-> (c/Mu-fresh-symbol* m) r/make-F (unparse-F opts))
          opts (free-ops/with-bounded-frees opts {(r/make-F nme) r/no-bounds})
          body (c/Mu-body* nme m opts)]
      (list (unparse-Name-symbol-in-ns `t/Rec opts) [nme] (unparse-type body opts))))

  MatchType
  (unparse-type* 
    [m opts]
    (list* (unparse-Name-symbol-in-ns `t/Match opts)
           (unparse-type (:target m) opts)
           (doall
             (mapcat (fn [t]
                       (let [tsyn (unparse-type t opts)]
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
    [m opts]
    (list (unparse-Name-symbol-in-ns `t/Instance opts)
          (unparse-Name-symbol-in-ns (:the-class m) opts)))
  Satisfies
  (unparse-type*
    [m opts]
    (list (unparse-Name-symbol-in-ns `t/Satisfies opts)
          (unparse-Name-symbol-in-ns (:the-var m) opts))))

(defn Bounds->vector [{:keys [upper-bound lower-bound] :as bnds} opts]
  {:pre [(r/Bounds? bnds)]}
  (cond-> []
    (not (r/Top? upper-bound)) (conj :< (unparse-type upper-bound opts))
    (not (r/Bottom? lower-bound)) (conj :> (unparse-type lower-bound opts))))

(defn unparse-poly-bounds-entry [name bnds opts]
  {:pre [(r/Bounds? bnds)]}
  (let [name (-> name r/make-F (unparse-F opts))]
    (if (= r/no-bounds bnds)
      name
      (into [name] (Bounds->vector bnds opts)))))

(defn unparse-poly-dotted-bounds-entry [free-name bbnd opts]
  (assert (= r/dotted-no-bounds bbnd)) ;; TODO do something interesting
  [(-> free-name r/make-F (unparse-F opts)) :..])

(defn unparse-poly-binder [dotted? free-names bbnds named opts]
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
                  (unparse-poly-bounds-entry free-name bbnd opts))
        binder (into (mapv unp-inb fixed-inb)
                     (concat
                       (when-let [[_ free-name bbnd] dotted-inb]
                         (unparse-poly-dotted-bounds-entry free-name bbnd opts))
                       (when named-inb
                         [:named (mapv unp-inb named-inb)])))]
    binder))

(extend-protocol IUnparseType
  Poly
  (unparse-type*
    [{:keys [nbound named kind] :as p} opts]
    (case kind
      :Poly (let [free-names (c/Poly-fresh-symbols* p)
                  ;_ (prn "Poly unparse" free-names (map meta free-names))
                  bbnds (c/Poly-bbnds* free-names p opts)
                  binder (unparse-poly-binder false free-names bbnds named opts)
                  body (c/Poly-body* free-names p opts)]
              (list (unparse-Name-symbol-in-ns `t/All opts) binder (unparse-type body opts)))
      :PolyDots (let [free-names (vec (c/PolyDots-fresh-symbols* p))
                      bbnds (c/PolyDots-bbnds* free-names p opts)
                      binder (unparse-poly-binder true free-names bbnds named opts)
                      body (c/PolyDots-body* free-names p opts)]
                  (list (unparse-Name-symbol-in-ns `t/All opts) binder (unparse-type body opts))))))

;(ann unparse-typefn-bounds-entry [t/Sym Bounds Variance Opts -> Any])
(defn unparse-typefn-bounds-entry [name bnds v opts]
  {:pre [(r/Bounds? bnds)]}
  (let [name (-> name r/make-F (unparse-F opts))]
    (into [name :variance v]
          (when (not= r/no-bounds bnds)
            (Bounds->vector bnds opts)))))

(extend-protocol IUnparseType
  TypeFn
  (unparse-type* 
    [{:keys [nbound] :as p} opts]
    (let [free-names (c/TypeFn-fresh-symbols* p)
          bbnds (c/TypeFn-bbnds* free-names p opts)
          binder (mapv unparse-typefn-bounds-entry free-names bbnds (:variances p) (repeat opts))
          body (c/TypeFn-body* free-names bbnds p opts)]
      (list (unparse-Name-symbol-in-ns `t/TFn opts) binder (unparse-type body opts))))

  Bounds
  (unparse-type* 
    [{:keys [upper-bound lower-bound] :as bnds} opts]
    (if (= r/no-bounds bnds)
      (unparse-Name-symbol-in-ns `t/Type opts)
      (list* (unparse-Name-symbol-in-ns `t/Type opts)
             (Bounds->vector bnds opts))))

  Value
  (unparse-type* 
    [v opts]
    (if (and ((some-fn r/Nil? r/True? r/False?) v)
             (not (::vs/verbose-types opts)))
      (:val v)
      (list (unparse-Name-symbol-in-ns `t/Val opts) (:val v)))))

(defn- unparse-map-of-types [m opts]
  (reduce-kv (fn [m k v]
               (assert (r/Value? k) k)
               (assoc m (:val k) (unparse-type v opts)))
              {} m))

(extend-protocol IUnparseType
  HeterogeneousMap
  (unparse-type* 
    [v opts]
    (list* (unparse-Name-symbol-in-ns `t/HMap opts)
           (concat
             ; only elide if other information is present
             (when (or (seq (:types v))
                       (not (or (seq (:optional v))
                                (seq (:absent-keys v))
                                (c/complete-hmap? v))))
               [:mandatory (unparse-map-of-types (:types v) opts)])
             (when (seq (:optional v))
               [:optional (unparse-map-of-types (:optional v) opts)])
             (when-let [ks (and (not (c/complete-hmap? v))
                                (seq (:absent-keys v)))]
               [:absent-keys (set (map :val ks))])
             (when (c/complete-hmap? v)
               [:complete? true])))))

(defn unparse-KwArgs-trailing [v opts]
  {:pre [(r/KwArgs? v)]}
  (concat
    (when (seq (:optional v))
      [:optional (unparse-map-of-types (:optional v) opts)])
    (when (seq (:mandatory v))
      [:mandatory (unparse-map-of-types (:mandatory v) opts)])
    (when (:complete? v)
      [:complete? (:complete? v)])))

(defn unparse-heterogeneous* [sym {:keys [types rest drest fs objects repeat] :as v} opts]
  (let [first-part (concat
                     (map #(unparse-type % opts) (:types v))
                     (when rest [(unparse-type rest opts) '*])
                     (when drest [(unparse-type (:pre-type drest) opts)
                                  :..
                                  (unparse-bound (:name drest) opts)]))]
    (list* sym
           (vec first-part)
           (concat
             (when repeat
               [:repeat true])
             (when-not (every? #{(fl/-FS f/-top f/-top)} fs)
               [:proposition-sets (mapv #(unparse-proposition-set % opts) fs)])
             (when-not (every? #{orep/-empty} objects)
               [:objects (mapv #(unparse-object % opts) objects)])))))

(extend-protocol IUnparseType
  ;; TODO cannot parse these
  TopHSequential (unparse-type* [v opts] `t/AnyHSequential)
  TopKwArgsSeq (unparse-type* [v opts] `t/AnyKwArgsSeq)

  HSequential 
  (unparse-type* [v opts]
    (unparse-heterogeneous*
      (case (:kind v)
        :list (unparse-Name-symbol-in-ns `t/HList opts)
        :vector (unparse-Name-symbol-in-ns `t/HVec opts)
        :seq (unparse-Name-symbol-in-ns `t/HSeq opts)
        :sequential (unparse-Name-symbol-in-ns `t/HSequential opts))
      v
      opts))

  HSet
  (unparse-type* 
    [{:keys [fixed] :as v} opts]
    {:pre [(every? r/Value? fixed)]}
    (list (unparse-Name-symbol-in-ns `t/HSet opts) (set (map :val fixed))))

  KwArgsArray
  (unparse-type* 
    [{v :kw-args-regex} opts]
    (list* 'KwArgsArray (unparse-KwArgs-trailing v opts)))

  KwArgsSeq
  (unparse-type* 
    [{v :kw-args-regex} opts]
    (list* 'KwArgsSeq (unparse-KwArgs-trailing v opts)))

  AssocType
  (unparse-type* 
    [{:keys [target entries dentries]} opts]
    (list* (unparse-Name-symbol-in-ns `t/Assoc opts)
           (unparse-type target opts)
           (into
             (mapv #(unparse-type % opts) (apply concat entries))
             (when dentries [(unparse-type (:pre-type dentries) opts)
                             :..
                             (unparse-bound (:name dentries) opts)]))))

  MergeType
  (unparse-type* 
    [{:keys [types]} opts]
    (list* (unparse-Name-symbol-in-ns `t/Merge opts)
           (mapv #(unparse-type % opts) types)))

  GetType
  (unparse-type* 
    [{:keys [target key not-found]} opts]
    (list* (unparse-Name-symbol-in-ns `t/Get opts)
           (unparse-type target opts)
           (unparse-type key opts)
           (when (not= r/-nil not-found)
             [(unparse-type not-found opts)])))

; CLJS Types

  JSNumber 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSnumber opts))
  JSBoolean 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSboolean opts))
  JSObject 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSobject opts))
  CLJSInteger 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/CLJSInteger opts))
  JSString 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSstring opts))
  JSSymbol 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSsymbol opts))
  JSUndefined 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSundefined opts))
  JSNull 
  (unparse-type* [_ opts] (unparse-Name-symbol-in-ns 'typed.clojure/JSnull opts))
  JSObj 
  (unparse-type* [t opts] (list (unparse-Name-symbol-in-ns 'typed.clojure/JSObj opts)
                                (zipmap (keys (:types t))
                                        (map #(unparse-type % opts) (vals (:types t))))))

  ArrayCLJS
  (unparse-type* 
    [{:keys [input-type output-type]} opts]
    (cond 
      (= input-type output-type) (list 'Array (unparse-type input-type opts))
      :else (list 'Array2 (unparse-type input-type opts) (unparse-type output-type opts))))

  JSNominal
  (unparse-type* 
    [{:keys [name poly?]} opts]
    (let [sym (symbol name)]
      (if (seq poly?)
        (list* sym (mapv #(unparse-type % opts) poly?))
        sym))))

; Objects

(extend-protocol IUnparseObject
  EmptyObject 
  (unparse-object [_ opts] 'empty)
  NoObject 
  (unparse-object [_ opts] 'no-object)
  Path 
  (unparse-object [{:keys [path id]} opts] 
    (let [;; Unparse Lexical objects to their structure
          id* (if (instance? Lexical id)
                ;; For now, just show as map with depth and index
                ;; In future, might have better syntax
                {:lexical {:depth (:depth id) :index (:index id)}}
                ;; Symbol passes through
                id)]
      (conj {:id id*} (when (seq path) [:path (mapv #(unparse-path-elem % opts) path)]))))
  
  Lexical
  (unparse-object [{:keys [depth index]} opts]
    {:lexical {:depth depth :index index}}))

; Path elems

(extend-protocol IUnparsePathElem
  KeyPE 
  (unparse-path-elem [t opts] (list 'Key (:val t)))
  CountPE 
  (unparse-path-elem [t opts] 'Count)
  ClassPE 
  (unparse-path-elem [t opts] 'Class)
  NthPE 
  (unparse-path-elem [t opts] (list 'Nth (:idx t)))
  KeysPE 
  (unparse-path-elem [t opts] 'Keys)
  ValsPE 
  (unparse-path-elem [t opts] 'Vals)
  KeywordPE 
  (unparse-path-elem [t opts] 'Keyword)
  SeqPE 
  (unparse-path-elem [t opts] 'Seq))

; Propositions

(defn unparse-proposition [f opts]
  (unparse-proposition* f opts))

(defn unparse-proposition-set [{:keys [then else] :as fs} opts]
  {:pre [(f/PropositionSet? fs)]}
  {:then (unparse-proposition then opts)
   :else (unparse-proposition else opts)})

(extend-protocol IUnparseProposition
  TopProposition 
  (unparse-proposition* [f opts] 'tt)
  BotProposition 
  (unparse-proposition* [f opts] 'ff)
  NoProposition 
  (unparse-proposition* [f opts] 'no-proposition)

  TypeProposition
  (unparse-proposition*
    [{:keys [type path id]} opts]
    (concat (list 'is (unparse-type type opts) id)
            (when (seq path)
              [(mapv #(unparse-path-elem % opts) path)])))

  NotTypeProposition
  (unparse-proposition* 
    [{:keys [type path id]} opts]
    (concat (list '! (unparse-type type opts) id)
            (when (seq path)
              [(mapv #(unparse-path-elem % opts) path)])))

  AndProposition 
  (unparse-proposition* [{:keys [fs]} opts] (apply list '& (map #(unparse-proposition % opts) fs)))
  OrProposition 
  (unparse-proposition* [{:keys [fs]} opts] (apply list 'or (map #(unparse-proposition % opts) fs)))

  ImpProposition
  (unparse-proposition* 
    [{:keys [a c]} opts]
    (list 'when (unparse-proposition a opts) (unparse-proposition c opts))))

;[TCResult -> Any]
(defn unparse-TCResult-map [r opts]
  (let [base {:type (unparse-type (r/ret-t r) opts)}]
    (if (and (= (fl/-FS f/-top f/-top) (r/ret-f r))
             (= (r/ret-o r) orep/-empty))
      base
      (cond-> (assoc base :proposition-set (unparse-proposition-set (r/ret-f r) opts))
        (not= (r/ret-o r) orep/-empty)
        (assoc :object (unparse-object (r/ret-o r) opts))))))

(defn unparse-TCResult [r opts]
  (let [{:keys [type] :as m} (unparse-TCResult-map r opts)]
    (if-some [[_ o] (find m :object)]
      [type (:proposition-set m) o]
      (if-some [[_ fs] (find m :proposition-set)]
        [type fs]
        type))))

(defn unparse-TCResult-in-ns [r ns opts]
  {:pre [((some-fn plat-con/namespace? symbol?) ns)]}
  (unparse-TCResult r (with-unparse-ns opts
                        (if (symbol? ns)
                          ns
                          (ns-name ns)))))

(extend-protocol IUnparseType
  TCResult
  (unparse-type* [v opts] (unparse-TCResult v opts)))
