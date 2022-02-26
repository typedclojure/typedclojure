;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc typed.clj.checker.parse-unparse
  (:require [clojure.core.typed :as t]
            [clojure.core.typed.coerce-utils :as coerce]
            [clojure.core.typed.contract-utils :as con]
            [clojure.core.typed.current-impl :as impl]
            [clojure.core.typed.errors :as err]
            [clojure.core.typed.util-vars :as vs]
            [clojure.math.combinatorics :as comb]
            [clojure.set :as set]
            [typed.clj.checker.constant-type :as const]
            [typed.cljc.analyzer.passes.uniquify :as uniquify]
            [typed.cljc.checker.dvar-env :as dvar]
            [typed.cljc.checker.filter-ops :as fl]
            [typed.cljc.checker.filter-rep :as f]
            [typed.cljc.checker.free-ops :as free-ops]
            [typed.cljc.checker.hset-utils :as hset]
            [typed.cljc.checker.indirect-ops :as ind]
            [typed.cljc.checker.name-env :as nme-env]
            [typed.cljc.checker.object-rep :as orep]
            [typed.cljc.checker.path-rep :as pthrep]
            [typed.cljc.checker.type-ctors :as c]
            [typed.cljc.checker.type-rep :as r])
  (:import (typed.cljc.checker.type_rep NotType DifferenceType Intersection Union FnIntersection
                                        DottedPretype Function RClass App TApp
                                        PrimitiveArray DataType Protocol TypeFn Poly PolyDots
                                        Mu HeterogeneousMap
                                        CountRange Name Value Top TypeOf Unchecked TopFunction B F Result AnyValue
                                        KwArgsSeq KwArgsArray TCError Extends JSNumber JSBoolean SymbolicClosure
                                        CLJSInteger ArrayCLJS JSNominal JSString TCResult AssocType
                                        GetType HSequential HSet JSUndefined JSNull JSSymbol JSObject
                                        JSObj)
           (typed.cljc.checker.filter_rep TopFilter BotFilter TypeFilter NotTypeFilter AndFilter OrFilter
                                          ImpFilter NoFilter)
           (typed.cljc.checker.object_rep NoObject EmptyObject Path)
           (typed.cljc.checker.path_rep KeyPE CountPE ClassPE KeysPE ValsPE NthPE KeywordPE)
           (clojure.lang Cons IPersistentList Symbol IPersistentVector)))

(defprotocol IUnparseType 
  (unparse-type* [t]))
(defprotocol IUnparseObject
  (unparse-object [o]))
(defprotocol IUnparsePathElem
  (unparse-path-elem [p]))
(defprotocol IUnparseFilter
  (unparse-filter* [fl]))

(defonce ^:dynamic *parse-type-in-ns* nil)
(set-validator! #'*parse-type-in-ns* (some-fn nil? symbol? con/namespace?))

(declare unparse-type unparse-filter unparse-filter-set unparse-flow-set)

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

    (defmethod print-method typed.cljc.checker.impl_protocols.IFilter [s writer]
      (cond 
        (f/FilterSet? s) (print-method (unparse-filter-set s) writer)
        (r/FlowSet? s) (print-method (unparse-flow-set s) writer)
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

(defmacro with-parse-ns [sym & body]
  `(binding [*parse-type-in-ns* ~sym]
     ~@body))

(defn with-parse-ns* [sym f]
  {:pre [(symbol? sym)]}
  (binding [*parse-type-in-ns* sym]
    (f)))

(declare parse-type* resolve-type-clj->sym resolve-type-clj resolve-type-cljs)

(defn parse-type [s]
  ;(prn `parse-type (pr-str s))
  (parse-type* s))

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

(def parsed-free-map? (con/hmap-c? :fname symbol?
                                   :bnd r/Bounds?
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
        (err/int-error (str "Invalid variance " (pr-str variance) " in free binder: " f)))
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
(defn parse-free [f]
  {:post [((con/hvector-c? symbol? r/Bounds?) %)]}
  (let [validate-sym (fn [s]
                       (when-not (symbol? s)
                         (err/int-error (str "Type variable must be a symbol, given: " (pr-str s))))
                       (when (namespace s)
                         (err/int-error (str "Type variable must not be namespace qualified: " (pr-str s))))
                       (when (.contains (name s) ".")
                         (err/int-error (str "Type variable must not contain dots (.): " (pr-str s))))
                       (when (#{"true" "nil" "false"} (name s))
                         (err/int-error (str "Type variable must not be named true, false, or nil: " (pr-str s)))))]
    (if (symbol? f)
      (do (validate-sym f)
          [f r/no-bounds])
      (let [[n & {:keys [< >] :as opts}] f]
        (validate-sym n)
        (when (contains? opts :kind)
          (err/deprecated-warn "Kind annotation for TFn parameters"))
        (when (:variance opts) 
          (err/int-error "Variance not supported for variables introduced with All"))
        [n (let [upper-or-nil (when (contains? opts :<)
                                (parse-type <))
                 lower-or-nil (when (contains? opts :>)
                                (parse-type >))]
             (c/infer-bounds upper-or-nil lower-or-nil))]))))

(defn check-forbidden-rec [rec tbody]
  (letfn [(well-formed? [t]
            (and (not= rec t)
                 (if ((some-fn r/Intersection? r/Union?) t)
                   (every? well-formed? (:types t))
                   true)))]
    (when-not (well-formed? tbody)
      (err/int-error (str "Recursive type not allowed here")))))

(defn- Mu*-var []
  (let [v (ns-resolve (find-ns 'typed.cljc.checker.type-ctors) 'Mu*)]
    (assert (var? v) "Mu* unbound")
    v))

(defn parse-rec-type [[rec & [[free-symbol :as bnder] type 
                              :as args]]]
  (when-not (== 1 (count bnder))
    (err/int-error "Rec type requires exactly one entry in binder"))
  (when-not (== 2 (count args))
    (err/int-error "Wrong arguments to Rec"))
  (let [Mu* @(Mu*-var)
        _ (when-not (= 1 (count bnder)) 
            (err/int-error "Only one variable allowed: Rec"))
        f (r/make-F free-symbol)
        body (free-ops/with-frees [f]
               (parse-type type))
        
        _ (check-forbidden-rec f body)]
    (Mu* (:name f) body)))

;(defmethod parse-type-list 'DottedPretype
;  [[_ psyn bsyn]]
;  (let [df (dvar/*dotted-scope* bsyn)]
;    (assert df bsyn)
;    (r/DottedPretype1-maker (free-ops/with-frees [df]
;                         (parse-type psyn))
;                       (:name (dvar/*dotted-scope* bsyn)))))

(defn parse-CountRange [[_ & [n u :as args]]]
  (when-not (#{1 2} (count args))
    (err/int-error "Wrong arguments to CountRange"))
  (when-not (integer? n)
    (err/int-error "First argument to CountRange must be an integer"))
  (when-not (or (#{1} (count args))
                (integer? u))
    (err/int-error "Second argument to CountRange must be an integer"))
  (r/make-CountRange n u))

(defmethod parse-type-list 'typed.clojure/CountRange [t] (parse-CountRange t))

(defn uniquify-local [sym]
  (get-in vs/*current-expr* [:env ::uniquify/locals-frame-val sym]))

(defmethod parse-type-list 'typed.clojure/TypeOf [[_ sym :as t]]
  (impl/assert-clojure)
  (when-not (= 2 (count t))
    (err/int-error (str "Wrong number of arguments to TypeOf (" (count t) ")")))
  (when-not (symbol? sym)
    (err/int-error "Argument to TypeOf must be a symbol."))
  (let [uniquified-local (uniquify-local sym)
        vsym (let [r (resolve-type-clj sym)]
               (when (var? r)
                 (coerce/var->symbol r)))]
    (if uniquified-local
      (let [t (ind/type-of-nofail uniquified-local)]
        (when-not t
          (err/int-error (str "Could not resolve TypeOf for local " sym)))
        t)
      (do
        (when-not vsym
          (err/int-error (str "Could not resolve TypeOf for var " sym)))
        (r/-type-of vsym)))))

(defn parse-ExactCount [[_ & [n :as args]]]
  (when-not (#{1} (count args))
    (err/int-error "Wrong arguments to ExactCount"))
  (when-not (integer? n)
    (err/int-error "First argument to ExactCount must be an integer"))
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
  (when-not (== 1 (count args))
    (err/int-error "Wrong arguments to predicate"))
  (predicate-for (parse-type t-syn)))

(defmethod parse-type-list 'typed.clojure/Pred [t] (parse-Pred t))

(defn parse-Not [[_ tsyn :as all]]
  (when-not (= (count all) 2) 
    (err/int-error (str "Wrong arguments to Not (expected 1): " all)))
  (r/NotType-maker (parse-type tsyn)))
(defmethod parse-type-list 'typed.clojure/Not [frm] (parse-Not frm))

(defn parse-Difference [[_ tsyn & dsyns :as all]]
  (when-not (<= 3 (count all))
    (err/int-error (str "Wrong arguments to Difference (expected at least 2): " all)))
  (apply r/-difference (parse-type tsyn) (mapv parse-type dsyns)))

(defmethod parse-type-list 'typed.clojure/Difference [t] (parse-Difference t))

(defmethod parse-type-list 'typed.clojure/Rec [syn] (parse-rec-type syn))

(defn parse-Assoc [[_ tsyn & entries :as all]]
  (when-not (<= 1 (count (next all)))
    (err/int-error (str "Wrong arguments to Assoc: " all)))
  (let [{ellipsis-pos '...}
        (zipmap entries (range))

        [entries dentries] (split-at (if ellipsis-pos
                                       (dec ellipsis-pos)
                                       (count entries))
                                     entries)

        [drest-type _ drest-bnd] (when ellipsis-pos
                                   dentries)

        _ (when-not (-> entries count even?)
            (err/int-error (str "Incorrect Assoc syntax: " all " , must have even number of key/val pair.")))

        _ (when-not (or (not ellipsis-pos)
                        (= (count dentries) 3))
            (err/int-error (str "Incorrect Assoc syntax: " all " , Dotted rest entry must be 3 entries")))

        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol"))]
  (r/AssocType-maker (parse-type tsyn)
                     (into []
                           (comp (map parse-type)
                                 (partition-all 2)
                                 (map vec))
                           entries)
                     (when ellipsis-pos
                       (let [bnd (dvar/*dotted-scope* drest-bnd)
                             _ (when-not bnd
                                 (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type drest-type))
                           (:name bnd)))))))

(defmethod parse-type-list 'typed.clojure/Assoc [t] (parse-Assoc t))

(defn parse-Get [[_ tsyn keysyn & not-foundsyn :as all]]
  (when-not (#{2 3} (count (next all)))
    (err/int-error (str "Wrong arguments to Get: " all)))
  (r/-get (parse-type tsyn)
          (parse-type keysyn)
          :not-found
          (when (#{3} (count (next all)))
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
                            (let [_ (when-not (#{2} (count (take 2 bnds)))
                                      (err/int-error (str "Keyword option " (second bnds)
                                                        " has no associated value")))
                                  [k v & rst] bnds]
                              (recur rst
                                     (conj out k v)))
                            [bnds out]))]
        (recur rst
               (conj out group))))))

(defn parse-dotted-binder [bnds]
  {:pre [(vector? bnds)]}
  (let [frees-with-bnds (reduce (fn [fs fsyn]
                                  {:pre [(vector? fs)]
                                   :post [(every? (con/hvector-c? symbol? r/Bounds?) %)]}
                                  (conj fs
                                        (free-ops/with-bounded-frees (into {}
                                                                           (map (fn [[n bnd]] [(r/make-F n) bnd]))
                                                                           fs)
                                          (parse-free fsyn))))
                                [] (-> bnds pop pop))
        dvar (parse-free (-> bnds pop peek))]
    [frees-with-bnds dvar]))

(defn parse-normal-binder [bnds]
  (let [frees-with-bnds
        (reduce (fn [fs fsyn]
                  {:pre [(vector? fs)]
                   :post [(every? (con/hvector-c? symbol? r/Bounds?) %)]}
                  (conj fs
                        (free-ops/with-bounded-frees (into {}
                                                           (map (fn [[n bnd]] [(r/make-F n) bnd]))
                                                           fs)
                          (parse-free fsyn))))
                [] bnds)]
    [frees-with-bnds nil]))

(defn parse-unknown-binder [bnds]
  {:pre [((some-fn nil? vector?) bnds)]}
  (when bnds
    ((if (#{'...} (peek bnds))
       parse-dotted-binder
       parse-normal-binder)
     bnds)))

(defn parse-All-binder [bnds]
  {:pre [(vector? bnds)]}
  (let [[positional kwargs] (split-with (complement keyword?) bnds)
        positional (vec positional)
        _ (when-not (even? (count kwargs))
            (err/int-error (str "Expected an even number of keyword options to All, given: " (vec kwargs))))
        _ (when (seq kwargs)
            (when-not (apply distinct? (map first (partition 2 kwargs)))
              (err/int-error (str "Gave repeated keyword args to All: " (vec kwargs)))))
        {:keys [named] :as kwargs} kwargs
        _ (let [unsupported (set/difference (set (keys kwargs)) #{:named})]
            (when (seq unsupported)
              (err/int-error (str "Unsupported keyword argument(s) to All: " unsupported))))
        _ (when (contains? kwargs :named)
            (when-not (and (vector? named)
                           (every? symbol? named))
              (err/int-error (str ":named keyword argument to All must be a vector of symbols, given: " (pr-str named)))))
        dotted? (boolean (#{:... '...} (peek positional)))
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
              (err/int-error (str "Variables bound by All must be unique, given: " no-dots))))
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

;dispatch on last element of syntax in binder
(defn parse-all-type [bnds type]
  (let [_ (assert (vector? bnds))
        {:keys [frees-with-bnds dvar named]} (parse-All-binder bnds)
        bfs (into {}
                  (map (fn [[n bnd]] [(r/make-F n) bnd]))
                  frees-with-bnds)]
    (if dvar
      (free-ops/with-bounded-frees bfs
        (c/PolyDots* (map first (concat frees-with-bnds [dvar]))
                     (map second (concat frees-with-bnds [dvar]))
                     (dvar/with-dotted [(r/make-F (first dvar))]
                       (parse-type type))
                     :named named))
      (free-ops/with-bounded-frees bfs
        (c/Poly* (map first frees-with-bnds)
                 (map second frees-with-bnds)
                 (parse-type type)
                 :named named)))))

(defmethod parse-type-list 'Extends
  [[_ extends & {:keys [without] :as opts} :as syn]]
  (when-not (empty? (set/difference (set (keys opts)) #{:without}))
    (err/int-error (str "Invalid options to Extends:" (keys opts))))
  (when-not (vector? extends) 
    (err/int-error (str "Extends takes a vector of types: " (pr-str syn))))
  (c/-extends (doall (map parse-type extends))
              :without (doall (map parse-type without))))

(defn parse-All [[_All_ bnds syn & more :as all]]
  ;(prn "All syntax" all)
  (when more
    (err/int-error (str "Bad All syntax: " all)))
  (parse-all-type bnds syn))

(defmethod parse-type-list 'typed.clojure/All [t] (parse-All t))

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
    (err/int-error "Expected 1 argument to Array"))
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
    (err/int-error "Expected 1 argument to ReadOnlyArray"))
  (let [o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object (r/Bottom) o)
      :cljs (r/ArrayCLJS-maker (r/Bottom) o))))

(defmethod parse-type-list 'ReadOnlyArray [syn] (parse-ReadOnlyArray syn))
(defmethod parse-type-list 'cljs.core.typed/ReadOnlyArray [syn] (parse-ReadOnlyArray syn))

(defmethod parse-type-list 'Array2
  [[_ isyn osyn & none]]
  (when-not (empty? none) 
    (err/int-error "Expected 2 arguments to Array2"))
  (let [i (parse-type isyn)
        o (parse-type osyn)]
    (impl/impl-case
      :clojure (r/PrimitiveArray-maker Object i o)
      :cljs (r/ArrayCLJS-maker i o))))

(defmethod parse-type-list 'Array3
  [[_ jsyn isyn osyn & none]]
  (impl/assert-clojure)
  (when-not (empty? none) 
    (err/int-error "Expected 3 arguments to Array3"))
  (let [jrclass (c/fully-resolve-type (parse-type jsyn))
        _ (when-not (r/RClass? jrclass) 
            (err/int-error "First argument to Array3 must be a Class"))]
    (r/PrimitiveArray-maker (r/RClass->Class jrclass) (parse-type isyn) (parse-type osyn))))

(declare parse-function)

(defn parse-fn-intersection-type [[Fn & types]]
  (apply r/make-FnIntersection (mapv parse-function types)))

(defn parse-Fn [[_ & types :as syn]]
  (when-not (seq types) 
    (err/int-error (str "Must pass at least one arity to Fn: " (pr-str syn))))
  (when-not (every? vector? types) 
    (err/int-error (str "Fn accepts vectors, given: " (pr-str syn))))
  (parse-fn-intersection-type syn))

(defmethod parse-type-list 'Fn [t] 
  (err/deprecated-plain-op 'Fn 'IFn)
  (parse-Fn t))
(defmethod parse-type-list 'typed.clojure/IFn [t] (parse-Fn t))

(defn parse-free-binder [[nme & {:keys [variance < > kind] :as opts}]]
  (when-not (symbol? nme)
    (err/int-error "First entry in free binder should be a name symbol"))
  {:nme nme :variance (or variance :invariant)
   :bound (r/Bounds-maker
            ;upper
            (when-not kind
              (if (contains? opts :<)
                (parse-type <)
                r/-any))
            ;lower
            (when-not kind
              (if (contains? opts :>)
                (parse-type >)
                r/-nothing))
            ;kind
            (when kind
              (parse-type kind)))})

(defn parse-tfn-binder [[nme & opts-flat :as all]]
  {:pre [(vector? all)]
   :post [((con/hmap-c? :nme symbol? :variance r/variance?
                        :bound r/Bounds?)
           %)]}
  (let [_ (when-not (even? (count opts-flat))
            (err/int-error (str "Uneven arguments passed to TFn binder: "
                              (pr-str all))))
        {:keys [variance < >] 
         :or {variance :inferred}
         :as opts} 
        (apply hash-map opts-flat)]
    (when-not (symbol? nme)
      (err/int-error "Must provide a name symbol to TFn"))
    (when (contains? opts :kind)
      (err/deprecated-warn "Kind annotation for TFn parameters"))
    (when-not (r/variance? variance)
      (err/int-error (str "Invalid variance: " (pr-str variance))))
    {:nme nme :variance variance
     :bound (let [upper-or-nil (when (contains? opts :<)
                                 (parse-type <))
                  lower-or-nil (when (contains? opts :>)
                                 (parse-type >))]
              (c/infer-bounds upper-or-nil lower-or-nil))}))

(defn parse-type-fn 
  [[_ binder bodysyn :as tfn]]
  (when-not (= 3 (count tfn))
    (err/int-error (str "Wrong number of arguments to TFn: " (pr-str tfn))))
  (when-not (every? vector? binder)
    (err/int-error (str "TFn binder should be vector of vectors: " (pr-str tfn))))
  (let [; don't scope a free in its own bounds. Should review this decision
        free-maps (free-ops/with-free-symbols (map (fn [s]
                                                     {:pre [(vector? s)]
                                                      :post [(symbol? %)]}
                                                     (first s))
                                                   binder)
                    (mapv parse-tfn-binder binder))
        bodyt (free-ops/with-bounded-frees (into {}
                                                 (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound]))
                                                 free-maps)
                (parse-type bodysyn))
        ; We check variances lazily in TypeFn-body*. This avoids any weird issues with calculating
        ; variances with potentially partially defined types.
        ;vs (free-ops/with-bounded-frees (map (fn [{:keys [nme bound]}] [(r/make-F nme) bound])
        ;                                     free-maps)
        ;     (frees/fv-variances bodyt))
        ;_ (doseq [{:keys [nme variance]} free-maps]
        ;    (when-let [actual-v (vs nme)]
        ;      (when-not (= (vs nme) variance)
        ;        (err/int-error (str "Type variable " nme " appears in " (name actual-v) " position "
        ;                          "when declared " (name variance))))))
        ]
    (c/TypeFn* (map :nme free-maps)
               (map :variance free-maps)
               (map :bound free-maps)
               bodyt
               {:meta {:env vs/*current-env*}})))

(defmethod parse-type-list 'typed.clojure/TFn [syn] (parse-type-fn syn))

;; parse-HVec, parse-HSequential and parse-HSeq have many common patterns
;; so we reuse them
(defn parse-types-with-rest-drest [err-msg]
  (fn [syns]
    (let [syns (vec syns)
          rest? (#{:* '*} (peek syns))
          dotted? (and (#{:... '...} (some-> (not-empty syns) pop peek))
                       (<= 3 (count syns)))
          _ (when (and rest? dotted?)
              (err/int-error (str err-msg syns)))
          {:keys [fixed rest drest]}
          (cond
            rest?
            (let [fixed (mapv parse-type (-> syns pop pop))
                  rest (parse-type (-> syns pop peek))]
              {:fixed fixed
               :rest rest})
            dotted?
            (let [fixed (mapv parse-type (-> syns pop pop pop))
                  [drest-type _dots_ drest-bnd :as dot-syntax] (take-last 3 syns)
                  ; should never fail, if the logic changes above it's probably
                  ; useful to keep around.
                  _ (when-not (#{3} (count dot-syntax))
                      (err/int-error (str "Bad vector syntax: " dot-syntax)))
                  bnd (dvar/*dotted-scope* drest-bnd)
                  _ (when-not bnd
                      (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
              {:fixed fixed
               :drest (r/DottedPretype1-maker
                        (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                                             (parse-type drest-type))
                        (:name bnd))})
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
  (fn [[_ syn & {:keys [filter-sets objects repeat]}]]
    (let [{:keys [fixed drest rest]} (parse-h*-types syn)]
      (constructor fixed
                   :filters (when filter-sets
                              (mapv parse-filter-set filter-sets))
                   :objects (when objects
                              (mapv parse-object objects))
                   :drest drest
                   :rest rest
                   :repeat (when (true? repeat)
                             true)))))

(def parse-HVec (parse-heterogeneous* parse-hvec-types r/-hvec))
(def parse-HSequential (parse-heterogeneous* parse-hsequential-types r/-hsequential))
(def parse-HSeq (parse-heterogeneous* parse-hseq-types r/-hseq))
(def parse-HList (parse-heterogeneous* parse-hseq-types (comp #(assoc % :kind :list)
                                                              r/-hsequential)))

(defmethod parse-type-list 'typed.clojure/HVec [t] (parse-HVec t))
(defmethod parse-type-list 'typed.clojure/HSequential [t] (parse-HSequential t))
(defmethod parse-type-list 'typed.clojure/HSeq [t] (parse-HSeq t))
(defmethod parse-type-list 'typed.clojure/HList [t]
  (prn `parse-hlist t)
  (parse-HList t))

(defn parse-HSet [[_ ts & {:keys [complete?] :or {complete? true}} :as args]]
  (let [bad (seq (remove hset/valid-fixed? ts))]
    (when bad
      (err/int-error (str "Bad arguments to HSet: " (pr-str bad))))
    (r/-hset (into #{}
                   (map r/-val)
                   ts)
             :complete? complete?)))

(defmethod parse-type-list 'typed.clojure/HSet [t] (parse-HSet t))

(defn- syn-to-hmap [mandatory optional absent-keys complete?]
  (when mandatory
    (when-not (map? mandatory)
      (err/int-error (str "Mandatory entries to HMap must be a map: " mandatory))))
  (when optional
    (when-not (map? optional)
      (err/int-error (str "Optional entries to HMap must be a map: " optional))))
  (letfn [(mapt [m]
            (into {}
                  (map (fn [[k v]]
                         [(r/-val k)
                          (parse-type v)]))
                  m))]
    (let [_ (when-not (every? empty? [(set/intersection (set (keys mandatory))
                                                        (set (keys optional)))
                                      (set/intersection (set (keys mandatory))
                                                        (set absent-keys))
                                      (set/intersection (set (keys optional))
                                                        (set absent-keys))])
              (err/int-error (str "HMap options contain duplicate key entries: "
                                "Mandatory: " (into {} mandatory) ", Optional: " (into {} optional) 
                                ", Absent: " (set absent-keys))))
          _ (when-not (every? keyword? (keys mandatory)) (err/int-error "HMap's mandatory keys must be keywords"))
          mandatory (mapt mandatory)
          _ (when-not (every? keyword? (keys optional)) (err/int-error "HMap's optional keys must be keywords"))
          optional (mapt optional)
          _ (when-not (every? keyword? absent-keys) (err/int-error "HMap's absent keys must be keywords"))
          absent-keys (set (map r/-val absent-keys))]
      (c/make-HMap :mandatory mandatory :optional optional 
                   :complete? complete? :absent-keys absent-keys))))

(defn parse-quoted-hvec [syn]
  (let [{:keys [fixed drest rest]} (parse-hvec-types syn)]
    (r/-hvec fixed
             :drest drest
             :rest rest)))

(defmethod parse-type-list 'quote 
  [[_ syn]]
  (cond
    ((some-fn number? keyword? symbol? string?) syn) (r/-val syn)
    (vector? syn) (parse-quoted-hvec syn)
    ; quoted map is a partial map with mandatory keys
    (map? syn) (syn-to-hmap syn nil nil false)
    :else (err/int-error (str "Invalid use of quote: " (pr-str syn)))))

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
            (err/int-error (str "Uneven keyword arguments to HMap: " (pr-str all))))
        flat-keys (sequence
                    (comp (partition-all 2)
                          (map first))
                    flat-opts)
        _ (when-not (every? keyword? flat-keys)
            (err/int-error (str "HMap requires keyword arguments, given " (pr-str (first flat-keys))
                              #_#_" in: " (pr-str all))))
        _ (let [kf (->> flat-keys
                        multi-frequencies
                        (map first)
                        seq)]
            (when-let [[k] kf]
              (err/int-error (str "Repeated keyword argument to HMap: " (pr-str k)))))

        {:keys [optional mandatory absent-keys complete?]
         :or {complete? false}
         :as others} (apply hash-map flat-opts)
        _ (when-let [[k] (seq (set/difference (set (keys others)) supported-options))]
            (err/int-error (str "Unsupported HMap keyword argument: " (pr-str k))))
        _ (when (and deprecated-mandatory mandatory)
            (err/int-error (str "Cannot provide both deprecated initial map syntax and :mandatory option to HMap")))
        mandatory (or deprecated-mandatory mandatory)]
    (syn-to-hmap mandatory optional absent-keys complete?)))

(defmethod parse-type-list 'HMap [t] (parse-HMap t))
(defmethod parse-type-list 'typed.clojure/HMap [t] (parse-HMap t))

(defn parse-JSObj [[_JSObj_ types :as all]]
  (let [_ (when-not (= 2 (count all))
            (err/int-error (str "Bad syntax to JSObj: " (pr-str all))))
        _ (when-not (every? keyword? (keys types))
            (err/int-error (str "JSObj requires keyword keys, given " (pr-str (class (first (remove keyword? (keys types))))))))
        parsed-types (zipmap (keys types)
                             (map parse-type (vals types)))]
    (r/JSObj-maker parsed-types)))

(defmethod parse-type-list 'typed.clojure/JSObj [t] (parse-JSObj t))

(def ^:private cljs-ns #((requiring-resolve 'typed.cljs.checker.util/cljs-ns)))

(defn- parse-in-ns []
  {:post [(symbol? %)]}
  (or *parse-type-in-ns*
      (impl/impl-case
        :clojure (ns-name *ns*)
        :cljs (cljs-ns))))

(def ns-rewrites-clj {'clojure.core.typed 'typed.clojure})
(def ^:private ns-unrewrites-clj (set/map-invert ns-rewrites-clj))

(defn- resolve-type-clj
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

(defn- resolve-type-clj->sym
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
(defn- resolve-type-cljs
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

(defn parse-RClass [cls-sym params-syn]
  (impl/assert-clojure)
  (let [RClass-of @(RClass-of-var)
        cls (resolve-type-clj cls-sym)
        _ (when-not (class? cls) (err/int-error (str (pr-str cls-sym) " cannot be resolved")))
        tparams (doall (map parse-type params-syn))]
    (RClass-of cls tparams)))

(defn parse-Value [[_Value_ syn :as all]]
  (when-not (#{2} (count all))
    (err/int-error (str "Incorrect number of arguments to Value, " (count all)
                      ", expected 2: " all)))
  (impl/impl-case
    :clojure (const/constant-type syn)
    :cljs (cond
            ((some-fn symbol? keyword?) syn)
              (r/-val syn)
            :else (assert nil "FIXME CLJS parse Value"))))

(defmethod parse-type-list 'typed.clojure/Val [t] (parse-Value t))
(defmethod parse-type-list 'typed.clojure/Value [t] (parse-Value t))

(defmethod parse-type-list 'KeywordArgs
  [[_KeywordArgs_ & {:keys [optional mandatory]}]]
  (when-not (= #{}
               (set/intersection (set (keys optional))
                                 (set (keys mandatory))))
    (err/int-error (str "Optional and mandatory keyword arguments should be disjoint: "
                      (set/intersection (set (keys optional))
                                        (set (keys mandatory))))))
  (let [optional (into {}
                       (map (fn [[k v]]
                              (do (when-not (keyword? k) (err/int-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                                  [(r/-val k) (parse-type v)])))
                       optional)
        mandatory (into {}
                        (map (fn [[k v]]
                               (do (when-not (keyword? k) (err/int-error (str "Keyword argument keys must be keywords: " (pr-str k))))
                                   [(r/-val k) (parse-type v)])))
                        mandatory)]
    (apply c/Un (for [opts (map #(into {} %) (comb/subsets optional))
                      :let [m (into mandatory opts)
                            kss (comb/permutations (keys m))]
                      ks kss
                      k (r/-hseq (mapcat #(find m %) ks))]
                  k))))

(declare unparse-type deprecated-list)

(defn parse-type-list-default 
  [[n & args :as syn]]
  (if-let [d (deprecated-list syn)]
    d
    (let [op (parse-type n)]
      ;(prn "tapp op" op)
      (when-not ((some-fn r/Name? r/TypeFn? r/F? r/B? r/Poly?) op)
        (err/int-error (str "Invalid operator to type application: " syn)))
      (with-meta (r/TApp-maker op (mapv parse-type args))
                 {:syn syn
                  :env vs/*current-env*}))))

(defmethod parse-type-list :default 
  [[n & args :as syn]]
  (parse-type-list-default syn))

(defmethod parse-type* Cons [l] (parse-type-list l))
(defmethod parse-type* IPersistentList [l] 
  (parse-type-list l))

(defmulti parse-type-symbol
  (fn [n] 
    {:pre [(symbol? n)]}
    (or (impl/impl-case
          :clojure (resolve-type-clj->sym n)
          ;;FIXME logic is all tangled
          :cljs (resolve-type-cljs n))
        n)))

(defn parse-Any [sym]
  (if (-> sym meta ::t/infer)
    r/-infer-any
    r/-any))

(defmethod parse-type-symbol 'typed.clojure/Any [s] (parse-Any s))
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

;[Any -> (U nil Type)]
(defmulti deprecated-clj-symbol identity)

(defmethod deprecated-clj-symbol :default [_] nil)

;[Any -> (U nil Type)]
(defn deprecated-symbol [sym]
  {:post [((some-fn nil? r/Type?) %)]}
  (impl/impl-case
    :clojure (deprecated-clj-symbol sym)
    :cljs nil))

;[Any -> (U nil Type)]
(defmulti deprecated-clj-list 
  (fn [[op]]
    (when (symbol? op)
      ((some-fn
         (every-pred
           class? coerce/Class->symbol)
         (every-pred
           var? coerce/var->symbol))
       (resolve-type-clj op)))))

(defmethod deprecated-clj-list :default [_] nil)

;[Any -> (U nil Type)]
(defn deprecated-list [lst]
  {:post [((some-fn nil? r/Type?) %)]}
  (impl/impl-case
    :clojure (deprecated-clj-list lst)
    :cljs nil))

(defn parse-type-symbol-default
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
          rsym ((some-fn deprecated-symbol r/Name-maker) rsym)
          :else (let [menv (let [m (meta sym)]
                             (when ((every-pred :line :column :file) m)
                               m))]
                  (binding [vs/*current-env* (or menv vs/*current-env*)]
                    (err/int-error (str "Cannot resolve type: " (pr-str sym)
                                        "\nHint: Is " (pr-str sym) " in scope?")
                                   {:use-current-env true})))))))

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
    (not ((some-fn seq? list?) f)) (err/int-error (str "Malformed filter expression: " (pr-str f)))
    :else (parse-filter* f)))

(defn parse-object-path [{:keys [id path]}]
  (when-not (f/name-ref? id)
    (err/int-error (str "Must pass natural number or symbol as id: " (pr-str id))))
  (orep/-path (when path (mapv parse-path-elem path)) id))

(defn parse-object [obj]
  (case obj
    empty orep/-empty
    no-object orep/-no-object
    (parse-object-path obj)))

(defn parse-filter-set [{:keys [then else] :as fsyn}]
  (when-not (map? fsyn)
    (err/int-error "Filter set must be a map"))
  (when-some [extra (not-empty (set/difference (set (keys fsyn)) #{:then :else}))]
    (err/int-error (str "Invalid filter set options: " extra)))
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
  (err/int-error (str "Malformed filter expression: " (pr-str syn))))

(defmethod parse-filter* 'is
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (err/int-error (str "Wrong number of arguments to is")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-filter t nme p)))

(defmethod parse-filter* '!
  [[_ & [tsyn nme psyns :as all]]]
  (when-not (#{2 3} (count all))
    (err/int-error (str "Wrong number of arguments to !")))
  (let [t (parse-type tsyn)
        p (when (= 3 (count all))
            (mapv parse-path-elem psyns))]
    (fl/-not-filter t nme p)))

(defmethod parse-filter* '|
  [[_ & fsyns]]
  (apply fl/-or (mapv parse-filter fsyns)))

(defmethod parse-filter* '&
  [[_ & fsyns]]
  (apply fl/-and (mapv parse-filter fsyns)))

(defmethod parse-filter* 'when
  [[_ & [a c :as args] :as all]]
  (when-not (#{2} (count args))
    (err/int-error (str "Wrong number of arguments to when: " all)))
  (fl/-imp (parse-filter a) (parse-filter c)))

;FIXME clean up the magic. eg. handle (Class foo bar) as an error
(defmulti parse-path-elem 
  #(cond
     (symbol? %) %
     (coll? %) (first %)
     :else 
       (err/int-error (str "Malformed path element: " (pr-str %)))))

(defmethod parse-path-elem :default [syn]
  (err/int-error (str "Malformed path element: " (pr-str syn))))

(defmethod parse-path-elem 'Class [_] (pthrep/ClassPE-maker))
(defmethod parse-path-elem 'Count [_] (pthrep/CountPE-maker))

(defmethod parse-path-elem 'Keys [_] (pthrep/KeysPE-maker))
(defmethod parse-path-elem 'Vals [_] (pthrep/ValsPE-maker))

(defmethod parse-path-elem 'Key
  [[_ & [ksyn :as all]]]
  (when-not (= 1 (count all))
    (err/int-error "Wrong arguments to Key"))
  (pthrep/-kpe ksyn))

(defmethod parse-path-elem 'Nth
  [[_ & [idx :as all]]]
  (when-not (= 1 (count all))
    (err/int-error "Wrong arguments to Nth"))
  (pthrep/NthPE-maker idx))

(defmethod parse-path-elem 'Keyword [_] (pthrep/KeywordPE-maker))

(defn- parse-kw-map [m]
  {:post [((con/hash-c? r/Value? r/Type?) %)]}
  (into {}
        (map (fn [[k v]]
               [(r/-val k) (parse-type v)]))
        m))

(defn parse-function [f]
  {:post [(r/Function? %)]}
  (let [is-arrow '#{-> :->}
        all-dom (take-while (complement is-arrow) f)
        [the-arrow rng & opts-flat :as chk] (drop-while (complement is-arrow) f) ;opts aren't used yet
        ;TODO deprecate
        ;_ (when ('#{->} the-arrow)
        ;    )
        _ (when-not (<= 2 (count chk))
            (err/int-error (str "Incorrect function syntax: " f)))

        _ (when-not (even? (count opts-flat))
            (err/int-error (str "Incorrect function syntax, must have even number of keyword parameters: " f)))

        opts (apply hash-map opts-flat)

        {ellipsis-pos :...
         kw-ellipsis-pos '...
         asterix-pos '*
         kw-asterix-pos :*
         ampersand-pos '&
         push-rest-pos '<*
         push-dot-pos '<...}
        (zipmap all-dom (range))

        _ (when-not (#{0 1} (count (filter identity [asterix-pos ellipsis-pos kw-ellipsis-pos ampersand-pos 
                                                     kw-asterix-pos push-rest-pos])))
            (err/int-error "Can only provide one rest argument option: & ... * or <*"))

        ellipsis-pos (or ellipsis-pos kw-ellipsis-pos)
        asterix-pos (or asterix-pos kw-asterix-pos)

        _ (when-some [ks (seq (remove #{:filters :object :flow} (keys opts)))]
            (err/int-error (str "Invalid function keyword option/s: " ks)))

        filters (when-some [[_ fsyn] (find opts :filters)]
                  (parse-filter-set fsyn))

        object (when-some [[_ obj] (find opts :object)]
                 (parse-object obj))

        flow (when-some [[_ obj] (find opts :flow)]
               (r/-flow (parse-filter obj)))

        fixed-dom (cond
                    asterix-pos (take (dec asterix-pos) all-dom)
                    ellipsis-pos (take (dec ellipsis-pos) all-dom)
                    ampersand-pos (take ampersand-pos all-dom)
                    push-rest-pos (take (dec push-rest-pos) all-dom)
                    push-dot-pos (take (dec push-dot-pos) all-dom)
                    :else all-dom)

        rest-type (when asterix-pos
                    (nth all-dom (dec asterix-pos)))
        _ (when-not (or (not asterix-pos)
                        (= (count all-dom) (inc asterix-pos)))
            (err/int-error (str "Trailing syntax after rest parameter: " (pr-str (drop (inc asterix-pos) all-dom)))))
        [drest-type _ drest-bnd :as drest-seq] (when ellipsis-pos
                                                 (drop (dec ellipsis-pos) all-dom))
        _ (when-not (or (not ellipsis-pos) (= 3 (count drest-seq)))
            (err/int-error "Dotted rest entry must be 3 entries"))
        _ (when-not (or (not ellipsis-pos) (symbol? drest-bnd))
            (err/int-error "Dotted bound must be symbol"))
        [pdot-type _ pdot-bnd :as pdot-seq] (when push-dot-pos
                                              (drop (dec push-dot-pos) all-dom))
        _ (when-not (or (not push-dot-pos) (= 3 (count pdot-seq)))
            (err/int-error "push dotted rest entry must be 3 entries"))
        _ (when-not (or (not push-dot-pos) (symbol? pdot-bnd))
            (err/int-error "push dotted bound must be symbol"))
        [& {optional-kws :optional
            mandatory-kws :mandatory
            :as kw-opts}
         :as kws-seq]
        (let [kwsyn (when ampersand-pos
                      (drop (inc ampersand-pos) all-dom))]
          ; support deprecated syntax [& {} -> ] to be equivalent to [& :optional {} -> ]
          (if (and kwsyn
                   (map? (first kwsyn)))
            (do (err/deprecated-warn "[& {} -> ] function syntax is deprecated. Use [& :optional {} -> ]")
                (cons :optional kwsyn))
            kwsyn))

        _ (when-some [extra (seq (remove #{:mandatory :optional} (keys kw-opts)))]
            (err/int-error (str "Unknown t/IFn options for keyword arguments: "
                                (vec extra))))
        _ (when-not (or (not ampersand-pos) (seq kws-seq))
            (err/int-error "Must provide syntax after &"))

        prest-type (when push-rest-pos
                     (nth all-dom (dec push-rest-pos)))
        _ (when-not (or (not push-rest-pos)
                        (= (count all-dom) (inc push-rest-pos)))
            (err/int-error (str "Trailing syntax after pust-rest parameter: " (pr-str (drop (inc push-rest-pos) all-dom)))))]
    (r/make-Function (mapv parse-type fixed-dom)
                     (parse-type rng)
                     :rest
                     (when asterix-pos
                       (parse-type rest-type))
                     :drest
                     (when ellipsis-pos
                       (let [bnd (dvar/*dotted-scope* drest-bnd)
                             _ (when-not bnd 
                                 (err/int-error (str (pr-str drest-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type drest-type))
                           (:name bnd))))
                     :prest
                     (when push-rest-pos
                       (parse-type prest-type))
                     :pdot
                     (when push-dot-pos
                       (let [bnd (dvar/*dotted-scope* pdot-bnd)
                             _ (when-not bnd
                                 (err/int-error (str (pr-str pdot-bnd) " is not in scope as a dotted variable")))]
                         (r/DottedPretype1-maker
                           (free-ops/with-frees [bnd] ;with dotted bound in scope as free
                             (parse-type pdot-type))
                           (:name bnd))))
                     :filter filters
                     :object object
                     :flow flow
                     :optional-kws (some-> optional-kws parse-kw-map)
                     :mandatory-kws (some-> mandatory-kws parse-kw-map))))

(defmethod parse-type* IPersistentVector
  [f]
  (r/make-FnIntersection (parse-function f)))

(defmethod parse-type* :default
  [k]
  (err/int-error (str "Bad type syntax: " (pr-str k)
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
  {:pre [(con/namespace? ns)
         (simple-symbol? nsym)]
   :post [((some-fn nil? symbol?) %)]}
  (impl/assert-clojure)
  (some (fn [[alias ans]]
          (let [ans-sym (ns-name ans)]
            (when (or (= nsym (ns-unrewrites-clj ans-sym))
                      (= nsym ans-sym))
              alias)))
        ;; prefer shorter, lexicographically earlier aliases
        (sort-by (juxt (comp count name key) key)
                 (ns-aliases ns))))

(defn core-lang-Class-sym [clsym]
  {:pre [(symbol? clsym)]
   :post [((some-fn nil? symbol?) %)]}
  (when (.startsWith (str clsym) "clojure.lang.")
    (symbol (.getSimpleName (Class/forName (str clsym))))))

(defn Class-symbol-intern [clsym ns]
  {:pre [(con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (some (fn [[isym cls]]
          (when (= (str clsym) (str (coerce/Class->symbol cls)))
            isym))
        (ns-imports ns)))

(defn var-symbol-intern 
  "Returns a symbol interned in ns for var symbol, or nil if none.

  (var-symbol-intern 'clojure.core/symbol (find-ns 'clojure.core))
  ;=> 'symbol
  (var-symbol-intern 'bar (find-ns 'clojure.core))
  ;=> nil"
  [sym ns]
  {:pre [(symbol? sym)
         (con/namespace? ns)]
   :post [((some-fn nil? symbol?) %)]}
  (some (fn [[isym var]]
          (when (var? var)
            (when (= sym (symbol var))
              isym)))
        (ns-map ns)))

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

(defn unparse-type [t]
  ; quick way of giving a Name that the user is familiar with
  ;(prn "unparse-type" (class t))
  (if-let [nsym (-> t meta :source-Name)]
    nsym
    (unparse-type* t)))

(defn unp [t] (prn (unparse-type t)))

(extend-protocol IUnparseType
  Top 
  (unparse-type* [_] (unparse-Name-symbol-in-ns `t/Any))
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
  (unparse-type* 
    [{:keys [pre-type name]}]
    (list 'DottedPretype (unparse-type pre-type) (if (symbol? name)
                                                   (-> name r/make-F r/F-original-name)
                                                   name)))

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
  (unparse-type* 
    [{:keys [] :as f}]
    ; Note: don't print f here, results in infinite recursion
    ;(prn (-> f :name) (-> f :name meta))
    (r/F-original-name f))

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
      (seq types) (list* (unparse-Name-symbol-in-ns `t/U)
                         (doall (map unparse-type types)))
      :else (unparse-Name-symbol-in-ns `t/Nothing)))

  FnIntersection
  (unparse-type* 
    [{types :types}]
    (cond
      ; use vector sugar where appropriate
      (and (not vs/*verbose-types*)
           (== 1 (count types)))
      (unparse-type (first types))

      :else
      (list* (unparse-Name-symbol-in-ns `t/IFn)
             (doall (map unparse-type types)))))

  Intersection
  (unparse-type* 
    [{types :types}]
    (list* (unparse-Name-symbol-in-ns `t/I)
           (doall (map unparse-type types))))

  DifferenceType
  (unparse-type* 
    [{:keys [type without]}]
    (list* (unparse-Name-symbol-in-ns `t/Difference)
           (unparse-type* type)
           (doall (map unparse-type without))))

  NotType
  (unparse-type* 
    [{:keys [type]}]
    (list (unparse-Name-symbol-in-ns `t/Not) (unparse-type type)))

  TopFunction 
  (unparse-type* [_] (unparse-Name-symbol-in-ns `t/AnyFunction)))

(defn- unparse-kw-map [m]
  {:pre [((con/hash-c? r/Value? r/Type?) m)]}
  (into {}
        (map (fn [[k v]]
               [(:val k) (unparse-type v)]))
        m))

(defn unparse-result [{:keys [t fl o flow] :as rng}]
  {:pre [(r/Result? rng)]}
  (concat [(unparse-type t)]
          (when-not (every? (some-fn f/TopFilter? f/NoFilter?) [(:then fl) (:else fl)])
            [:filters (unparse-filter-set fl)])
          (when-not ((some-fn orep/NoObject? orep/EmptyObject?) o)
            [:object (unparse-object o)])
          (when-not ((some-fn f/TopFilter? f/NoFilter?) (:normal flow))
            [:flow (unparse-flow-set flow)])))

(defn unparse-bound [name]
  {:pre [((some-fn symbol? nat-int?) name)]}
  (if (symbol? name)
    (-> name r/make-F r/F-original-name)
    `(~'B ~name)))

(extend-protocol IUnparseType
  SymbolicClosure
  (unparse-type* 
    [{:keys [fexpr env]}]
    (list 'SymbolicClosure))

  Function
  (unparse-type* 
    [{:keys [dom rng kws rest drest prest pdot]}]
    (vec (concat (doall (map unparse-type dom))
                 (when rest
                   [(unparse-type rest) '*])
                 (when drest
                   (let [{:keys [pre-type name]} drest]
                     [(unparse-type pre-type)
                      '...
                      (unparse-bound name)]))
                 (when kws
                   (let [{:keys [optional mandatory]} kws]
                     (list* '&
                            (concat
                              (when (seq mandatory)
                                [:mandatory (unparse-kw-map mandatory)])
                              (when (seq optional)
                                [:optional (unparse-kw-map optional)])))))
                 (when prest
                   [(unparse-type prest) '<*])
                 (when pdot
                   (let [{:keys [pre-type name]} pdot]
                     [(unparse-type pre-type)
                      '<...
                      (unparse-bound name)]))
                 ['->]
                 (unparse-result rng)))))

(defn unparse-flow-set [flow]
  {:pre [(r/FlowSet? flow)]}
  (unparse-filter (r/flow-normal flow)))

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
      (list* (unparse-Name-symbol-in-ns the-class) (doall (map unparse-type poly?)))))

  Mu
  (unparse-type* 
    [m]
    (let [nme (-> (c/Mu-fresh-symbol* m) r/make-F r/F-original-name)
          body (c/Mu-body* nme m)]
      (list (unparse-Name-symbol-in-ns `t/Rec) [nme] (unparse-type body)))))

(defn unparse-poly-bounds-entry [name {:keys [upper-bound lower-bound higher-kind] :as bnds}]
  (let [name (-> name r/make-F r/F-original-name)
        u (when upper-bound 
            (unparse-type upper-bound))
        l (when lower-bound 
            (unparse-type lower-bound))
        h (when higher-kind
            (unparse-type higher-kind))]
    (or (when higher-kind
          [name :kind h])
        (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
          [name :< u :> l])
        (when-not (r/Top? upper-bound) 
          [name :< u])
        (when-not (r/Bottom? lower-bound)
          [name :> l])
        name)))

(defn unparse-poly-dotted-bounds-entry [free-name bbnd]
  ; ignore dotted bound for now, not sure what it means yet.
  [(-> free-name r/make-F r/F-original-name) '...])

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
  PolyDots
  (unparse-type* 
    [{:keys [nbound named] :as p}]
    (let [free-names (vec (c/PolyDots-fresh-symbols* p))
          bbnds (c/PolyDots-bbnds* free-names p)
          binder (unparse-poly-binder true free-names bbnds named)
          body (c/PolyDots-body* free-names p)]
      (list (unparse-Name-symbol-in-ns `t/All) binder (unparse-type body))))

  Extends
  (unparse-type* 
    [{:keys [extends without]}]
    (list* 'Extends
           (mapv unparse-type extends)
           (when (seq without)
             [:without (mapv unparse-type without)])))

  Poly
  (unparse-type* 
    [{:keys [nbound named] :as p}]
    (let [free-names (c/Poly-fresh-symbols* p)
          ;_ (prn "Poly unparse" free-names (map meta free-names))
          bbnds (c/Poly-bbnds* free-names p)
          binder (unparse-poly-binder false free-names bbnds named)
          body (c/Poly-body* free-names p)]
      (list (unparse-Name-symbol-in-ns `t/All) binder (unparse-type body)))))

;(ann unparse-typefn-bounds-entry [t/Sym Bounds Variance -> Any])
(defn unparse-typefn-bounds-entry [name {:keys [upper-bound lower-bound higher-kind]} v]
  (let [name (-> name r/make-F r/F-original-name)
        u (when upper-bound 
            (unparse-type upper-bound))
        l (when lower-bound 
            (unparse-type lower-bound))
        h (when higher-kind
            (unparse-type higher-kind))]
    (or (when higher-kind
          [name :variance v :kind h])
        (when-not (or (r/Top? upper-bound) (r/Bottom? lower-bound))
          [name :variance v :< u :> l])
        (when-not (r/Top? upper-bound) 
          [name :variance v :< u])
        (when-not (r/Bottom? lower-bound)
          [name :variance v :> l])
        [name :variance v])))

(extend-protocol IUnparseType
  TypeFn
  (unparse-type* 
    [{:keys [nbound] :as p}]
    (let [free-names (c/TypeFn-fresh-symbols* p)
          bbnds (c/TypeFn-bbnds* free-names p)
          binder (mapv unparse-typefn-bounds-entry free-names bbnds (:variances p))
          body (c/TypeFn-body* free-names p)]
      (list (unparse-Name-symbol-in-ns `t/TFn) binder (unparse-type body))))

  Value
  (unparse-type* 
    [v]
    (if ((some-fn r/Nil? r/True? r/False?) v)
      (:val v)
      (list (unparse-Name-symbol-in-ns `t/Val) (:val v)))))

(defn- unparse-map-of-types [m]
  (into {} (map (fn [[k v]]
                  (assert (r/Value? k) k)
                  (vector (:val k) (unparse-type v)))
                m)))

(extend-protocol IUnparseType
  HeterogeneousMap
  (unparse-type* 
    [^HeterogeneousMap v]
    (list* (unparse-Name-symbol-in-ns `t/HMap)
           (concat
             ; only elide if other information is present
             (when (or (seq (:types v))
                       (not (or (seq (:optional v))
                                (seq (:absent-keys v))
                                (c/complete-hmap? v))))
               [:mandatory (unparse-map-of-types (.types v))])
             (when (seq (:optional v))
               [:optional (unparse-map-of-types (:optional v))])
             (when-let [ks (and (not (c/complete-hmap? v))
                                (seq (.absent-keys v)))]
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
                                  '...
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
           (concat
             (doall (map unparse-type (apply concat entries)))
             (when dentries [(unparse-type (:pre-type dentries))
                             '...
                             (unparse-bound (:name dentries))]))))

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
        (list* sym (map unparse-type poly?))
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
  (unparse-path-elem [t] 'Keyword))

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
  (unparse-filter* [{:keys [fs]}] (apply list '| (map unparse-filter fs)))

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
  {:pre [((some-fn con/namespace? symbol?) ns)]}
  (binding [*unparse-type-in-ns* (if (symbol? ns)
                                   ns
                                   (ns-name ns))]
    (unparse-TCResult r)))

(extend-protocol IUnparseType
  TCResult
  (unparse-type* [v] (unparse-TCResult v)))
