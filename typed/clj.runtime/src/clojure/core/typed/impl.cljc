;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns 
  ^:internal
  ^:typed.clojure/ignore
  clojure.core.typed.impl
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop let
                            ; keep these since the deprecated_wrapper_macros ns may intern these names
                            for doseq dotimes 
                            defn atom ref cast
                            #?(:clj requiring-resolve)
                            #_filter #_remove])
  (:require [clojure.core :as core]
            [clojure.core.typed.import-macros :as import-m]
            ; also for `import-macros` below
            [clojure.core.typed.macros :as macros]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]]))
  (:import (clojure.lang Compiler)))

(defmacro ^:private with-clojure-impl [& body]
  `((requiring-resolve 'clojure.core.typed.current-impl/with-clojure-impl*) (core/fn [] (do ~@body))))

;=============================================================
; # core.typed
;
; This is the main namespace for core.typed. This project is
; split into many internal namespaces. Here are some of the main ones:
;
; c.c.typed.base-env
;   The base global type environment. All base Var Annotations,
;   Java method annotations, Class overriding and other annotations
;   live here.
;
; c.c.typed.type-{rep,ctors}, c.c.parse-unparse,
; c.c.typed.fold-{rep,default}
;   Internal type representation and operations.
;   
; c.c.typed.check
;   The type checker.
;
; c.c.typed.cs-gen
;   Polymorphic local type inference algorithm.

(core/defn load-if-needed
  "Load and initialize all of core.typed if not already"
  []
  ((requiring-resolve 'clojure.core.typed.load-if-needed/load-if-needed)))

;(ann method-type [Symbol -> nil])
(core/defn method-type
  "Given a method symbol, print the core.typed types assigned to it.
  Intended for use at the REPL."
  [mname]
  (load-if-needed)
  (core/let [ms (->> (#?(:cljr clojure.lang.RT/classForNameE :default Class/forName) (namespace mname))
                     ((requiring-resolve 'clojure.reflect/type-reflect))
                     :members
                     (core/filter #(and (instance? clojure.reflect.Method %)
                                        (= (str (:name %)) (name mname))))
                     set)
             _ (assert (seq ms) (str "Method " mname " not found"))]
    (println "Method name:" mname)
    (core/doseq [m ms]
      (println ((requiring-resolve 'typed.clj.checker.parse-unparse/unparse-type)
                ((requiring-resolve 'typed.clj.checker.check/Method->Type)
                 m
                 ((requiring-resolve 'typed.clj.runtime.env/clj-opts)))
                ((requiring-resolve 'typed.clj.runtime.env/clj-opts)))))))

;=============================================================
; Special functions

(core/defn 
  ^{:forms '[(letfn> [fn-spec-or-annotation*] expr*)]}
  letfn>
  "Like letfn, but each function spec must be annotated.

  eg. (letfn> [a :- [Number -> Number]
               (a [b] 2)

               c :- [Symbol -> nil]
               (c [s] nil)]
        ...)"
  [fn-specs-and-annotations body]
  (core/let
       [bindings fn-specs-and-annotations
        ; (Vector (U '[Symbol TypeSyn] LetFnInit))
        normalised-bindings
        (core/loop [[fbnd :as bindings] bindings
                    norm []]
          (cond
            (empty? bindings) norm
            (symbol? fbnd) (do
                             (assert (#{:-} (second bindings))
                                     "letfn> annotations require :- separator")
                             (assert (<= 3 (count bindings)))
                             (recur 
                               (drop 3 bindings)
                               (conj norm [(nth bindings 0)
                                           (nth bindings 2)])))
            (list? fbnd) (recur
                           (next bindings)
                           (conj norm fbnd))
            :else (throw (Exception. (str "Unknown syntax to letfn>: " fbnd)))))
        {anns false inits true} (group-by list? normalised-bindings)]
    `(core/letfn ~(vec inits)
       '~(mapv second anns)
       ;preserve letfn empty body
       ~@(or body [nil]))))

(core/defn ^:no-doc
  declare-datatypes* 
  "Internal use only. Use declare-datatypes."
  [syms nsym]
  (core/let [declare-datatype* (requiring-resolve 'clojure.core.typed.current-impl/declare-datatype*)
             checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))]
    (core/doseq [sym syms]
      (assert (not (or (some #(= \. %) (str sym))
                       (namespace sym)))
              (str "Cannot declare qualified datatype: " sym))
      (core/let [qsym (symbol (str (munge (name nsym)) \. (name sym)))]
        (declare-datatype* checker qsym))))
  nil)

(core/defn declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [syms]
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'declare-datatypes*) '~syms '~(ns-name *ns*))))

(core/defn ^:no-doc
  declare-protocols* 
  "Internal use only. Use declare-protocols."
  [syms]
  nil)

(core/defn declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [syms]
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'declare-protocols*) '~syms)))

(core/defn ^:no-doc
  declare-alias-kind* 
  "Internal use only. Use declare-alias-kind."
  [sym ty]
  nil)

(core/defn declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  `(clojure.core.typed/tc-ignore
     (declare ~sym)
     ((requiring-resolve 'declare-alias-kind*)
      ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
      '~sym '~ty)))

(core/defn ^:no-doc
  declare-names* 
  "Internal use only. Use declare-names."
  [syms]
  (core/let [nsym (ns-name *ns*)]
    (core/doseq [sym syms]
      ((requiring-resolve 'clojure.core.typed.current-impl/declare-name*)
       ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
       (symbol (str nsym) (str sym)))))
  nil)

(core/defn declare-names 
  "Declare names, similar to declare but on the type level."
  [syms]
  (assert (every? (every-pred symbol? (complement namespace)) syms)
          "declare-names only accepts unqualified symbols")
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'declare-names*) '~syms)))

(declare add-to-rt-alias-env add-tc-type-name)

(core/defn ^:no-doc
  defalias*
  "Internal use only. Use defalias."
  [qsym t form]
  (add-to-rt-alias-env form qsym t)
  (add-tc-type-name form qsym t)
  nil)

(defmacro ^:no-doc with-current-location
  [form & body]
  `(core/let [form# ~form]
     (with-bindings {(requiring-resolve 'clojure.core.typed.util-vars/*current-env*)
                     {:ns {:name (ns-name *ns*)}
                      :file *file*
                      :line (or (-> form# meta :line)
                                #?(:cljr @Compiler/LineVar :default @Compiler/LINE))
                      :column (or (-> form# meta :column)
                                  #?(:cljr @Compiler/ColumnVar :default @Compiler/COLUMN))}}
       (do ~@body))))

(defmacro ^:private delay-rt-parse
  "We can type check c.c.t/parse-ast if we replace all instances
  of parse-ast in clojure.core.typed with delay-parse. Otherwise
  there is a circular dependency."
  [t opts]
  `(core/let [t# ~t
              opts# (assoc ~opts :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))]
     ((requiring-resolve `typed.cljc.runtime.env-utils/delay-type*)
      (bound-fn [] ((requiring-resolve 'clojure.core.typed.parse-ast/parse-clj) t# opts#)))))

;;TODO implement reparsing on internal ns reload
(defmacro ^:private delay-tc-parse
  [t opts]
  `(core/let [t# ~t
              opts# (assoc ~opts :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))]
     ((requiring-resolve `typed.cljc.runtime.env-utils/delay-type*)
      (bound-fn []
        ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-clj)
         t#
         opts#)))))

(core/defn ^:no-doc add-to-rt-alias-env [form qsym t]
  (core/let [opts (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                         :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))]
    (with-clojure-impl
      ((requiring-resolve 'clojure.core.typed.current-impl/add-alias-env)
       ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
       qsym
       (with-current-location form
         (delay-rt-parse t opts)))))
  nil)

(def ^:private int-error #(apply (requiring-resolve 'clojure.core.typed.errors/int-error) %&)) 

;;TODO implement reparsing on internal ns reload
(core/defn ^:no-doc add-tc-type-name [form qsym t]
  (with-clojure-impl
    (core/let
      [checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
       opts (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                   :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))
       ;; preserve *ns*
       bfn (bound-fn [f] (f))
       t ((requiring-resolve `typed.cljc.runtime.env-utils/delay-type*)
           (core/fn []
             (core/let
               [unparse-type (requiring-resolve 'typed.clj.checker.parse-unparse/unparse-type)
                t (bfn
                    #(with-current-location form
                       ((requiring-resolve 'typed.cljc.runtime.env-utils/force-type)
                        (delay-tc-parse t opts))))
                _ (with-clojure-impl
                    (when-let [tfn ((requiring-resolve 'typed.cljc.checker.declared-kind-env/declared-kind-or-nil) checker qsym)]
                      (when-not ((requiring-resolve 'typed.clj.checker.subtype/subtype?) t tfn)
                        (int-error (str "Declared kind " (unparse-type tfn opts)
                                        " does not match actual kind " (unparse-type t opts))
                                   opts))))]
               t)))]
      ((requiring-resolve 'clojure.core.typed.current-impl/add-tc-type-name) checker qsym t)))
  nil)

(defn- qualify-sym [sym]
  (core/let [qual (if-some [nsp (some-> sym namespace symbol)]
                    (or (some-> (or ((ns-aliases *ns*) nsp)
                                    (find-ns nsp))
                                ns-name)
                        (throw (Exception. (str "Could not resolve namespace " nsp " in sym " sym))))
                    (ns-name *ns*))]
    (-> (symbol (str qual) (name sym))
        (with-meta (not-empty
                     (-> {}
                         (into (meta sym))
                         (assoc :file *file*)
                         ;;FIXME find line number
                         #_(into (meta &form))))))))

(core/defn defalias 
  "Define a recursive type alias on a qualified symbol. Takes an optional doc-string as a second
  argument.

  Updates the corresponding var with documentation.
  
  eg. (defalias MyAlias
        \"Here is my alias\"
        (U nil String))
  
      ;; recursive alias
      (defalias Expr
        (U '{:op ':if :test Expr :then Expr :else Expr}
           '{:op ':const :val Any}))"
  ([&form sym doc-str t]
   (assert (string? doc-str) "Doc-string passed to defalias must be a string")
   (defalias &form (vary-meta sym assoc :doc doc-str) t))
  ([&form sym t]
   (assert (symbol? sym) (str "First argument to defalias must be a symbol: " sym))
   (core/let
     [qsym (qualify-sym sym)]
     `(clojure.core.typed/tc-ignore
        (when (= "true" (#?(:cljr Environment/GetEnvironmentVariable :default System/getProperty) "clojure.core.typed.intern-defaliases"))
          (intern '~qsym '~(with-meta (symbol (name sym))
                                      (meta sym))))
        ((requiring-resolve 'defalias*) '~qsym '~t '~&form)))))

(defmacro ^:private defspecial [& body]
  (when (= "true" (#?(:cljr Environment/GetEnvironmentVariable :default System/getProperty) "clojure.core.typed.special-vars"))
    `(def ~@body)))

(defspecial
  ^{:doc "Any is the top type that contains all possible values."
    :forms '[Any]
    :clojure.core.typed/special-type true}
  Any)

(defspecial
  ^{:doc "AnyValue contains all Value singleton types"
    :forms '[AnyValue]
    :clojure.core.typed/special-type true}
  AnyValue)

(defspecial
  ^{:doc "TCError is the type of a type error in the type checker. Use only after
         a type error has been thrown. Only ever use this type in a custom typing rule."
    :forms '[TCError]
    :clojure.core.typed/special-type true}
  TCError)

(defspecial
  ^{:doc "U represents a union of types"
    :forms '[(U type*)]
    :clojure.core.typed/special-type true}
  U)

(defspecial
  ^{:doc "(alpha) Resolves to the type of the var (lazily) or local (eagerly) named by sym."
    :forms '[(TypeOf sym)]
    :clojure.core.typed/special-type true}
  TypeOf)

(defspecial
  ^{:doc "Nothing is the bottom type that has no values."
    :forms '[Nothing]
    :clojure.core.typed/special-type true}
  Nothing)

(defspecial
  ^{:doc "I represents an intersection of types"
    :forms '[(I type*)]
    :clojure.core.typed/special-type true}
  I)

(defspecial
  ^{:doc "A singleton type for a constant value."
    :forms '[(Val Constant)
             'Constant]
    :clojure.core.typed/special-type true}
  Val)

(defspecial
  ^{:doc "A singleton type for a constant value."
    :forms '[(Value Constant)
             'Constant]
    :clojure.core.typed/special-type true}
  Value)

(defspecial
  ^{:doc "A type representing a range of counts for a collection"
    :forms '[(CountRange Integer)
             (CountRange Integer Integer)]
    :clojure.core.typed/special-type true}
  CountRange)

(defspecial
  ^{:doc "A type representing a precise count for a collection"
    :forms '[(ExactCount Integer)]
    :clojure.core.typed/special-type true}
  ExactCount)

(defspecial
  ^{:doc "Difference represents a difference of types.

         (Difference t s) is the same as type t with type s removed.

         eg. (Difference (U Num nil) nil)  => Num
         "
    :forms '[(Difference type type type*)]
    :clojure.core.typed/special-type true}
  Difference)

(defspecial
  ^{:doc "HVec is a type for heterogeneous vectors.
         It extends clojure.core.typed/Vec and is a subtype
         of clojure.core.typed/HSequential."
    :forms '[(HVec [fixed*] :filter-sets [FS*] :objects [obj*])
             (HVec [fixed* type *] :filter-sets [FS*] :objects [obj*])
             (HVec [fixed* type ... bound] :filter-sets [FS*] :objects [obj*])
             '[fixed*]
             '[fixed* type *]
             '[fixed* type ... bound]]
    :clojure.core.typed/special-type true}
  HVec)

(defspecial
  ^{:doc "HMap is a type for heterogeneous maps."
    :forms '[(HMap :mandatory {Constant Type*}
                   :optional  {Constant Type*}
                   :absent-keys #{Constant*}
                   :complete? Boolean)
             '{Constant Type*}]
    :clojure.core.typed/special-type true}
  HMap)

(defspecial
  ^{:doc "HSequential is a type for heterogeneous sequential persistent collections.
         It extends IPersistentCollection and Sequential"
    :forms '[(HSequential [fixed*] :filter-sets [FS*] :objects [obj*])
             (HSequential [fixed* rest *] :filter-sets [FS*] :objects [obj*])
             (HSequential [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
    :clojure.core.typed/special-type true}
  HSequential)

(defspecial
  ^{:doc "HSeq is a type for heterogeneous seqs"
    :forms '[(HSeq [fixed*] :filter-sets [FS*] :objects [obj*])
             (HSeq [fixed* rest *] :filter-sets [FS*] :objects [obj*])
             (HSeq [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
    :clojure.core.typed/special-type true}
  HSeq)

(defspecial
  ^{:doc "HList is a type for heterogeneous lists. Is a supertype of HSeq that implements IPersistentList."
    :forms '[(HList [fixed*] :filter-sets [FS*] :objects [obj*])
             (HList [fixed* rest *] :filter-sets [FS*] :objects [obj*])
             (HList [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
    :clojure.core.typed/special-type true}
  HList)

(defspecial
  ^{:doc "HSet is a type for heterogeneous sets.
         Takes a set of simple values. By default
         :complete? is true.

         eg. (HSet #{:a :b :c} :complete? true)"
    :forms '[(HSet #{fixed*} :complete? Boolean)]
    :clojure.core.typed/special-type true}
  HSet)

(defspecial
  ^{:doc "An ordered intersection type of function arities."
    :forms '[(IFn ArityVec+)
             [fixed* -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
             [fixed* rest * -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
             [fixed* drest ... bound -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]]
    :clojure.core.typed/special-type true}
  IFn)

(defspecial
  ^{:doc "A predicate for the given type.

         eg. Type for integer?: (Pred Int)"
    :forms '[(Pred type)]
    :clojure.core.typed/special-type true}
  Pred)

(defspecial
  ^{:doc "A type representing an assoc operation"
    :forms '[(Assoc type type-pairs*)]
    :clojure.core.typed/special-type true}
  Assoc)

(defspecial
  ^{:doc "A type representing a dissoc operation"
    :forms '[(Dissoc type type*)]
    :clojure.core.typed/special-type true}
  Dissoc)

(defspecial
  ^{:doc "A type representing a get operation"
    :forms '[(Get type type)
             (Get type type type)]
    :clojure.core.typed/special-type true}
  Get)

(defspecial
  ^{:doc "A recursive type"
    :forms '[(Rec binder type)]
    :clojure.core.typed/special-type true}
  Rec)

(defspecial
  ^{:doc "A polymorphic binder"
    :forms '[(All binder type)]
    :clojure.core.typed/special-type true}
  All)

(defspecial
  ^{:doc "A type function"
    :forms '[(TFn binder type)]
    :clojure.core.typed/special-type true}
  TFn)

(core/defn ^:no-doc rclass-pred
  "Do not use"
  [rcls opts]
  (core/let
    [checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
     add-rclass-env (requiring-resolve 'clojure.core.typed.current-impl/add-rclass-env)
     Class->symbol (requiring-resolve 'clojure.core.typed.current-impl/Class->symbol)]
    (add-rclass-env checker (Class->symbol rcls) opts)))

(defmacro ^:no-doc rclass-preds
  "Do not use"
  [& args]
  `(do
     ~@(core/for [[k v] (partition 2 args)]
         `(rclass-pred ~k ~v))))

;(ann into-array>* [Any Any -> Any])
(core/defn ^:no-doc
  into-array>*
  "Internal use only. Use into-array>."
  ([cljt coll]
   (load-if-needed)
   (with-clojure-impl
     (into-array ((requiring-resolve 'typed.clj.checker.array-ops/Type->array-member-Class)
                  ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-clj) cljt)
                  ((requiring-resolve 'typed.clj.runtime.env/clj-opts))) coll)))
  ([javat cljt coll]
   (load-if-needed)
   (with-clojure-impl
     (into-array ((requiring-resolve 'typed.clj.checker.array-ops/Type->array-member-Class)
                  ((requiring-resolve 'typed.clj.checker.parse-unparse/parse-clj) javat)
                  ((requiring-resolve 'typed.clj.runtime.env/clj-opts))) coll)))
  ;this is the hacky case to prevent full core.typed from loading
  ([into-array-syn javat cljt coll]
   (into-array (resolve into-array-syn) coll)))

(core/defn ^:no-doc
  non-nil-return* 
  "Internal use only. Use non-nil-return."
  [msym arities]
  ((requiring-resolve 'clojure.core.typed.current-impl/add-nonnilable-method-return)
   ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
   msym arities)
  nil)

(core/defn non-nil-return 
  "Override the return type of fully qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg. ; must use full class name
      (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'non-nil-return*) '~msym '~arities)))

(core/defn ^:no-doc
  nilable-param* 
  "Internal use only. Use nilable-param."
  [msym mmap]
  ((requiring-resolve 'clojure.core.typed.current-impl/add-method-nilable-param)
   ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
   msym mmap)
  nil)

(core/defn nilable-param 
  "Override which parameters in qualified method msym may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'nilable-param*) '~msym '~mmap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(core/defn ^:no-doc
  untyped-var*
  "Internal use only. Use untyped-var."
  [varsym typesyn prs-ns form]
  (with-clojure-impl
    (core/let
      [opts (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                   :typed.clj.checker.parse-unparse/parse-type-in-ns prs-ns)
       checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
       var (resolve varsym)
       _ (assert (var? var) (str varsym " must resolve to a var."))
       qsym ((requiring-resolve 'clojure.core.typed.coerce-utils/var->symbol) var)
       expected-type (with-current-location form
                       (delay-tc-parse typesyn opts))
       _ ((requiring-resolve 'clojure.core.typed.current-impl/add-untyped-var) checker prs-ns qsym expected-type)]
      nil)))

(core/defn untyped-var
  "Check a given var has the specified type at runtime."
  [&form varsym typesyn]
  (core/let
       [prs-ns (-> *ns* ns-name)
        qsym (if (namespace varsym)
               varsym
               (symbol (str prs-ns) (str varsym)))]
    `(clojure.core.typed/tc-ignore
       ((requiring-resolve 'untyped-var*) '~qsym '~typesyn '~prs-ns '~&form))))

(core/defn ^:no-doc
  ann*
  "Internal use only. Use ann."
  [defining-nsym qsym typesyn check? form opt]
  (macros/when-bindable-defining-ns defining-nsym
    (with-clojure-impl
      (core/let
        [checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
         warn (requiring-resolve 'clojure.core.typed.errors/warn)
         var-env (requiring-resolve 'clojure.core.typed.current-impl/var-env)
         add-var-env (requiring-resolve 'clojure.core.typed.current-impl/add-var-env)
         add-tc-var-type (requiring-resolve 'clojure.core.typed.current-impl/add-tc-var-type)
         check-var? (requiring-resolve 'clojure.core.typed.current-impl/check-var?)
         remove-nocheck-var (requiring-resolve 'clojure.core.typed.current-impl/remove-nocheck-var)
         add-nocheck-var (requiring-resolve 'clojure.core.typed.current-impl/add-nocheck-var)
         opts (assoc ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                     :typed.clj.checker.parse-unparse/parse-type-in-ns (ns-name *ns*))
         _ (when (and (contains? (var-env checker) qsym)
                      (not (check-var? checker qsym))
                      check?)
             (when-not (get opt :force-check)
               (warn (str "Removing :no-check from var " qsym)))
             (remove-nocheck-var checker qsym))
         _ (when-not check?
             (add-nocheck-var checker qsym))
         loc-form (or (some #(when ((every-pred :line :column) (meta %))
                               %)
                            [(second form)
                             (first form)])
                      form)
         ast (with-current-location loc-form
               (delay-rt-parse typesyn opts))
         tc-type (with-current-location loc-form
                   (delay-tc-parse typesyn opts))]
        (add-var-env checker qsym ast)
        (add-tc-var-type checker qsym tc-type))))
  nil)

(core/defn ann
  "Annotate varsym with type. If unqualified, qualify in the current namespace.
  If varsym has metadata {:no-check true}, ignore definitions of varsym 
  while type checking. Supports namespace aliases and fully qualified namespaces
  to annotate vars in other namespaces.
  
  eg. ; annotate the var foo in this namespace
      (ann foo [Number -> Number])
  
      ; annotate a var in another namespace
      (ann another.ns/bar [-> nil])
   
      ; don't check this var
      (ann ^:no-check foobar [Integer -> String])"
  [&form varsym typesyn]
  (core/let
       [qsym (if-let [nsym (some-> (namespace varsym) symbol)]
               (symbol (if-some [ns (get (ns-aliases *ns*) nsym)]
                         (-> ns ns-name str)
                         (str nsym))
                       (name varsym))
               (symbol (-> *ns* ns-name str) (str varsym)))
        opts (meta varsym)
        check? (not (:no-check opts))]
    `(clojure.core.typed/tc-ignore
       ((requiring-resolve 'ann*) '~(ns-name *ns*) '~qsym '~typesyn '~check? '~&form '~opts))))

(core/defn ann-many
  "Annotate several vars with type t.

  eg. (ann-many FakeSearch
                web1 web2 image1 image2 video1 video2)"
  [t vs]
  `(do ~@(map #(list `clojure.core.typed/ann % t) vs)))

(core/defn ^:no-doc
  ann-datatype*
  "Internal use only. Use ann-datatype."
  [defining-nsym vbnd dname fields opt form]
  (macros/when-bindable-defining-ns defining-nsym
    (with-clojure-impl
      (core/let [opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                 checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
                 add-datatype-env (requiring-resolve 'clojure.core.typed.current-impl/add-datatype-env)
                 gen-datatype* (requiring-resolve 'clojure.core.typed.current-impl/gen-datatype*)
                 qname (if (some #{\.} (str dname))
                         dname
                         (symbol (str (namespace-munge *ns*) "." dname)))]
        (add-datatype-env
          checker
          qname
          {:record? false
           :name qname
           :fields fields
           :bnd vbnd})
        (with-current-location form
          (gen-datatype* @(requiring-resolve 'clojure.core.typed.util-vars/*current-env*) (ns-name *ns*) dname fields vbnd opt false checker opts))
        nil))))

(core/defn
  ^{:forms '[(ann-datatype dname [field :- type*] opts*)
             (ann-datatype binder dname [field :- type*] opts*)]}
  ann-datatype
  "Annotate datatype Class name dname with expected fields.
  If unqualified, qualify in the current namespace.
  Takes an optional type variable binder before the name.

  Fields must be specified in the same order as presented 
  in deftype, with exactly the same field names.

  Also annotates datatype factories and constructors.

  Binder is a vector of specs. Each spec is a vector
  with the variable name as the first entry, followed by
  keyword arguments:
  - :variance (mandatory)
    The declared variance of the type variable. Possible
    values are :covariant, :contravariant and :invariant.
  - :< (optional)
    The upper type bound of the type variable. Defaults to
    Any, or the most general type of the same rank as the
    lower bound.
  - :> (optional)
    The lower type bound of the type variable. Defaults to
    Nothing, or the least general type of the same rank as the
    upper bound.

  eg. ; a datatype in the current namespace
      (ann-datatype MyDatatype [a :- Number,
                                b :- Long])

      ; a datatype in another namespace
      (ann-datatype another.ns.TheirDatatype
                    [str :- String,
                     vec :- (Vec Number)])

      ; a datatype, polymorphic in a
      (ann-datatype [[a :variance :covariant]]
                    MyPolyDatatype
                    [str :- String,
                     vec :- (Vec Number)
                     ply :- (Set a)])"
  [&form args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
  (core/let
       [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opts}]
        (if bnd-provided?
          (next args)
          args)]
    (assert (not rplc) "Replace NYI")
    (assert (symbol? dname)
            (str "Must provide name symbol: " dname))
    `(clojure.core.typed/tc-ignore
       ((requiring-resolve 'ann-datatype*) '~(ns-name *ns*) '~vbnd '~dname '~fields '~opts '~&form))))

(core/defn ^:no-doc
  ann-record*
  "Internal use only. Use ann-record"
  [defining-nsym vbnd dname fields opt form]
  (macros/when-bindable-defining-ns defining-nsym
    (with-clojure-impl
      (core/let [opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                 checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
                 add-datatype-env (requiring-resolve 'clojure.core.typed.current-impl/add-datatype-env)
                 gen-datatype* (requiring-resolve 'clojure.core.typed.current-impl/gen-datatype*)
                 qname (if (some #{\.} (str dname))
                         dname
                         (symbol (str (namespace-munge *ns*) "." dname)))]
        (add-datatype-env 
          checker
          qname
          {:record? true
           :name qname
           :fields fields
           :bnd vbnd})
        (with-current-location form
          (gen-datatype* @(requiring-resolve 'clojure.core.typed.util-vars/*current-env*) (ns-name *ns*) dname fields vbnd opt true checker opts))
        nil))))

(core/defn 
  ^{:forms '[(ann-record dname [field :- type*] opts*)
             (ann-record binder dname [field :- type*] opts*)]}
  ann-record 
  "Annotate record Class name dname with expected fields.
  If unqualified, qualify in the current namespace.
  Takes an optional type variable binder before the name.

  Fields must be specified in the same order as presented 
  in defrecord, with exactly the same field names.

  Also annotates record factories and constructors.

  Binder is a vector of specs. Each spec is a vector
  with the variable name as the first entry, followed by
  keyword arguments:
  - :variance (mandatory)
    The declared variance of the type variable. Possible
    values are :covariant, :contravariant and :invariant.
  - :< (optional)
    The upper type bound of the type variable. Defaults to
    Any, or the most general type of the same rank as the
    lower bound.
  - :> (optional)
    The lower type bound of the type variable. Defaults to
    Nothing, or the least general type of the same rank as the
    upper bound.
  
  eg. ; a record in the current namespace
      (ann-record MyRecord [a :- Number,
                            b :- Long])

      ; a record in another namespace
      (ann-record another.ns.TheirRecord
                  [str :- String,
                   vec :- (Vec Number)])

      ; a record, polymorphic in a
      (ann-record [[a :variance :covariant]]
                  MyPolyRecord
                  [str :- String,
                   vec :- (Vec Number)
                   ply :- (Set a)])"
  [&form args]
  ;[dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
  (core/let
       [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [dname fields & {ancests :unchecked-ancestors rplc :replace :as opt}]
        (if bnd-provided?
          (next args)
          args)]
    `(clojure.core.typed/tc-ignore
       ((requiring-resolve 'ann-record*)
        '~(ns-name *ns*) '~vbnd '~dname '~fields '~opt '~&form))))

(core/defn ^:no-doc
  ann-protocol*
  "Internal use only. Use ann-protocol."
  [defining-nsym vbnd varsym mth form]
  (macros/when-bindable-defining-ns defining-nsym
    (with-clojure-impl
      (core/let [opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
                 checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
                 add-protocol-env (requiring-resolve 'clojure.core.typed.current-impl/add-protocol-env)
                 gen-protocol* (requiring-resolve 'clojure.core.typed.current-impl/gen-protocol*)
                 qualsym (if (namespace varsym)
                           varsym
                           (symbol (str (ns-name *ns*)) (name varsym)))]
        (add-protocol-env
          checker
          qualsym
          {:name qualsym
           :methods mth
           :bnds vbnd})
        (with-current-location form
          (gen-protocol*
            @(requiring-resolve 'clojure.core.typed.util-vars/*current-env*)
            (ns-name *ns*)
            varsym
            vbnd
            mth
            checker
            opts)))))
  nil)

(core/defn
  ^{:forms '[(ann-protocol vbnd varsym & methods)
             (ann-protocol varsym & methods)]}
  ann-protocol 
  "Annotate a possibly polymorphic protocol var with method types.
  
  eg. (ann-protocol IFoo
        bar
        (IFn [IFoo -> Any]
             [IFoo Number Symbol -> Any])
        baz
        [IFoo Number -> Number])
      (t/tc-ignore
        (defprotocol IFoo
          (bar [this] [this n s])
          (baz [this n])))

      ; polymorphic protocol
      ; x is scoped in the methods
      (ann-protocol [[x :variance :covariant]]
        IFooPoly
        bar
        (IFn [(IFooPoly x) -> Any]
             [(IFooPoly x) Number Symbol -> Any])
        baz
        [(IFooPoly x) Number -> Number])
      (t/tc-ignore
        (defprotocol IFooPoly
          (bar [this] [this n s])
          (baz [this n])))"
  [&form args]
  (assert (seq args) "Protocol name must be provided")
  (core/let [bnd-provided? (vector? (first args))
             vbnd (when bnd-provided?
                    (first args))
             [varsym & mth] (if bnd-provided?
                              (next args)
                              args)
             _ (assert (symbol? varsym) "Protocol name must be a symbol")
             _ (assert (not (vector? (first mth)))
                       "Type variable binder goes before protocol name in ann-protocol")
             _ (core/let [fs (frequencies (map first (partition 2 mth)))]
                 (when-let [dups (seq (filter (core/fn [[_ freq]] (< 1 freq)) fs))]
                   (println (str "WARNING: Duplicate method annotations in ann-protocol (" varsym 
                                 "): " ((requiring-resolve 'clojure.string/join) ", " (map first dups))))))
             ; duplicates are checked above.
             {:as mth} mth
             qsym (qualify-sym varsym)]
    `(clojure.core.typed/tc-ignore
       ((requiring-resolve 'ann-protocol*) '~(ns-name *ns*) '~vbnd '~qsym '~mth '~&form))))

;;TODO remember to use when-bindable-defining-ns when implementing
(core/defn ^:no-doc
  ann-interface* 
  "Internal use only. Use ann-interface."
  [vbnd clsym mth]
  nil)

(core/defn 
  ^{:forms '[(ann-interface vbnd varsym & methods)
             (ann-interface varsym & methods)]}
  ann-interface 
  "Annotate a possibly polymorphic interface (created with definterface) with method types.

  Note: Unlike ann-protocol, omit the target ('this') argument in the method signatures.
  
  eg. (ann-interface IFoo
        bar
        (Fn [-> Any]
            [Number Symbol -> Any])
        baz
        [Number -> Number])
      (definterface IFoo
        (bar [] [n s])
        (baz [n]))

      ; polymorphic protocol
      ; x is scoped in the methods
      (ann-protocol [[x :variance :covariant]]
        IFooPoly
        bar
        (Fn [-> Any]
            [Number Symbol -> Any])
        baz
        [Number -> Number])
      (definterface IFooPoly
        (bar [] [n s])
        (baz [n]))"
  [args]
  (core/let
       [bnd-provided? (vector? (first args))
        vbnd (when bnd-provided?
               (first args))
        [clsym & mth] (if bnd-provided?
                         (next args)
                         args)
        _ (assert (symbol? clsym) "Interface name provided to ann-interface must be a symbol")
        _ (core/let [fs (frequencies (map first (partition 2 mth)))]
            (when-let [dups (seq (filter (core/fn [[_ freq]] (< 1 freq)) fs))]
              (println (str "WARNING: Duplicate method annotations in ann-interface (" clsym 
                            "): " ((requiring-resolve 'clojure.string/join) ", " (map first dups))))
              (flush)))
        ; duplicates are checked above.
        {:as mth} mth
        qualsym (if (namespace clsym)
                  clsym
                  (symbol (munge (str (ns-name *ns*))) (name clsym)))]
    `(clojure.core.typed/tc-ignore
       ((requiring-resolve 'ann-interface*) '~vbnd '~clsym '~mth))))

(core/defn ^:no-doc
  override-constructor* 
  "Internal use only. Use override-constructor."
  [defining-nsym ctorsym typesyn form]
  (macros/when-bindable-defining-ns defining-nsym
    (core/let [checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
               add-constructor-override (requiring-resolve 'clojure.core.typed.current-impl/add-constructor-override)
               opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))]
      (add-constructor-override 
        checker
        ctorsym
        (with-current-location form
          (delay-tc-parse typesyn opts)))
      nil)))

(core/defn override-constructor 
  "Override all constructors for Class ctorsym with type."
  [&form ctorsym typesyn]
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'override-constructor*)
      '~(ns-name *ns*) '~ctorsym '~typesyn '~&form)))

(core/defn ^:no-doc
  override-method* 
  "Internal use only. Use override-method."
  [defining-nsym methodsym typesyn form]
  (macros/when-bindable-defining-ns defining-nsym
    (core/let [checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
               add-method-override (requiring-resolve 'clojure.core.typed.current-impl/add-method-override)
               opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))]
      (add-method-override 
        checker
        methodsym
        (with-current-location form
          (delay-tc-parse typesyn opts)))
      nil)))

(core/defn override-method 
  "Override type for qualified method methodsym.

  methodsym identifies the method to override and should be a
  namespace-qualified symbol in the form <class>/<method-name>.
  The class name needs to be fully qualified.

  typesyn uses the same annotation syntax as functions.

  Use non-nil-return instead of override-method if you want to
  declare that a method can never return nil.

  Example:

    (override-method java.util.Properties/stringPropertyNames
                     [-> (java.util.Set String)])

  This overrides the return type of method stringPropertyNames
  of class java.util.Properties to be (java.util.Set String)."
  [&form methodsym typesyn]
  (assert ((every-pred symbol? namespace) methodsym) "Method symbol must be a qualified symbol")
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'override-method*)
      '~(ns-name *ns*) '~methodsym '~typesyn '~&form)))

(core/defn ^:no-doc
  typed-deps* 
  "Internal use only. Use typed-deps."
  [args form]
  (with-clojure-impl
    (core/let [opts ((requiring-resolve 'typed.clj.runtime.env/clj-opts))
               checker ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
               ns->URL (requiring-resolve 'clojure.core.typed.coerce-utils/ns->URL)
               add-ns-deps (requiring-resolve 'clojure.core.typed.current-impl/add-ns-deps)]
      (with-current-location form
        (core/doseq [dep args]
          (when-not (ns->URL dep opts)
            (int-error (str "Cannot find dependency declared with typed-deps: " dep) opts)))
        (add-ns-deps checker (ns-name *ns*) (set args)))
      nil)))

(core/defn typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols. Only has effect via check-ns.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [&form args]
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'typed-deps*) '~args '~&form)))

(core/defn ^:no-doc var>* [sym]
  ((requiring-resolve 'clojure.core.typed.current-impl/the-var) sym))

(core/defn var>
  "Like var, but resolves at runtime like ns-resolve and is understood by
  the type checker. sym must be fully qualified (without aliases).
  
  eg. (var> clojure.core/+)"
  [sym]
  `((requiring-resolve 'var>*) '~sym))

(core/defn ^:no-doc
  warn-on-unannotated-vars*
  "Internal use only. Use allow-unannotated-vars"
  [nsym]
  ((requiring-resolve 'clojure.core.typed.current-impl/register-warn-on-unannotated-vars)
   ((requiring-resolve 'clojure.core.typed.current-impl/clj-checker))
   nsym)
  nil)

(core/defn warn-on-unannotated-vars
  "Allow unannotated vars in the current namespace. 
  
  Emits a warning instead of a type error when checking
  a def without a corresponding expected type.

  Disables automatic inference of `def` expressions.
  
  eg. (warn-on-unannotated-vars)"
  []
  `(clojure.core.typed/tc-ignore
     ((requiring-resolve 'warn-on-unannotated-vars*) '~(ns-name *ns*))))

;(ann statistics [(Coll Symbol) -> (Map Symbol Stats)])
(core/defn statistics 
  "Takes a collection of namespace symbols and returns a map mapping the namespace
  symbols to a map of data"
  [nsyms]
  (load-if-needed)
  ((requiring-resolve 'typed.clj.checker.statistics/statistics) nsyms ((requiring-resolve 'typed.clj.runtime.env/clj-opts))))

; (ann var-coverage [(Coll Symbol) -> nil])
(core/defn var-coverage 
  "Summarises annotated var coverage statistics to *out*
  for namespaces nsyms, a collection of symbols or a symbol/namespace.
  Defaults to the current namespace if no argument provided."
  ([] (var-coverage *ns*))
  ([nsyms-or-nsym]
   (load-if-needed)
   ((requiring-resolve 'typed.clj.checker.statistics/var-coverage) nsyms-or-nsym)))

(core/defn envs
  "Returns a map of type environments, according to the current state of the
  type checker.

  Output map:
  - :vars      map from var symbols to their verbosely printed types
  - :aliases   map from alias var symbols (made with defalias) to their verbosely printed types
  - :special-types  a set of Vars that are special to the type checker (like Any, U, I)"
  []
  (load-if-needed)
  (merge ((requiring-resolve 'clojure.core.typed.all-envs/all-envs-clj))
         {:special-types (set (->> (ns-publics 'clojure.core.typed)
                                   vals
                                   (filter (core/fn [v]
                                             (when (var? v)
                                               (-> v meta :clojure.core.typed/special-type))))))}))

(core/defn prepare-infer-ns
  "Instruments the current namespace to prepare for runtime type
  or spec inference.

  Optional keys:
    :ns     The namespace to infer types for. (Symbol/Namespace)
            Default: *ns*
    :strategy  Choose which inference preparation strategy to use.
               - :compile      recompile the namespace and wrap at compilation-time.
                               Supports local annotation inference. Source is analyzed
                               via core.typed's custom analyzer.
               - :instrument   wrap top-level vars without recompilation.
                               No support for local annotations, but the default
                               Clojure analyzer is used.
               Default: :compile
    :track-strategy  Choose which track strategy to use.
                     - :lazy    wrap hash maps and possibly other data structures, and
                                lazily track values as they are used.
                     - :eager   eagerly walk over all values, a la clojure.spec checking.
                     Default: :lazy
  "
  [{:keys [ns strategy] :as config
    :or {strategy :compile
         ns *ns*}}]
  (load-if-needed)
  (case strategy
    :compile
    (with-clojure-impl
      (with-bindings {(requiring-resolve 'clojure.core.typed.util-vars/*prepare-infer-ns*) true
                      (requiring-resolve 'clojure.core.typed.util-vars/*instrument-infer-config*) (-> config
                                                                                                      (dissoc :ns))}
        ((requiring-resolve 'clojure.core.typed.load/load-typed-file) 
          (subs (@#'clojure.core/root-resource (if (symbol? ns) ns (ns-name ns))) 1))))
    :instrument
    (throw (Exception. ":instrument not yet implemented")))
  :ok)

(core/defn refresh-runtime-infer 
  "Clean the current state of runtime inference.
  Will forget the results of any tests on instrumented code."
  []
  (load-if-needed)
  ((requiring-resolve 'typed.clj.annotator/refresh-runtime-infer)))

(core/defn runtime-infer 
  "Infer and insert annotations for a given namespace.

  There are two ways to instrument your namespace.

  Call `prepare-infer-ns` function on the namespace
  of your choosing.

  Alternatively, use the :runtime-infer
  feature in your namespace metadata. Note: core.typed
  must be installed via `clojure.core.typed/install`.

  eg. (ns my-ns
        {:lang :core.typed
         :core.typed {:features #{:runtime-infer}}}
        (:require [clojure.core.typed :as t]))

  After your namespace is instrumented, run your tests
  and/or exercise the functions in your namespace.

  Then call `runtime-infer` to populate the namespace's
  corresponding file with these generated annotations.

  Optional keys:
    :ns     The namespace to infer types for. (Symbol/Namespace)
            Default: *ns*
    :fuel   Number of iterations to perform in inference algorithm
            (integer)
            Default: nil (don't restrict iterations)
    :debug  Perform print debugging. (:all/:iterations/nil)
            Default: nil
    :track-depth   Maximum nesting depth data will be tracked.
                   Default: nil (don't restrict nestings)
    :track-count   Maximum number of elements of a single collection
                   will be tracked.
                   Default: nil (don't restrict elements)
    :root-results  Maximum number of inference results collected per top-level
                   root form, from the perspective of the tracker (eg. vars, local functions).
                   Default: nil (don't restrict)
    :preserve-unknown  If true, output the symbol `?` where inference was cut off
                       or never reached.
                       Default: nil (convert to unknown to `clojure.core.typed/Any`)
    :out-dir       A classpath-relative directory (string) to which to dump changes to files,
                   instead of modifying the original file.
                   Default: nil (modify original file)
    :no-squash-vertically     If true, disable the `squash-vertically` pass.
                              Default: nil

  eg. (runtime-infer) ; infer for *ns*

      (runtime-infer :ns 'my-ns) ; infer for my-ns

      (runtime-infer :fuel 0) ; iterations in type inference algorithm
                              ; (higher = smaller types + more recursive)

      (runtime-infer :debug :iterations) ; enable iteration debugging"
  [& kws]
  (load-if-needed)
  (core/let [m (-> (if (= 1 (count kws))
                     (do
                       ((requiring-resolve 'clojure.core.typed.errors/deprecated-warn)
                        "runtime-infer with 1 arg: use {:ns <ns>}"
                        ((requiring-resolve 'typed.clj.runtime.env/clj-opts)))
                       {:ns (first kws)})
                     (apply hash-map kws))
                   (update :ns #(or % *ns*)))]
    ((requiring-resolve 'typed.clj.annotator/runtime-infer) m)))

(core/defn spec-infer 
  "Infer and insert specs for a given namespace.

  There are two ways to instrument your namespace.

  Call `prepare-infer-ns` function on the namespace
  of your choosing.

  Alternatively, use the :runtime-infer
  feature in your namespace metadata. Note: core.typed
  must be installed via `clojure.core.typed/install`.

  eg. (ns my-ns
        {:lang :core.typed
         :core.typed {:features #{:runtime-infer}}}
        (:require [clojure.core.typed :as t]))

  After your namespace is instrumented, run your tests
  and/or exercise the functions in your namespace.

  Then call `spec-infer` to populate the namespace's
  corresponding file with these generated specs.

  Optional keys:
    :ns     The namespace to infer specs for. (Symbol/Namespace)
            Default: *ns*
    :fuel   Number of iterations to perform in inference algorithm
            (integer)
            Default: nil (don't restrict iterations)
    :debug  Perform print debugging. (:all/:iterations/nil)
            Default: nil
    :track-depth   Maximum nesting depth data will be tracked.
                   Default: nil (don't restrict nestings)
    :track-count   Maximum number of elements of a single collection
                   will be tracked.
                   Default: nil (don't restrict elements)
    :root-results  Maximum number of inference results collected per top-level
                   root form, from the perspective of the tracker (eg. vars, local functions).
                   Default: nil (don't restrict)
    :preserve-unknown  If true, output the symbol `?` where inference was cut off
                       or never reached.
                       Default: nil (convert to unknown to `clojure.core/any?`)
    :higher-order-fspec   If true, generate higher-order fspecs.
                          Default: false
    :out-dir       A classpath-relative directory (string) to which to dump changes to files,
                   instead of modifying the original file.
                   Default: nil (modify original file)
    :no-squash-vertically     If true, disable the `squash-vertically` pass.
                              Default: nil
    :spec-macros   If true, output specs for macros.
                   Default: nil (elide macro specs)

  eg. (spec-infer) ; infer for *ns*

      (spec-infer :ns 'my-ns) ; infer for my-ns

      (spec-infer :fuel 0) ; iterations in spec inference algorithm
                           ; (higher = smaller specs + more recursive)

      (spec-infer :debug :iterations) ; enable iteration debugging
  "
  [& kws]
  (load-if-needed)
  (core/let [m (-> (if (= 1 (count kws))
                     (do
                       ((requiring-resolve 'clojure.core.typed.errors/deprecated-warn)
                        "runtime-infer with 1 arg: use {:ns <ns>}"
                        ((requiring-resolve 'typed.clj.runtime.env/clj-opts)))
                       {:ns (first kws)})
                     (apply hash-map kws))
                   (update :ns #(or % *ns*)))]
    ((requiring-resolve 'typed.clj.annotator/spec-infer) m)))

(core/defn register!
  "Internal -- Do not use"
  []
  ((requiring-resolve 'clojure.core.typed.current-impl/register-clj!)))

(core/defn pred 
  "Generate a flat (runtime) predicate for type that returns true if the
  argument is a subtype of the type, otherwise false.

  The current type variable and dotted type variable scope is cleared before parsing.
  
  eg. ((pred Number) 1)
      ;=> true"
  [&form t]
  (register!)
  (with-current-location &form
    ((requiring-resolve 'clojure.core.typed.type-contract/type-syntax->pred) t)))

(core/defn cast
  "Cast a value to a type. Returns a new value that conforms
  to the given type, otherwise throws an error with blame.

  eg. (cast Int 1)
      ;=> 1

      (cast Int nil)
      ; Fail, <blame positive ...>

      ((cast [Int -> Int] identity)
       1)
      ;=> 1

      ((cast [Int -> Int] identity)
       nil)
      ; Fail, <blame negative ...>

      (cast [Int -> Int] nil)
      ; Fail, <blame positive ...>

  (defalias Options
    (HMap :optional {:positive (U Sym Str),
                     :negative (U Sym Str)
                     :file (U Str nil)
                     :line (U Int nil)
                     :column (U Int nil)}))

  (IFn [Contract Any -> Any]
       [Contract Any Options -> Any])

  Options:
  - :positive   positive blame, (U Sym Str)
  - :negative   negative blame, (U Sym Str)
  - :file       file name where contract is checked, (U Str nil)
  - :line       line number where contract is checked, (U Int nil)
  - :column     column number where contract is checked, (U Int nil)"
  ([&form t x opt]
   (register!) ; for type-syntax->contract
   `(do :clojure.core.typed.special-form/special-form
        :clojure.core.typed/cast
        {:type '~t}
        ;; type checker expects a contract to be in this form, ie. ((fn [x] ..) x)
        ;; - clojure.core.typed.check.add-cast
        ;; - clojure.core.typed.check.special.cast
        ((core/fn [x#]
           ((requiring-resolve 'clojure.core.typed.contract/contract*)
             ~(with-current-location &form
                ((requiring-resolve 'clojure.core.typed.type-contract/type-syntax->contract)
                 t))
             x#
             (core/let [opt# ~opt]
               ((requiring-resolve 'clojure.core.typed.contract/make-blame)
                 :positive (or (:positive opt#)
                               "cast")
                 :negative (or (:negative opt#)
                               "cast")
                 :file (or (:file opt#)
                           ~*file*)
                 :line (or (:line opt#)
                           ~(or (-> &form meta :line)
                                #?(:cljr @Compiler/LineVar :default @Compiler/LINE)))
                 :column (or (:column opt#)
                             ~(or (-> &form meta :column)
                                  #?(:cljr @Compiler/ColumnVar :default @Compiler/COLUMN)))))))
         ~x))))

(core/defn infer-unannotated-vars
  "EXPERIMENTAL

  Return a vector of potential var annotations in the given
  namespace, or the current namespace.

  To enable for the current namespace, add the :infer-vars
  :experimental feature to the ns metadata like so:

    (ns infer-me
      {:lang :core.typed
       :core.typed {:experimental #{:infer-vars
                                    :infer-locals}}}
      ...)

  Then run check-ns like usual, and infer-unannotated-vars
  will return the inferred vars without annotations.

  (t/infer-unannotated-vars)
  => [(t/ann u/bar t/Int)
      (t/ann u/foo (t/U [t/Any -> t/Any] Int))]
                                "

  ([] (infer-unannotated-vars (ns-name *ns*)))
  ([nsym-or-ns]
   (load-if-needed)
   (with-clojure-impl
     ((requiring-resolve 'typed.clj.checker.experimental.infer-vars/infer-unannotated-vars) (ns-name nsym-or-ns)))))

;============================================================
; Define clojure.core typed wrappers below here to ensure we don't use them above
; thus dynaload as lazily as possible.
;============================================================

(core/defn dotimes
  "Like clojure.core/dotimes, but with optional annotations.

  If annotation for binding is omitted, defaults to Int.
  
  eg. (dotimes [_ 100]
        (println \"like normal\"))

      (dotimes [x :- Num, 100.123]
        (println \"like normal\" x))"
  [bindings body]
  (@#'core/assert-args
     (vector? bindings) "a vector for its binding"
     (= 2 (count bindings)) "exactly 2 forms in binding vector")
  (core/let [[i t? t n] (if (= :- (second bindings))
                          (core/let [[i _ t n] bindings]
                            (assert (== (count bindings) 4) "Bad arguments to dotimes")
                            [i true t n])
                          (core/let [[i n] bindings]
                            (assert (== (count bindings) 2) "Bad arguments to dotimes")
                            [i nil nil n]))]
    `(core/let [n# (long ~n)]
       (clojure.core.typed/loop [~i :- ~(if t? t `clojure.core.typed/Int) 0]
         (when (< ~i n#)
           ~@body
           (recur (unchecked-inc ~i)))))))
