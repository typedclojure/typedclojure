;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns 
  ^{:doc "This namespace contains typed wrapper macros, type aliases
and functions for type checking Clojure code. check-ns is the interface
for checking namespaces, cf for checking individual forms."}
  ^:typed.clojure/ignore
  clojure.core.typed
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
            ;; must be present as clojure.core.typed.impl expands to use it
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

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

(core/defn reset-caches
  "Reset internal type caches."
  []
  ((requiring-resolve 'clojure.core.typed.impl/reset-caches)))

;(ann method-type [Symbol -> nil])
(core/defn method-type
  "Given a method symbol, print the core.typed types assigned to it.
  Intended for use at the REPL."
  [mname]
  ((requiring-resolve 'clojure.core.typed.impl/method-type)
   mname))

(core/defn install
  "Install the :core.typed :lang. Takes an optional set of features
  to install, defaults to `:all`, which is equivalent to the set of
  all features.

  Features:
    - :load    Installs typed `load` over `clojure.core/load`, which type checks files
               on the presence of a {:lang :core.typed} metadata entry in the `ns` form.
               The metadata must be inserted in the actual `ns` form saved to disk,
               as it is read directly from the file instead of the current Namespace
               metadata.
    - :eval    Installs typed `eval` over `clojure.core/eval`.
               If `(= :core.typed (:lang (meta *ns*)))` is true, the form will be implicitly
               type checked. The syntax save to disk is ignored however.

  eg. (install)            ; installs `load` and `eval`
  eg. (install :all)       ; installs `load` and `eval`
  eg. (install #{:eval})   ; installs `eval`
  eg. (install #{:load})   ; installs `load`"
  ([] (install :all))
  ([features]
   ((requiring-resolve 'clojure.core.typed.load/install) features)))

;=============================================================
; Special functions

(core/defn print-filterset
  "During type checking, print the filter set attached to form, 
  preceeded by literal string debug-string.
  Returns nil.
  
  eg. (let [s (seq (get-a-seqable))]
        (print-filterset \"Here now\" s))"
  [debug-string frm]
  frm)

(core/defn ^:no-doc
  inst-poly-ctor
  "Internal use only. Use inst-ctor"
  [inst-of types-syn]
  inst-of)

(defmacro inst 
  "Instantiate a polymorphic type with a number of types.
  
  eg. (inst foo-fn t1 t2 t3 ...)"
  [inst-of & types]
  inst-of)

;FIXME should be a special do-op
;;TODO move to clojure.core.typed.impl
(defmacro inst-ctor
  "Instantiate a call to a constructor with a number of types.
  First argument must be an immediate call to a constructor.
  Returns exactly the instantiatee (the first argument).
  
  eg. (inst-ctor (PolyCtor. a b c)
                 t1 t2 ...)"
  [inst-of & types]
  `(inst-poly-ctor ~inst-of '~types))

(defmacro 
  ^{:forms '[(letfn> [fn-spec-or-annotation*] expr*)]}
  letfn>
  "Like letfn, but each function spec must be annotated.

  eg. (letfn> [a :- [Number -> Number]
               (a [b] 2)

               c :- [Symbol -> nil]
               (c [s] nil)]
        ...)"
  [fn-specs-and-annotations & body]
  ((requiring-resolve 'clojure.core.typed.impl/letfn>)
   fn-specs-and-annotations body))

(defmacro declare-datatypes 
  "Declare datatypes, similar to declare but on the type level."
  [& syms]
  ((requiring-resolve 'clojure.core.typed.impl/declare-datatypes)
   syms))

(defmacro declare-protocols 
  "Declare protocols, similar to declare but on the type level."
  [& syms]
  ((requiring-resolve 'clojure.core.typed.impl/declare-protocols)
   syms))

(defmacro declare-alias-kind
  "Declare a kind for an alias, similar to declare but on the kind level."
  [sym ty]
  ((requiring-resolve 'clojure.core.typed.impl/declare-alias-kind)
   sym ty))

(defmacro declare-names
  "Declare names, similar to declare but on the type level."
  [& syms]
  ((requiring-resolve 'clojure.core.typed.impl/declare-names) syms))

(defmacro defalias 
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
  ([sym doc-str t] ((requiring-resolve 'clojure.core.typed.impl/defalias) &form sym doc-str t))
  ([sym t] ((requiring-resolve 'clojure.core.typed.impl/defalias) &form sym t)))

(defmacro ^:private defspecial [& body]
  (when (= "true" (#?(:cljr Environment/GetEnvironmentVariable :default System/getProperty) "clojure.core.typed.special-vars"))
    `(def ~@body)))

(defspecial
  ^{:doc "Any is the top type that contains all possible values."
    :forms '[Any]
    ::special-type true}
  Any)

(defspecial
  ^{:doc "AnyValue contains all Value singleton types"
    :forms '[AnyValue]
    ::special-type true}
  AnyValue)

(defspecial
  ^{:doc "TCError is the type of a type error in the type checker. Use only after
         a type error has been thrown. Only ever use this type in a custom typing rule."
    :forms '[TCError]
    ::special-type true}
  TCError)

(defspecial
  ^{:doc "U represents a union of types"
    :forms '[(U type*)]
    ::special-type true}
  U)

(defspecial
  ^{:doc "(alpha) Resolves to the type of the var (lazily) or local (eagerly) named by sym."
    :forms '[(TypeOf sym)]
    ::special-type true}
  TypeOf)

(defspecial
  ^{:doc "Nothing is the bottom type that has no values."
    :forms '[Nothing]
    ::special-type true}
  Nothing)

(defspecial
  ^{:doc "I represents an intersection of types"
    :forms '[(I type*)]
    ::special-type true}
  I)

(defspecial
  ^{:doc "A singleton type for a constant value."
    :forms '[(Val Constant)
             'Constant]
    ::special-type true}
  Val)

(defspecial
  ^{:doc "A singleton type for a constant value."
    :forms '[(Value Constant)
             'Constant]
    ::special-type true}
  Value)

(defspecial
  ^{:doc "A type representing a range of counts for a collection"
    :forms '[(CountRange Integer)
             (CountRange Integer Integer)]
    ::special-type true}
  CountRange)

(defspecial
  ^{:doc "A type representing a precise count for a collection"
    :forms '[(ExactCount Integer)]
    ::special-type true}
  ExactCount)

(defspecial
  ^{:doc "Difference represents a difference of types.

         (Difference t s) is the same as type t with type s removed.

         eg. (Difference (U Num nil) nil)  => Num
         "
    :forms '[(Difference type type type*)]
    ::special-type true}
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
    ::special-type true}
  HVec)

(defspecial
  ^{:doc "HMap is a type for heterogeneous maps."
    :forms '[(HMap :mandatory {Constant Type*}
                   :optional  {Constant Type*}
                   :absent-keys #{Constant*}
                   :complete? Boolean)
             '{Constant Type*}]
    ::special-type true}
  HMap)

(defspecial
  ^{:doc "HSequential is a type for heterogeneous sequential persistent collections.
         It extends IPersistentCollection and Sequential"
    :forms '[(HSequential [fixed*] :filter-sets [FS*] :objects [obj*])
             (HSequential [fixed* rest *] :filter-sets [FS*] :objects [obj*])
             (HSequential [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
    ::special-type true}
  HSequential)

(defspecial
  ^{:doc "HSeq is a type for heterogeneous seqs"
    :forms '[(HSeq [fixed*] :filter-sets [FS*] :objects [obj*])
             (HSeq [fixed* rest *] :filter-sets [FS*] :objects [obj*])
             (HSeq [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
    ::special-type true}
  HSeq)

(defspecial
  ^{:doc "HList is a type for heterogeneous lists. Is a supertype of HSeq that implements IPersistentList."
    :forms '[(HList [fixed*] :filter-sets [FS*] :objects [obj*])
             (HList [fixed* rest *] :filter-sets [FS*] :objects [obj*])
             (HList [fixed* drest ... bound] :filter-sets [FS*] :objects [obj*])]
    ::special-type true}
  HList)

(defspecial
  ^{:doc "HSet is a type for heterogeneous sets.
         Takes a set of simple values. By default
         :complete? is true.

         eg. (HSet #{:a :b :c} :complete? true)"
    :forms '[(HSet #{fixed*} :complete? Boolean)]
    ::special-type true}
  HSet)

(defspecial
  ^{:doc "An ordered intersection type of function arities."
    :forms '[(IFn ArityVec+)
             [fixed* -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
             [fixed* rest * -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]
             [fixed* drest ... bound -> ret :filters {:then fl :else fl} :object {:id Foo :path Bar}]]
    ::special-type true}
  IFn)

(defspecial
  ^{:doc "A predicate for the given type.

         eg. Type for integer?: (Pred Int)"
    :forms '[(Pred type)]
    ::special-type true}
  Pred)

(defspecial
  ^{:doc "A type representing an assoc operation"
    :forms '[(Assoc type type-pairs*)]
    ::special-type true}
  Assoc)

(defspecial
  ^{:doc "A type representing a dissoc operation"
    :forms '[(Dissoc type type*)]
    ::special-type true}
  Dissoc)

(defspecial
  ^{:doc "A type representing a get operation"
    :forms '[(Get type type)
             (Get type type type)]
    ::special-type true}
  Get)

(defspecial
  ^{:doc "A recursive type"
    :forms '[(Rec binder type)]
    ::special-type true}
  Rec)

(defspecial
  ^{:doc "A polymorphic binder"
    :forms '[(All binder type)]
    ::special-type true}
  All)

(defspecial
  ^{:doc "A type function"
    :forms '[(TFn binder type)]
    ::special-type true}
  TFn)

(core/defn ^:no-doc rclass-pred
  "Do not use"
  [rcls opts]
  (core/let
    [add-rclass-env (requiring-resolve 'clojure.core.typed.current-impl/add-rclass-env)
     Class->symbol (requiring-resolve 'clojure.core.typed.current-impl/Class->symbol)]
    (with-clojure-impl
      (add-rclass-env (Class->symbol rcls) opts))))

(defmacro ^:no-doc rclass-preds
  "Do not use"
  [& args]
  `(do
     ~@(core/for [[k v] (partition 2 args)]
         `(rclass-pred ~k ~v))))

;;TODO make into-array typing rule and delete this and into-array>
(core/defn ^:no-doc
  into-array>*
  "Internal use only. Use into-array>."
  ([cljt coll] ((requiring-resolve 'clojure.core.typed.impl/into-array>*) cljt coll))
  ([javat cljt coll] ((requiring-resolve 'clojure.core.typed.impl/into-array>*) javat cljt coll))
  ;this is the hacky case to prevent full core.typed from loading
  ([into-array-syn javat cljt coll] ((requiring-resolve 'clojure.core.typed.impl/into-array>*) into-array-syn javat cljt coll)))

;FIXME hacky 4-arity version to prevent full type system from loading
(defmacro into-array> 
  "Make a Java array with Java class javat and Typed Clojure type
  cljt. Resulting array will be of type javat, but elements of coll must be under
  cljt. cljt should be a subtype of javat (the same or more specific).

  *Temporary hack*
  into-array-syn is exactly the syntax to put as the first argument to into-array.
  Calling resolve on this syntax should give the correct class."
  ([cljt coll]
   `(into-array>* '~cljt ~coll))
  ([javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll))
  ([into-array-syn javat cljt coll]
   `(into-array>* '~javat '~cljt ~coll)))

(defmacro non-nil-return 
  "Override the return type of fully qualified method msym to be non-nil.
  Takes a set of relevant arities,
  represented by the number of parameters it takes (rest parameter counts as one),
  or :all which overrides all arities.
  
  eg. ; must use full class name
      (non-nil-return java.lang.Class/getDeclaredMethod :all)"
  [msym arities]
  ((requiring-resolve 'clojure.core.typed.impl/non-nil-return)
   msym arities))

(defmacro nilable-param 
  "Override which parameters in qualified method msym may accept
  nilable values. If the parameter is a parameterised type or
  an Array, this also declares the parameterised types and the Array type as nilable.

  mmap is a map mapping arity parameter number to a set of parameter
  positions (integers). If the map contains the key :all then this overrides
  other entries. The key can also be :all, which declares all parameters nilable."
  [msym mmap]
  ((requiring-resolve 'clojure.core.typed.impl/nilable-param)
   msym mmap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Annotations

(core/defn print-env 
  "During type checking, print the type environment to *out*,
  preceeded by literal string debug-str."
  [debug-str]
  nil)

(defmacro untyped-var
  "Check a given var has the specified type at runtime."
  [varsym typesyn]
  ((requiring-resolve 'clojure.core.typed.impl/untyped-var)
   &form varsym typesyn))

(defmacro ann
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
  [varsym typesyn]
  ((requiring-resolve 'clojure.core.typed.impl/ann)
   &form varsym typesyn))

(defmacro ann-many
  "Annotate several vars with type t.

  eg. (ann-many FakeSearch
                web1 web2 image1 image2 video1 video2)"
  [t & vs]
  ((requiring-resolve 'clojure.core.typed.impl/ann-many)
   t vs))

(defmacro
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
  [& args]
  ((requiring-resolve 'clojure.core.typed.impl/ann-datatype)
   &form args))

(defmacro 
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
  [& args]
  ((requiring-resolve 'clojure.core.typed.impl/ann-record)
   &form args))

(defmacro
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
  [& args]
  ((requiring-resolve 'clojure.core.typed.impl/ann-protocol)
   &form args))

(defmacro
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
  [& args]
  ((requiring-resolve 'clojure.core.typed.impl/ann-interface) args))

(defmacro override-constructor 
  "Override all constructors for Class ctorsym with type."
  [ctorsym typesyn]
  ((requiring-resolve 'clojure.core.typed.impl/override-constructor)
   &form ctorsym typesyn))

(defmacro override-method 
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
  [methodsym typesyn]
  ((requiring-resolve 'clojure.core.typed.impl/override-method)
   &form methodsym typesyn))

(defmacro typed-deps 
  "Declare namespaces which should be checked before the current namespace.
  Accepts any number of symbols. Only has effect via check-ns.
  
  eg. (typed-deps clojure.core.typed.holes
                  myns.types)"
  [& args]
  ((requiring-resolve 'clojure.core.typed.impl/typed-deps)
   &form args))

(defmacro var>
  "Like var, but resolves at runtime like ns-resolve and is understood by
  the type checker. sym must be fully qualified (without aliases).
  
  eg. (var> clojure.core/+)"
  [sym]
  ((requiring-resolve 'clojure.core.typed.impl/var>)
   sym))

(defmacro warn-on-unannotated-vars
  "Allow unannotated vars in the current namespace. 
  
  Emits a warning instead of a type error when checking
  a def without a corresponding expected type.

  Disables automatic inference of `def` expressions.
  
  eg. (warn-on-unannotated-vars)"
  []
  ((requiring-resolve 'clojure.core.typed.impl/warn-on-unannotated-vars)))

(core/defn default-check-config []
  ((requiring-resolve 'typed.clj.checker/default-check-config)))

(core/defn check-form-info 
  "Function that type checks a form and returns a map of results from type checking the
  form.
  
  Options
  - :expected        Type syntax representing the expected type for this form
                     type-provided? option must be true to utilise the type.
  - :type-provided?  If true, use the expected type to check the form.
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
                     of (Map '{:line Int :column Int :file Str} Str).
  - :checked-ast     Returns the entire AST for the given form as the :checked-ast entry,
                     annotated with the static types inferred after checking.
                     If a fatal error occurs, mapped to nil.
  - :beta-limit      A natural integer which denotes the maximum number of beta reductions
                     the type system can perform on a single top-level form (post Gilardi-scenario).
  - :check-config    Configuration map for the type checker. (See corresponding option for `check-ns`)
  
  Default return map
  - :ret             TCResult inferred for the current form
  - :out-form        The macroexpanded result of type-checking, if successful. 
  - :result          The evaluated result of :out-form, if any.
  - :ex              If an exception was thrown during evaluation, this key will be present
                     with the exception as the value.
  DEPRECATED
  - :delayed-errors  A sequence of delayed errors (ex-info instances)
  - :profile         Use Timbre to profile the type checker. Timbre must be
                     added as a dependency. Must use the \"slim\" JAR."
  [form & {:as opt}]
  ((requiring-resolve 'typed.clj.checker/check-form-info) form (or opt {})))

(core/defn check-form*
  "Function that takes a form and optional expected type syntax and
  type checks the form. If expected is provided, type-provided?
  must be true.
  
  Takes same options as check-form-info, except 2nd argument is :expected,
  3rd argument is :type-provided?, and subsequent keys in opt will be merged over
  them."
  ([form] ((requiring-resolve 'typed.clj.checker/check-form*) form))
  ([form expected] ((requiring-resolve 'typed.clj.checker/check-form*) form expected))
  ([form expected type-provided? & opt]
   (apply (requiring-resolve 'typed.clj.checker/check-form*) form expected type-provided? opt)))

; cf can pollute current type environment to allow REPL experimentation
(defmacro cf
  "Takes a form and an optional expected type and
  returns a human-readable inferred type for that form.
  Throws an exception if type checking fails.

  Do not use cf inside a typed namespace. cf is intended to be
  used at the REPL or within a unit test. Note that testing for
  truthiness is not sufficient to unit test a call to cf, as nil
  and false are valid type syntax.

  cf preserves annotations from previous calls to check-ns or cf,
  and keeps any new ones collected during a cf. This is useful for
  debugging and experimentation. cf may be less strict than check-ns
  with type checker warnings.
  
  eg. (cf 1) 
      ;=> Long

      (cf #(inc %) [Number -> Number])
      ;=> [Number -> Number]"
   ([form] `(check-form* '~form))
   ([form expected] `(check-form* '~form '~expected)))

(core/defn check-ns-info
  "Same as check-ns, but returns a map of results from type checking the
  namespace.

  Options
  - :type-provided?  If true, use the expected type to check the form
  - :profile         Use Timbre to profile the type checker. Timbre must be
  added as a dependency. Must use the \"slim\" JAR.
  - :file-mapping    If true, return map provides entry :file-mapping, a hash-map
  of (Map '{:line Int :column Int :file Str} Str).
  - :check-deps      If true, recursively type check namespace dependencies.
  Default: true

  Default return map
  - :delayed-errors  A sequence of delayed errors (ex-info instances)"
  ([] ((requiring-resolve 'typed.clj.checker/check-ns-info)))
  ([ns-or-syms & opt]
   (apply (requiring-resolve 'typed.clj.checker/check-ns-info) ns-or-syms opt)))

(core/defn check-ns
  "Type check a namespace/s (a symbol or Namespace, or collection).
  If not provided default to current namespace.
  Returns a true value if type checking is successful, otherwise
  throws an Exception.

  Do not use check-ns within a checked namespace.
  It is intended to be used at the REPL or within a unit test.
  Suggested idiom for clojure.test: (is (check-ns 'your.ns))
  
  Keyword arguments:
  - :trace         If true, print some basic tracing of the type checker
                   Default: nil
  - :check-config   Configuration map for the type checker.
    - :check-ns-dep  If `:recheck`, always check dependencies.
                     If `:never`, ns dependencies are ignored.
                     #{:recheck :never}
                     Default: :never
    - :unannotated-def   If `:unchecked`, unannotated `def`s are ignored
                         and their type is not recorded.
                         If `:infer`, unannotated `def`s are inferred by their
                         root binding and the type is recorded in the type environment.
                         #{:unchecked :infer}
                         Also applies to `defmethod`s on unannotated `defmulti`s.
                         Default: :infer
    - :unannotated-var   If `:unchecked`, unannotated vars are given an *unsound*
                         annotation that is used to statically infer its type
                         based on usages/definition (see `infer-unannotated-vars`).
                         If `:any`, usages of unannotated vars are given type `Any` (sound).
                         If `:error`, unannotated vars are a type error (sound).
                         #{:unchecked :any :error}
                         Default: :error
    - :unannotated-arg   (Not Yet Implemented)
                         If `:unchecked`, unannotated fn arguments are given an *unsound*
                         annotation that is used to statically infer its argument types
                         based on definition.
                         If `:any`, unannotated fn arguments are give type `Any` (sound).
                         #{:unchecked :any}
                         Default: :any
    - :check-form-eval   Configures when to evaluate a form relative to type checking it.
                         If :never, don't evaluate individual forms as part of type checking.
                         Avoids side effects during expansion and analysis.
                         If :before, evaluate entire individual forms before type checking, ignoring
                         the Gilardi scenario. Avoids side effects during expansion and analysis.
                         If :after, evaluate individual forms after type checking, respecting the
                         Gilardi scenario.
                         Default: :never
    - :check-ns-load     If :require-before-check, `require` all checked namespaces before checking.
                         If :never, don't load files before checking.
                         Default: :require-before-check

  Removed:
  - :profile       If true, use Timbre to profile the type checker. Timbre must be
                   added as a dependency. Must use the \"slim\" JAR.
                   Default: nil


  If providing keyword arguments, the namespace to check must be provided
  as the first argument.

  Bind clojure.core.typed.util-vars/*verbose-types* to true to print fully qualified types.
  Bind clojure.core.typed.util-vars/*verbose-forms* to print full forms in error messages.
  
  eg. (check-ns 'myns.typed)
      ;=> :ok
     
      ; implicitly check current namespace
      (check-ns)
      ;=> :ok"
  ([] ((requiring-resolve 'typed.clj.checker/check-ns)))
  ([ns-or-syms & {:as opt}]
   ((requiring-resolve 'typed.clj.checker/check-ns) ns-or-syms opt)))

(core/defn check-ns2 
  ([] ((requiring-resolve 'typed.clj.checker/check-ns2)))
  ([ns-or-syms & {:as opt}]
   ((requiring-resolve 'typed.clj.checker/check-ns2) ns-or-syms opt)))

;(ann statistics [(Coll Symbol) -> (Map Symbol Stats)])
(core/defn statistics 
  "Takes a collection of namespace symbols and returns a map mapping the namespace
  symbols to a map of data"
  [nsyms]
  ((requiring-resolve 'clojure.core.typed.impl/statistics) nsyms))

; (ann var-coverage [(Coll Symbol) -> nil])
(core/defn var-coverage 
  "Summarises annotated var coverage statistics to *out*
  for namespaces nsyms, a collection of symbols or a symbol/namespace.
  Defaults to the current namespace if no argument provided."
  ([] (var-coverage *ns*))
  ([nsyms-or-nsym]
   ((requiring-resolve 'clojure.core.typed.impl/var-coverage) nsyms-or-nsym)))

(core/defn envs
  "Returns a map of type environments, according to the current state of the
  type checker.

  Output map:
  - :vars      map from var symbols to their verbosely printed types
  - :aliases   map from alias var symbols (made with defalias) to their verbosely printed types
  - :special-types  a set of Vars that are special to the type checker (like Any, U, I)"
  []
  ((requiring-resolve 'clojure.core.typed.impl/envs)))

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
  [& {:keys [ns strategy] :as config
      :or {strategy :compile
           ns *ns*}}]
  ((requiring-resolve 'clojure.core.typed.impl/prepare-infer-ns) config))

(core/defn refresh-runtime-infer 
  "Clean the current state of runtime inference.
  Will forget the results of any tests on instrumented code."
  []
  ((requiring-resolve 'clojure.core.typed.impl/refresh-runtime-infer)))

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
  (apply (requiring-resolve 'clojure.core.typed.impl/runtime-infer) kws))

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
  (apply (requiring-resolve 'clojure.core.typed.impl/spec-infer) kws))

(core/defn ^:internal register!
  "Internal -- Do not use"
  []
  ((requiring-resolve 'clojure.core.typed.current-impl/register-clj!)))

(defmacro pred 
  "Generate a flat (runtime) predicate for type that returns true if the
  argument is a subtype of the type, otherwise false.

  The current type variable and dotted type variable scope is cleared before parsing.
  
  eg. ((pred Number) 1)
      ;=> true"
  [t]
  ((requiring-resolve 'clojure.core.typed.impl/pred)
   &form t))

(defmacro cast
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
  ([t x] ((requiring-resolve 'clojure.core.typed.impl/cast) &form t x {}))
  ([t x opt] ((requiring-resolve 'clojure.core.typed.impl/cast) &form t x opt)))

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
   ((requiring-resolve 'clojure.core.typed.impl/infer-unannotated-vars) (ns-name nsym-or-ns))))

;============================================================
; Define clojure.core typed wrappers below here to ensure we don't use them above
; thus dynaload as lazily as possible.
;============================================================

(when (= "true" (#?(:cljr Environment/GetEnvironmentVariable :default System/getProperty) "clojure.core.typed.deprecated-wrapper-macros"))
  (load "typed/deprecated_wrapper_macros"))

;;TODO make typing rule
(defmacro dotimes
  "Like clojure.core/dotimes, but with optional annotations.

  If annotation for binding is omitted, defaults to Int.
  
  eg. (dotimes [_ 100]
        (println \"like normal\"))

      (dotimes [x :- Num, 100.123]
        (println \"like normal\" x))"
  [bindings & body]
  ((requiring-resolve 'clojure.core.typed.impl/dotimes) bindings body))

(import-m/import-macros clojure.core.typed.macros
  [def fn loop let ann-form tc-ignore defprotocol
   when-let-fail defn atom ref])
