;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; note: this file is copied into resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime
;; via ./script/regen-kondo.sh
;; the canonical version is in the src folder
(ns ^:no-doc clojure.core.typed.macros
  (:refer-clojure :exclude [type defprotocol fn loop dotimes let for doseq
                            defn atom ref #?(:clj requiring-resolve)])
  (:require [clojure.core :as core]
            [clojure.core.typed.platform-case :refer [platform-case]]
            #?(:clj [io.github.frenchy64.fully-satisfies.requiring-resolve :refer [requiring-resolve]])))

;; TODO move all these macros to typed.clojure
(core/defn typed-sym [&env sym]
  {:pre [(simple-symbol? sym)]}
  (symbol
    (platform-case
      :clj "clojure.core.typed"
      :cljs "cljs.core.typed")
    (name sym)))

(core/defn core-kw [kw]
  (keyword "clojure.core.typed"
           (name kw)))

(core/defn parse-colon
  "Returns a vector of [provided? t args]"
  [fdecl name]
  (if (#{:-} (first fdecl))
    (core/let [[colon t & body] fdecl]
      [true t body])
    [false nil fdecl]))

(defmacro
  ^{:forms '[(def name docstring? :- type? expr)]}
  def
  "Like clojure.core/def with optional type annotations. Registers
  annotations like t/ann.

  NB: in Clojure it not useful refer a macro called `def` as it is a
  special form. Use an alias prefix (eg., `t/def`).

  eg. ;same as clojure.core/def
      (def vname 1)
      
      ;with Number `ann`
      (def vname :- Number 1)

      ;doc
      (def vname
        \"Docstring\"
        :- Long
        1)"
  [name & fdecl]
  (core/let [[docstring fdecl] ((requiring-resolve 'clojure.core.typed.internal/take-when) string? fdecl)
             [provided? t [body :as args]] (parse-colon fdecl 'def)]
    (assert (= 1 (count args)) "Wrong arguments to def")
    `(do
       ~@(when provided?
           [`(~(typed-sym &env 'ann) ~name ~t)])
       (def ~(vary-meta name #(merge
                                %
                                (when docstring
                                  {:doc docstring})))
         ~body))))

(core/defn expand-typed-fn [form]
  (core/let [{:keys [poly fn ann]} ((requiring-resolve 'clojure.core.typed.internal/parse-fn*) form)]
    fn))

(defmacro 
  ^{:forms '[(fn name? [param :- type* & param :- type * ?] :- type? exprs*)
             (fn name? ([param :- type* & param :- type * ?] :- type? exprs*)+)]}
  fn
  "Like clojure.core/fn, but with optional annotations.

  eg. ;these forms are equivalent
      (fn [a] b)
      (fn [a :- Any] b)
      (fn [a :- Any] :- Any b)
      (fn [a] :- Any b)

      ;annotate return
      (fn [a :- String] :- String body)

      ;named fn
      (fn fname [a :- String] :- String body)

      ;rest parameter
      (fn [a :- String & b :- Number *] body)

      ;dotted rest parameter
      (fn [a :- String & b :- Number :.. x] body)

      ;multi-arity
      (fn fname 
        ([a :- String] :- String ...)
        ([a :- String, b :- Number] :- String ...))

      ; polymorphic binder
      (fn :forall [x y z]
        fname 
        ([a :- String] :- String ...)
        ([a :- String, b :- Number] :- String ...))
  "
  [& forms]
  (expand-typed-fn &form))

(defmacro 
  ^{:forms '[(loop [binding :- type?, init*] exprs*)]}
  loop
  "Like clojure.core/loop, and supports optional type annotations.
  Arguments default to a generalised type based on the initial value.

  eg. (loop [a :- Number 1
             b :- (U nil Number) nil]
        ...)"
  [bindings & exprs]
  (core/let [{:keys [ann loop]} ((requiring-resolve 'clojure.core.typed.internal/parse-loop*) `(~bindings ~@exprs))]
    `(do :clojure.core.typed.special-form/special-form
         ~(core-kw :loop)
         {:ann '~ann}
         ~loop)))

(defmacro 
  ^{:forms '[(let [binding :- type?, init*] exprs*)]}
  let
  "Like clojure.core/let but supports optional type annotations.

  eg. (let [a :- Type, b
            a2 1.2]
        body)"
  [bvec & forms]
  (core/let [{:keys [let]} ((requiring-resolve 'clojure.core.typed.internal/parse-let*) (cons bvec forms))]
    let))

(defmacro ann-form
  "Annotate a form with an expected type."
  [form ty]
  form)

(defmacro defprotocol
  "Like defprotocol, but with optional type annotations.

  Omitted annotations default to Any. The first argument
  of a protocol cannot be annotated.

  Add a binder before the protocol name to define a polymorphic
  protocol. A binder before the method name defines a polymorphic
  method, however a method binder must not shadow type variables
  introduced by a protocol binder.

  Return types for each method arity can be annotated.

  Unlike clojure.core/defprotocol, successive methods can
  have the same arity. Semantically, providing multiple successive
  methods of the same arity is the same as just providing the left-most
  method. However the types for these methods will be accumulated into
  a Fn type.
  
  eg. ;annotate single method
  (defprotocol MyProtocol
    (a [this a :- Integer] :- Number))

  ;polymorphic protocol
  (defprotocol [[x :variance :covariant]]
    MyProtocol
    (a [this a :- Integer] :- Number))

  ;multiple types for the same method
  (defprotocol [[x :variance :covariant]]
    MyProtocol
    (a [this a :- Integer] :- Integer
       [this a :- Long] :- Long
       [this a :- Number] :- Number))

  ;polymorphic method+protocol
  (defprotocol [[x :variance :covariant]]
    MyProtocol
    ([y] a [this a :- x, b :- y] :- y))"
  [& body]
  (core/let [{:keys [ann-protocol defprotocol]} ((requiring-resolve 'clojure.core.typed.internal/parse-defprotocol*) body)]
    `(do ~ann-protocol
         (~(typed-sym &env 'tc-ignore)
           ~defprotocol))))

(defmacro tc-ignore 
  "Ignore forms in body during type checking"
  [& body]
  `(do ~@body))

(defmacro when-let-fail 
  "Like when-let, but fails if the binding yields a false value."
  [b & body]
  `(if-let ~b
     (do ~@body)
     (throw (ex-info (str "Expression was nil or false") {:form '~(second b)}))))

(defmacro atom
  ;;copied to typed.clojure
  "Like atom, but with optional type annotations.
  
  Same as (atom (ann-form init t) args*)
  
  eg. (atom 1) : (Atom (Value 1))
      (atom :- Num, 1) : (Atom Num)"
  [& args]
  (core/let [[provided? t args] (parse-colon args 'atom)
             [init & args] args]
    `(core/atom ~(if provided?
                   `(~(typed-sym &env 'ann-form) ~init ~t)
                   init)
                ~@args)))

(defmacro ref
  "Like ref, but with optional type annotations.
  
  Same as (ref (ann-form init t) args*)
  
  eg. (ref 1) : (Ref (Value 1))
      (ref :- Num, 1) : (Ref Num)"
  [& args]
  (core/let [[provided? t args] (parse-colon args 'ref)
             [init & args] args]
    `(core/ref ~(if provided?
                  `(~(typed-sym &env 'ann-form) ~init ~t)
                  init)
               ~@args)))

(defmacro
  ^{:forms '[(defn kw-args? name docstring? attr-map? [param :- type *] :- type exprs*)
             (defn kw-args? name docstring? attr-map? ([param :- type *] :- type exprs*)+)]}
  defn
  "Like defn, but registers annotation with t/ann.

  eg. (defn fname [a :- Number, b :- (U Symbol nil)] :- Integer ...)

  ;annotate return
  (defn fname [a :- String] :- String ...)

  ;multi-arity
  (defn fname 
    ([a :- String] :- String ...)
    ([a :- String, b :- Number] :- Long ...))

  ;polymorphic function
  (defn :forall [x y]
    fname 
    ([a :- x] :- (Coll y) ...)
    ([a :- Str, b :- y] :- y ...))"
  [& args]
  (core/let [{:keys [name args]} ((requiring-resolve 'clojure.core.typed.internal/parse-defn*) args)
             init `(~(typed-sym &env 'fn) ~@args)
             {:keys [reassembled-fn-type]} ((requiring-resolve 'clojure.core.typed.internal/parse-fn*) init)]
    `(~(typed-sym &env 'def) ~name :- ~reassembled-fn-type,
       ~init)))

(defmacro when-bindable-defining-ns [defining-nsym & body]
  ;; don't register type if the defining namespace doesn't exist.
  ;; the only sane semantics for typed.cljc.runtime.env-utils/delay-type.
  `(when-some [defining-ns# (find-ns ~defining-nsym)]
     (binding [*ns* defining-ns#]
       (do ~@body))))
