;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; experimental
(ns ^{:doc "Wrapper macros that do the right thing for each platform."}
  typed.clojure
  (:refer-clojure :exclude [type defprotocol #_letfn fn loop dotimes let for doseq
                            defn atom ref cast])
  (:require #?(;; not loadable in self hosted CLJS, otherwise always needed for
               ;; CLJ AOT compilation compatibility
               :clj clojure.core.typed
               ;; for self hosted CLJS normal :require's from .clj/c files. for
               ;; .clj{s,c} files, loaded via :require-macros in typed/clojure.cljs.
               :cljs cljs.core.typed)
            [clojure.core.typed.macros :as macros]))

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
  (macros/platform-case
    :clj `(clojure.core.typed/ann ~varsym ~typesyn)
    :cljs `(cljs.core.typed/ann ~varsym ~typesyn)))

(defmacro ann-many [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-many ~@args)
    :cljs (throw (ex-info "ann-many not yet implemented in CLJS" {}))))

(defmacro defalias [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/defalias ~@args)
    :cljs `(cljs.core.typed/defalias ~@args)))

(defmacro inst [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/inst ~@args)
    :cljs `(cljs.core.typed/inst ~@args)))

#?(:clj
   (defmacro inst-ctor [& args]
     (macros/platform-case
       :clj `(clojure.core.typed/inst ~@args)
       :cljs (throw (ex-info "inst-ctor does not applicable in CLJS" {})))))

(defmacro declare-datatypes [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-datatypes ~@args)
    :cljs (throw (ex-info "declare-datatypes not yet implemented in CLJS" {}))))

(defmacro declare-protocols [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-protocols ~@args)
    :cljs (throw (ex-info "declare-protocols not yet implemented in CLJS" {}))))

(defmacro declare-alias-kind [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-alias-kind ~@args)
    :cljs (throw (ex-info "declare-alias-kind not yet implemented in CLJS" {}))))

(defmacro declare-names [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/declare-names ~@args)
    :cljs (throw (ex-info "declare-names not yet implemented in CLJS" {}))))

#?(:clj
   (defmacro non-nil-return [& args]
     (macros/platform-case
       :clj `(clojure.core.typed/non-nil-return ~@args)
       :cljs (throw (ex-info "non-nil-return not applicable in CLJS" {})))))

#?(:clj
   (defmacro nilable-param [& args]
     (macros/platform-case
       :clj `(clojure.core.typed/nilable-param ~@args)
       :cljs (throw (ex-info "nilable-param not applicable in CLJS" {})))))

(defmacro def [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/def ~@args)
    :cljs `(cljs.core.typed/def ~@args)))

(defmacro fn [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/fn ~@args)
    :cljs `(cljs.core.typed/fn ~@args)))

(defmacro loop [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/loop ~@args)
    :cljs `(cljs.core.typed/loop ~@args)))

(defmacro ann-form [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/ann-form ~@args)
    :cljs `(cljs.core.typed/ann-form ~@args)))

(defmacro tc-ignore [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/tc-ignore ~@args)
    :cljs `(cljs.core.typed/tc-ignore ~@args)))

(defmacro defprotocol [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/defprotocol ~@args)
    :cljs `(cljs.core.typed/defprotocol ~@args)))

(defmacro defn [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/defn ~@args)
    :cljs `(cljs.core.typed/defn ~@args)))

(defmacro atom [& args]
  (macros/platform-case
    :clj `(clojure.core.typed/atom ~@args)
    :cljs `(cljs.core.typed/atom ~@args)))

#?(:clj
   (defmacro ref [& args]
     (macros/platform-case
       :clj `(clojure.core.typed/ref ~@args)
       :cljs (throw (ex-info "ref does not exist in CLJS" {})))))

;; checker ops

#?(:clj
   (defn check-ns-clj
     "Type check a Clojure namespace/s (a symbol or Namespace, or collection).
     If not provided default to current namespace based on clojure.core/*ns*.
     Returns a true value if type checking is successful, otherwise
     throws an Exception.

     Do not use check-ns-clj within a checked namespace.
     It is intended to be used at the REPL or within a unit test.
     Suggested idiom for clojure.test: (is (check-ns-clj 'your.ns))

     Keyword arguments:
     - :collect-only  If true, collect type annotations but don't type check code.
     Useful for debugging purposes.
     Default: nil
     - :trace         If true, print some basic tracing of the type checker
     Default: nil
     - :check-config   Configuration map for the type checker.
     - :check-ns-dep  If `:recheck`, always check dependencies.
     If `:never`, ns dependencies are ignored.
     #{:recheck :never}
     Default: :recheck
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

     Removed:
     - :profile       If true, use Timbre to profile the type checker. Timbre must be
     added as a dependency. Must use the \"slim\" JAR.
     Default: nil


     If providing keyword arguments, the namespace to check must be provided
     as the first argument.

     Bind clojure.core.typed.util-vars/*verbose-types* to true to print fully qualified types.
     Bind clojure.core.typed.util-vars/*verbose-forms* to print full forms in error messages.

     eg. (check-ns-clj 'myns.typed)
     ;=> :ok

     ; implicitly check current namespace
     (check-ns-clj)
     ;=> :ok

     ; collect but don't check the current namespace
     (check-ns-clj *ns* :collect-only true)"
     ([] (clojure.core.typed/check-ns))
     ([ns-or-syms & opt] (apply clojure.core.typed/check-ns ns-or-syms opt)))) 

(defn check-ns-cljs* [& args]
  (apply #?(:clj (requiring-resolve 'cljs.core.typed/check-ns*)
            :cljs cljs.core.typed/check-ns*)
         args))

(defmacro check-ns-cljs [& args]
  (apply #?(:clj (requiring-resolve 'cljs.core.typed/check-ns-expansion-side-effects)
            :cljs cljs.core.typed/check-ns-expansion-side-effects)
         args))

(defn check-ns-cljs-macros [& args]
  (apply #?(:clj (requiring-resolve 'cljs.core.typed/check-ns-macros)
            :cljs cljs.core.typed/check-ns-macros)
         args))
