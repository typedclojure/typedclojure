;;   Copyright (c) Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0/)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

(ns typed.clj.checker
  (:require [clojure.core :as core]
            [clojure.core.typed.load-if-needed :refer [load-if-needed]]
            [typed.clj.checker.check-form :as check-form-clj]
            [typed.clj.checker.check-ns :as check-ns-clj]))

(core/defn default-check-config []
  {:check-ns-dep :recheck
   :unannotated-def :infer
   :unannotated-var :error
   :unannotated-multi :error
   #_#_:unannotated-arg :any})

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
  (load-if-needed)
  (check-form-clj/check-form-info form (update opt :check-config #(into (default-check-config) %))))

(core/defn check-form*
  "Function that takes a form and optional expected type syntax and
  type checks the form. If expected is provided, type-provided?
  must be true.
  
  Takes same options as check-form-info, except 2nd argument is :expected,
  3rd argument is :type-provided?, and subsequent keys in opt will be merged over
  them."
  ([form] (check-form* form nil nil))
  ([form expected] (check-form* form expected true))
  ([form expected type-provided? & {:as opt}]
   (load-if-needed)
   (check-form-clj/check-form* form expected type-provided? (update opt :check-config #(into (default-check-config) %)))))

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
  ([] (check-ns-info *ns*))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-clj/check-ns-info ns-or-syms (update opt :check-config #(into (default-check-config) %)))))

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
    - :check-form-eval   Configures when to evaluate a form relative to type checking it.
                         If :never, don't evaluate individual forms as part of type checking.
                         Avoids side effects during expansion and analysis.
                         If :before, evaluate entire individual forms before type checking, ignoring
                         the Gilardi scenario. Avoids side effects during expansion and analysis.
                         If :after, evaluate individual forms after type checking, respecting the
                         Gilardi scenario.
                         Default: :after
    - :check-ns-load     If :require-before-check, `require` all checked namespaces before checking.
                         If :never, don't load files before checking.
                         Default: :never

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
  ([] (check-ns *ns*))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-clj/check-ns ns-or-syms (update opt :check-config #(into (default-check-config) %)))))

(core/defn check-ns2 
  ([] (check-ns2 *ns*))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-clj/check-ns ns-or-syms (update opt :check-config
                                             #(into {:check-ns-dep :never
                                                     :unannotated-def :unchecked
                                                     :unannotated-var :unchecked
                                                     :unannotated-arg :unchecked}
                                                    %)))))

(core/defn check-ns3 
  ([] (check-ns3 *ns*))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-clj/check-ns ns-or-syms (update opt :check-config
                                             #(into {:check-ns-dep :never}
                                                    %)))))

(core/defn check-ns4
  ([] (check-ns4 *ns*))
  ([ns-or-syms & {:as opt}]
   (load-if-needed)
   (check-ns-clj/check-ns ns-or-syms (update opt :check-config
                                             #(into {:check-ns-dep :never
                                                     :check-form-eval :never
                                                     :check-ns-load :require-before-check}
                                                    %)))))
