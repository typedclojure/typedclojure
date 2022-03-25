# 1.0.22

- add `clojure.java.io` annotations
- fix `{pos,neg,nat}-int?` annotations
- rename `typed.clj.malli` submodule to `typed.malli`
- add support for type providers
- add malli var type providers ([demo](example-projects/malli-type-providers))
- Breaking: make `t/def` and `t/defn` never infer their types, and use `t/ann` to register global annotation
  - Rationale: don't allow a namespace's type checking to be dependent on another namespace checking

# 1.0.21

- don't check the `typed.clojure` namespace

# 1.0.20

- add `lib.spec.alpha` lib for spec.alpha annotations
  - includes no-op typing rules for `s/def` and `s/fdef`
- remove outdated hints on type errors
- add :unanalyzed to `typed.cljs.analyzer`
- add `typed.clojure` namespace for cross-platform use
- Breaking: vars representing special types from clojure.core.typed (eg., Any, HMap) have been removed
  - set system property `clojure.core.typed.special-vars=true` to restore
  - rationale: `:refer`ing these vars is not cross-platform. type resolution can
    work without these vars
- Breaking: defalias no longer interns vars
  - set system property `clojure.core.typed.intern-defaliases=true` to restore
  - rationale: poor cross-platform story. follow spec's lead with separate registry.
- Breaking: t/Seqable is now nilable
  - rationale: in practice, t/Seqable is always made nilable in annotations. It's also
    usually in input position, so there's little reason not to broaden. 
    - t/Seqable predates `clojure.core/seqable?`, so now we can annotate as `(t/Pred (t/Seqable t/Any))`
- move shared base env to .cljc file

# 1.0.19

- fix bad `clojure.core/boolean?` annotation

# 1.0.18

- improve destructuring error messages for:
  - clojure.core/fn
  - clojure.core.typed/fn
- support :when/:while/:let in clojure.core/for typing rule
- create type rule for clojure.core/doseq with destructuring and support for all options
- create type rule for clojure.core/defn with improved destructuring error messages
- create type rule for clojure.core/defmethod with improved destructuring error messages
- check inlines before expansion to improve error messages
  - eg., now `clojure.core/inc` is appears in error messages instead of `clojure.lang.RT/inc`
- add `typed.clj.malli` project

# 1.0.17

- move ext.c.c.t typing rules from runtime jar to checker
- add spec{1,2} shim to prepare for spec1 support in typed.clj.spec
- add `clojure.core/for` typing rule with destructuring
- remove dynamic vars from uniquify analyzer pass
- add `clojure.core/let` typing rule
- add custom error message for vector destructuring
- add in-development `typed.clj.generators` namespace to typed.clj.checker
