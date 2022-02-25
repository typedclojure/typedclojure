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
