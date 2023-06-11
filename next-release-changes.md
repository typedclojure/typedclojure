- require expected type for `defmulti`
- enable symbolic closures by default
  - checking is deferred for unannotated local functions until (and only if) they are applied
    - more specifically, until required types for arguments are found
- more precise annotation for `clojure.core.async/pipe`
- removed old annotation macros `clojure.core.typed/{for,doseq}`
  - obsoleted by the improved type inference of the new macro rules for `clojure.core/{for,doseq}`
  - use Java property `clojure.core.typed.deprecated-wrapper-macros=true` to add them back
- global annotations registered via `t/ann`, `t/ann-protocol` etc., are now removed from the
  global type environment if the (Clojure JVM) namespace in which those macros occur is removed or reloaded.
  - eg., you can now delete misspelled `t/ann` simply by correcting it, then refreshing the namespace via tools.namespace.
    the underlying annotation for the misspelled version is removed as if it were a var in the old namespace.
- remove `:collect-only` feature
  - annotations are now collected at runtime, so this concept is now redundant
- add spec for atoms: `typed.spec.clojure.core/atom-spec`
- fix some `:forms` documentation for core aliases
