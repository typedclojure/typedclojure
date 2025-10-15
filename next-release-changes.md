- BREAKING: Namespaces now require `:typed.clojure` metadata to be checked by `check-ns`
  - An automated script can upgrade your files
    0. ensure your source files are backed up
    1. set JVM property `-Dtyped.clojure.preserve-check-ns-after-opt-in=true`
    2. run the checker as usual
    3. files that would previously have been checked will be updated with the new metadata on-disk
    4. the JVM property can be removed
- BREAKING: remove `clojure.core.typed/var>`
  - use `clojure.core/requiring-resolve`
- BREAKING: remove `clojure.core/typed-deps`
  - no longer useful
- Change how exceptions and type errors are returned from `check-{ns,form}-info`
  - `:type-errors` is a non-empty vector of maps describing type errors
  - `:ex` is a fatal error
- Improve local inference involving refined locals passed to `clojure.core/class`
- Fix https://github.com/typedclojure/typedclojure/issues/135
  - Improve path-type to handle safe downcast from t/Map to t/HMap
- BREAKING: Rename occurrence typing "filter" terminology to "proposition"
  - Documentation syntax: `:doc/filter-syntax` → `:doc/proposition-syntax`
  - Type syntax changes in `t/HVec`, `t/HSeq`, `t/HList`, `t/HSequential`:
    - `:filter-sets` → `:proposition-sets`
  - Function type syntax: `:filters` → `:propositions`
  - TCResult unparsing: `:filter-set` → `:proposition-set`
  - Runtime functions: `print-filterset` → `print-propositionset`
- Internal: Rename occurrence typing implementation records and protocols
  - `typed.cljc.checker.filter_rep` → `typed.cljc.checker.proposition_rep`
  - `typed.cljc.checker.filter_ops` → `typed.cljc.checker.proposition_ops`
  - `typed.cljc.checker.impl_protocols.IFilter` → `typed.cljc.checker.impl_protocols.IProposition`
  - `typed.cljc.checker.impl_protocols.IFilterSet` → `typed.cljc.checker.impl_protocols.IPropositionSet`
  - `typed.cljc.checker.filter_rep.FilterSet` → `typed.cljc.checker.proposition_rep.PropositionSet`
  - `typed.cljc.checker.filter_rep.BotFilter` → `typed.cljc.checker.proposition_rep.BotProposition`
  - `typed.cljc.checker.filter_rep.TopFilter` → `typed.cljc.checker.proposition_rep.TopProposition`
  - `typed.cljc.checker.filter_rep.NoFilter` → `typed.cljc.checker.proposition_rep.NoProposition`
  - `typed.cljc.checker.filter_rep.TypeFilter` → `typed.cljc.checker.proposition_rep.TypeProposition`
  - `typed.cljc.checker.filter_rep.NotTypeFilter` → `typed.cljc.checker.proposition_rep.NotTypeProposition`
  - `typed.cljc.checker.filter_rep.AndFilter` → `typed.cljc.checker.proposition_rep.AndProposition`
  - `typed.cljc.checker.filter_rep.OrFilter` → `typed.cljc.checker.proposition_rep.OrProposition`
  - `typed.cljc.checker.filter_rep.ImpFilter` → `typed.cljc.checker.proposition_rep.ImpProposition`