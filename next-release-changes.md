- annotate `clojure.core/empty`
- support `(t/Val "string")` in `t/pred`
- Close https://github.com/typedclojure/typedclojure/issues/143
  - correctly scope datatype type variables when checking deftype
  - fix t/Match to work without type variables
- revert bad optimization in analyzer (caught by David Miller during CLR port)
- support Clojure 1.9.0 in analyzers
- add alternative syntax for "or" proposition: (or Props...)
- using requiring-resolve to resolve protocol vars during type checking
  - allows `:as-alias` to be used in annotations
- add clj-kondo hook for `clojure.core.typed/pred`
- add `clojure.string/starts-with?` annotation
- default `:check-config` `:check-ns-dep` to `:never` and fix `:recheck`
- add `{:typed.clojure {:experimental #{:cache}}}` namespace meta to start investigating caching of type checking results
  - currently prints results to screen
- fix verbose printing for composite types
- don't uniquify type variables with verbose printing
  - add new new `:unique-tvars` option to `*verbose-types*` to uniquify type variable names
- namespace-level functions like `check-ns` now `require`s the namespace being checked before type checking it, and does not evaluate individual forms
  - usual `require` behavior applies (only loads if not already loaded), so user is now responsible for reloading type annotations, similar to spec
    - e.g., if you change a `t/ann`, make sure to evaluate the form just as you would an `s/def`
  - this enables future optimizations to type check forms in parallel and integrate type checking with tools.namespace/clj-reload namespace loading hooks
  - for previous behavior, use:
```clojure
(check-ns *ns* :check-config {:check-ns-load :never, :check-form-eval :after})
```
- micro-optimization improvements thanks to Oleksandr Yakushev (@alexander-yakushev)
- use fully-satisfies library for thread-safe variant of requiring-resolve in all submodules
  - add additional typing rule for this function
- restructure `clojure.core.typed` to optimize loading time
  - 80% improvement, also improves `typed.clojure` loading time
  - performance of generated code from `pred` and contracts may be affected because of `requiring-resolve` calls in output
- merged first batch of ClojureCLR specializations to analyzer and runtime, thanks to David Miller (@dmiller)
- pretty printing of Classes by their simple names only supported for classes interned by their simple names
- fix Array <: Seqable subtyping
- refactor tail of subtyping using single-dispatch, thanks to Oleksandr Yakushev (@alexander-yakushev)
- enable parallel checking by default, one thread per top-level form
  - defaults to number of processors
  - configurable via options: `(check-ns *ns* {:max-processors 1})`
- reset lexical type environment when checking ns dependency
- type check `clojure.core/assert` as if `*assert*` is true
- remove `*verbose-{types,forms}*`
  - use `:verbose-{types,forms} true` option
- remove system property `clojure.core.typed.intern-defaliases`
- support `:typed.clojure/ignore true` metadata on macro vars which is
  a hint to the checker that it always expands to ignored forms
- use `(t/cat T1 T2 T3)` to instantiate dotted variables via `t/inst`
  - polymorphic binders with one trailing dotted variable still support the old style of `T1 T2 T3`
- assert `t/Rec` binder must have simple symbols
- BREAKING: remove support for `...` and `:...` syntax in `t/All` binder, `t/IFn`, `t/cat`, and `t/HSequential`
  - now `:..`
- support bounded strings in `t/pred`
```clojure
(t/defalias String1<=10 (t/I t/Str (t/CountRange 1 10)))
(def string1<=10? (t/pred String1<=10))
(is (not (string1<=10? "")))
(is (string1<=10? "012345"))
(is (not (string1<=10? "0123456789ten")))
```
- use `bounded-count` in `t/CountRange` predicates to support infinite collections
```clojure
(is ((t/pred (t/CountRange 1)) (range)))
```
- leaner bytecode in `t/pred` output
- support infinite Names types in `t/pred`
