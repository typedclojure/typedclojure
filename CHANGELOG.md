# 1.2.0 (2023/11/05)

- introduce `t/Instance` and `t/Satisfies`
  - `t/Instance` provides classes with invariant type parameters with a common super type
    - e.g., `(t/Instance Comparable)` is a supertype of `(Comparable Any)`
  - similar for `t/Satisfies`, but for protocols
  - helpful for the return filters of `instance?` and `satisfies?`
    - i.e., we learn `(t/Instance Comparable)` if `(instance? Comparable ..)`, not `(Comparable Any)`
- add Class <: Protocol, DataType <: Protocol cases to constraint generation
- fix Class <: Protocol subtyping case
- assert that annotation is required when extending invariant protocols with deftype
- fix intersection simplication after change in 1.1.5
  - `(I Int (Nilable Int)) => Int`, not `(Nilable Int)`
- resolve first argument to `override-class` at runtime
- improve Java 21 support
  - annotate `java.util.SequencedCollection` and extend in `java.util.List`
- Breaking: remove `t/Var2`, change `clojure.lang.Var to take only 1 type parameter
  - use `t/AnyVar` for `(t/Var2 t/Nothing t/Any)`
- Deprecate `t/Var1`
  - use `t/Var`
- Breaking: remove `t/Ref2`, change `clojure.lang.Ref to take only 1 type parameter
  - use `t/AnyRef` for `(t/Ref2 t/Nothing t/Any)`
- Deprecate `t/Ref1`
  - use `t/Ref`
- Breaking: remove `t/Agent2`, change `clojure.lang.Agent to take only 1 type parameter
  - use `t/AnyAgent` for `(t/Agent2 t/Nothing t/Any)`
- Deprecate `t/Agent1`
  - use `t/Agent`
- Breaking: remove `t/Atom2`, change `clojure.lang.Atom to take only 1 type parameter
  - use `t/AnyAtom` for `(t/Atom2 t/Nothing t/Any)`
- Deprecate `t/Atom1`
  - use `t/Atom`
- Breaking: remove `t/Volatile2`, change `clojure.lang.Volatile to take only 1 type parameter
  - use `t/AnyVolatile` for `(t/Volatile2 t/Nothing t/Any)`
- add `t/AnySeqable` and `t/AnyNilableNonEmptySeq`
- don't use `resolve` to qualify first argument of `t/ann-protocol`
  - just qualify based on current namespace aliases
  - allows for annotations on protocols without a corresponding `defprotocol`
- add clj-kondo hooks for most public macros

# 1.1.5 (2023/10/25)

- introduce the kind of Types
  - `t/Type` is the kind of all types
  - can constrain by wrapping parens:
    - `(t/Type :< upper :> lower)`
- combine type and dotted variable environments
- Breaking: nesting dotted variable expansions are now disallowed
  - since the dotted variable is scoped as a regular var before
    the dots and shadows the dotted variable
  - e.g., `(t/All [b :..] ['[b :.. b] :.. b -> t/Any])`
- scope dotted variables as kind `(t/* t/Type)`
- check bounds when instantiating type functions and polymorphic types
- add t/Match
  - returns the result of the first (pattern) matching clause
```clojure
(t/Match nil nil :-> nil) ;=> nil
(t/Match t/Int
         nil :-> nil
         t/Int :-> t/Bool) ;=> t/Bool
(t/Match (t/Seqable t/Num)
         [[E :< t/Int]] (t/Seqable E) :-> '[':first E]
         [E] (t/Seqable E) :-> '[':second E])
;=> '[':second t/Num]"
```
- nil <: (ExactCount 0)
- add `t/SeqOn`
  - returns the return type of `seq` for given argument type
  - `(t/SeqOn nil) => nil`
  - `(t/SeqOn '[1 2]) => (t/HSeq :types ['1 '2])`
  - `(t/SeqOn (t/Seqable t/Any)) => (t/NilableNonEmptyASeq t/Any)`
- change `clojure.core/seq` to use `t/SeqOn`
- add new object path element `Seq`, which represents the result of calling `clojure.core/seq`
  - replaces old filters on `clojure.core/seq` annotation
- `cc/butlast` returns a nilable non-empty seq
- replacements in `override-class` are more intuitive
  - just provide replacements for the `clojure.core/bases` instead of `clojure.core/supers`
  - all ancestors must agree on their type parameters, like in Java
    - same class cannot extend both `(Seqable A)` and `(Seqable B)` transitively
        - must have a canonical list of arguments for each ancestor
- Fix [#121](https://github.com/typedclojure/typedclojure/issues/121): clojure.core.typed/envs was broken
- Fix [#122](https://github.com/typedclojure/typedclojure/issues/122): cyclic dependencies when using clojure.core.typed/ann without loading typed.clojure
- bump to clojure 1.12.0-alpha5, add jdk 21 to test matrix
- fix simplification of intersected RClass's by respecting variance
- fix overlap calculation of RClass's

# 1.1.4 (2023/10/09)

- internal: remove PolyDots
- `cc/keyword` can accept nil as first arg
- `cc/derive` returns a Hierarchy
- record current expression in TCResult's for future error message improvements
- support `:variance` inference on `t/TFn` parameters
  - `(TFn [x] x) == (TFn [[x :variance :covariant]] x)`
- `cc/fn` expressions now additionally infer as `t/Fn`
- Breaking: `t/TFn` variable bounds now only has variables to the left of it in scope, rather
  than all variables being bound simultaneously in all bounds
  - allowed: `(TFn [y [x :< y]] t/Any)`
  - not allowed: `(TFn [[x :< y] y] t/Any)`
- add `t/Volatile{2}`, support `cc/volatile!` and `cc/vreset!`
- fix `{r}subseq` annotations
- add invariant type parameter to clojure.lang.Sorted
- make sorted maps and sets invariant in their keys
- introduce t/Comparable, update base env with new ancestors
- fix heterogeneous vector invocation
  - only 1 argument is allowed, now 2 args is type error
  - upcast to t/Vec when argument is not a specific integer, previously type error
- fix parsing of `t/Get` optional 3rd argument (default type)
- fix return of assoc/dissoc on unknown keys
- annotate `group-by` to return non-empty vector groups
- Breaking: change `clojure.lang.Seqable` and `cljs.core/ISeqable` to take a Seq as type parameter
  - migration: change `(clojure.lang.Seqable x)` => `(t/Seqable x)`
- support `t/Merge` of type variables

# 1.1.3 (2023/09/19)

- make `assoc` support less unsound
  - can still assoc bad keys onto a sorted map and bad vals onto a record

# 1.1.2 (2023/09/18)

- improve `clojure.core/reductions` annotation
  - 2-arity must support zero-args on 1 arg
    - except when input collection is non-empty
  - add support for `reduced` values
  - return seq is always non-empty
- fix `clojure.core/reduce` annotation
  - result of calling first arg with zero-args does not unwrap `reduced`
  - use one type variable in 2-arity
- annotate `clojure.core/keep` transducer arity
- `clojure.core/keep` never returns nil, update annotation appropriately
- annotate transducers for `filter`, `remove`, `{take,drop}-while`, `partition-all`, `distinct`,
  `interpose`, `{map,keep}-indexed`, 
- return non-empty seq types in core annotations if infinite or known non-empty
- `keep` accepts `Seqable`, not just `Coll`
- annotate 1-arity of `drop-last`
- annotate `keyword` 1-arity to accept a var
- annotate clojure.core vars: `iteration`, `partitionv`, `parse-{uuid,boolean,double,long}`, `update-{keys,vals}`, `abs`, `NaN?`, `infinite?`, `*repl*`, `replace`,
  `partition-by`, `cat`, `dedupe`, `random-sample`, `eduction`, `tagged-literal{?}`, `reader-conditional{?}`, `unreduced`, `halt-when`, `ensure-reduced`, `completing`,
  `transduce`, `sorted-map-by`, `rational?`, `float?`, `{r}subseq`
- clojure.math annotations
- annotate 12 arities of `comp`
- add type parameter to clojure.lang.Sorted
- support seqables of map entries in `keys` and `vals`
- fix return type of `peek` in most general case (nilable return)
- iteratively check symbolic closures against types with invariant type variables
  - enables inference of:
    - `(reduce (fn [a b] (+ a b)) [1])`
    - `(comp (fn [y] y) (fn [x] x))`
- introduce wildcard type `t/Infer`, like `t/Any` but downcasted when checked against a more specific type
- don't recreate types in internal folding when types are unchanged after walking
- print symbolic closures as their most specific function type
  - `(fn [x] x)` prints as `[Nothing :-> Nothing]`
  - `(map (fn [x] x))` prints as `(Transducer Nothing Nothing)`
- don't mention expected type in error messages if it's `t/Infer`
- use `clojure.core.typed` alias in current namespace to shorten types (rather than just `typed.clojure`)
- better support for passing polymorphic functions to other polymorphic functions
  - e.g., `(into [] (map identity) [1])`
- support checking a `fn` against a union type
- introduce regex ops `t/*`, `t/+`, `t/alt`, `t/?`, `t/cat`
  - syntax only enabled in function type arguments for now
  - when expandable to existing types, syntactic sugar for `IFn`. e.g.,
    - `[(t/cat a b) :-> t/Any] => [a b :-> t/Any]`
    - `[(t/cat a b) :* :-> t/Any] => [(t/HSequential [a b] :repeat true) <* :-> t/Any]`
    - `[(t/cat a b) :.. c :-> t/Any] => [(t/HSequential [a b] :repeat true) <... c :-> t/Any]`
    - `[(t/* a) :-> t/Any] => [a :* :-> t/Any]`
    - `[(t/? a) :-> t/Any] => (IFn [:-> t/Any] [a :-> t/Any])`
    - `[(t/+ a) :-> t/Any] => [a :+ :-> t/Any] => [a a :* :-> t/Any]`
    - `[(t/alt (t/cat a) (t/cat a b)) :-> t/Any] => (t/IFn [a :-> t/Any] [a b :-> t/Any])`
  - Otherwise, function type will still parse, but may not be callable until more support is added.
    - please report failing usages
- support `clojure.core/update`

# 1.1.1 (2023/08/16)

- infer `(typed.clojure/fn ...)` as symbolic closure if unannotated
- add [symbolic execution guide](example-projects/symbolic-guide/README.md)

# 1.1.0 (2023/08/16)

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

# 1.0.32 (2022/09/17)

- [typedclojure#63](https://github.com/typedclojure/typedclojure/issues/63): add missing validation in heterogeneous type parsing
- type check `(merge)` as `nil`
- add new type `t/Merge`
  - same semantics as `clojure.core/merge` but at the type level
  - eg., `(t/Merge '{:a t/Int :b t/Bool} '{:a t/Bool :c t/Str}) => '{:a t/Bool :b t/Bool :c t/Str}`

# 1.0.31 (2022/06/11)

- don't check namespace if `^:typed.clojure/ignore` or `^{:typed.clojure {:ignore true}}` meta
  - eg., `(ns ^:typed.clojure/ignore foo)`
- introduce `typed.clojure/check-dir-clj{s}` for type checking directories of code
- introduce `typed.clojure.main` for command line usage
- improve `^::t/dbg` output
- fix `[:maybe]` case in Malli schema to static type translation
- simplify malli schemas `[:and s1 s2 s3 ...]` to `s1` in type translation to avoid tricky subtyping cases

# 1.0.30 (2022/05/04)

- new type syntax
  - `^:fake-quote 'TYPE` <=> `TYPE`
- new metadata annotations (in order of execution if combined)
  - `^{::t/- TYPE} EXP` <=> `(t/ann-form EXP TYPE)`
  - `^{::t/inst [...]} EXP` <=> `(t/inst EXP ...)`
  - `^{::t/unsafe-cast TYPE} EXP` checks EXP with no expected type and unsafely infers type TYPE
  - `^::t/ignore EXP` <=> `(t/tc-ignore EXP)`
  - `^::t/dbg EXP` prints type info on EXP
- add `typed.clojure/doc-clj{s}` for documentation snippets and type lookup

# 1.0.29 (2022/05/01)

- fix `clojure.core.typed/envs`

# 1.0.28

- better support malli refs named as strings that contain non-alphanumerics
- add [zero-deps](example-projects/zero-deps) example project for shipping type checked libraries without a Typed Clojure dependency.
- new syntactic sugar for rest arg
  - `[Foo Bar :+ :-> Baz] <=> [Foo Bar Bar :* :-> Baz]`
- new syntactic sugar for optional final arg
  - `[Foo Bar :? :-> Baz] <=> (IFn [Foo :-> Baz] [Foo Bar :-> Baz])`
- fix too many args required in `{min,max}-key`
- support zero args in `disj`
- fix annotation for `clojure.test/run-all-tests`
- if first arg to `merge` is a map, rest can be nilable and still returns a map
- alternative `:..` keyword syntax for dotted rest argument (along with `...` and `:...`)
- fix `t/All` parsing: allow combining `:..` with keyword args: `(t/All [a :.. :named [b])`
- Breaking: change `(t/Assoc c ... c)` type syntax to `(t/Assoc c :.. c)`
- improve type parsing error messages when no type provided before `:{?,*,+}`
- support checking unrolled rest arguments as `:*` function types
  - eg.,
```
(-> (fn ([] 0)
        ([a] (+ a))
        ([a b & more] (apply + a b more)))
    (ann-form [t/Int :* :-> t/Int]))
```
- refine rest type if known to be non-empty
  - eg., `(ann-form (fn [& rst] (first rst)) [t/Int :+ :-> t/Int])`
- ignore filter and object when checking right-hand-side of `set!`
- fix `clojure.core.typed/envs`
  - revealed (fixed) problems in annotations for `sort-by`, `method-sig`, `partition`

# 1.0.27

- support metadata annotations on `clojure.core/fn`
  - `(t/fn [a :- t/Int]) <=> (cc/fn [^{:typed.clojure/- t/Int} a])`
  - `(t/fn [] :- t/Bool) <=> (cc/fn ^{:typed.clojure/- t/Bool} [])`
  - `(t/ann-form (t/fn [a] a) (t/All [x] [x :-> x])) <=> (cc/fn ^{:typed.clojure/- (t/All [x] [x :-> x])} _id [a] a)`
- propagate expected type to fn arguments of invocations
  - can now check: `(fn [p :- [[t/Int :-> t/Int] :-> t/Int]] (p #(inc %)))`
- Breaking: type alias `typed.clojure/Associative` now takes 2 type arguments (before, 3)
  - similar for `cljs.core/IAssociative`, `clojure.lang.Associative`
- Breaking: remove unused type arg from `cc/reduce-kv` annotation
- fix bad annotations for `seqable?`, `indexed?`, `find`
- use more precise line/column information in type parsing errors
- Breaking: rename `t/MapEntry` to `t/AMapEntry` for fn outputs, change `t/MapEntry` for fn inputs
- fix malli->type ref translation
  - Reported by paola pereira in Clojurians Slack #core-typed. Thanks!

# 1.0.26

- add `typed.clojure/cns`
- change `typed.clojure/check-ns-cljs` from macro to function
- separate `clojure.core` and `cljs.core` type rules
- add `minimal-clj` example project to test typed.clj.checker in isolation without cljs deps
- add `clojure.core/defprotocol` rule for clj{s}
- add `cljs.core/implements?` rule for cljs
- fix filter checking for composite predicates by checking filter after inferring body filter
  - enables eg., `(defn sym-or-kw? [a] (or (symbol? a) (keyword? a)))`
- Breaking: remove `:flow` filters
  - Andrew M Kent (@pnwamk) figured out this was redundant with `:filters` around 2015.
- add `typed.clojure.jvm` for jvm annotations
  - add new op `override-class` -- see `typed.ann.clojure.jvm` for base environment using
- support `clojure.core/reify` in Clojure checker
- support `clojure.core/satisfies?` in Clojure checker
- support `:extend-via-metadata` (and other kw options) in defprotocol wrapper

# 1.0.25

- add `typed.clojure/check-ns`
- `typed.clojure/check-ns{,-clj,cljs}` no longer require the namespaces being checked to depend on a Typed Clojure namespace

# 1.0.24

- support more malli -> type conversions

# 1.0.23

- make malli schema provider extensible
  - see `typed.malli.schema-to-type/register-malli->type-extension`
- add spec1 type providers

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
