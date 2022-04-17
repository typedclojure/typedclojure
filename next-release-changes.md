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
