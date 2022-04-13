# Zero dependency type checking

This project demonstrates how to type check Clojure files with Typed Clojure
without a runtime dependency on Typed Clojure. This is particularly relevant for libraries
where minimizing dependencies is paramount. While this is possible for ClojureScript files,
its lack of `:as-alias` support makes this painful and thus the following approach is not (yet) recommended.

Here's the hypothetical setup: we want to distribute [typed-example.zero-deps](src/typed_example/zero_deps.cljc)
as a library that has zero dependencies---but we also want to type check it. It has a `reduce`, which cannot be inferred
automatically by Typed Clojure, so a local annotation is needed.

In our [deps.edn](deps.edn) we specify Typed Clojure as a dev dependency and Clojure is our only runtime dependency.

We need a namespace to store global annotations for [typed-example.zero-deps](src/typed_example/zero_deps.cljc).
If we want users of our jar to see these annotations, we use the `src` directory.
If they're internal annotations you don't want to distribute, put them in `test` or
perhaps `dev`. Either way, it won't add a runtime dependency for [typed-example.zero-deps](src/typed_example/zero_deps.cljc).

Let's put [typed-example.zero-deps.typedclojure-ann](src/typed_example/zero_deps/typedclojure_ann.cljc) in the `src` dir
and populate it with global annotations. It also contains a type alias we want to use in local annotations for [typed-example.zero-deps](src/typed_example/zero_deps.cljc).

To automatically load these annotations, we use the `resources` dir to add a [typedclojure_config.cljc](resources/typedclojure_config.cljc) file.
Again, you could use `dev-resources` or `test-resources` if these are internal annotations.

To type check [typed-example.zero-deps](src/typed_example/zero_deps.cljc), make sure `typed.clojure` is loaded.
One way to do this is:
```
((requiring-resolve `t/check-ns-clj))
```
This loads the Typed Clojure runtime, then the
checker is loaded before gathering the annotations in [typed-example.zero-deps.typedclojure-ann](src/typed_example/zero_deps/typedclojure_ann.cljc)
and checking the namespace.

## Usage

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; type check a Clojure file
user=> (t/check-ns-clj 'typed-example.zero-deps)
```
