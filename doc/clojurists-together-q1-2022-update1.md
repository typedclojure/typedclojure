# Typed Clojure Update 1 of 2

The goal of [this project funded by Clojurists Together](https://www.clojuriststogether.org/news/q1-2022-funding-announcement/) is to
(resurrect) support for type checking ClojureScript files in [Typed Clojure](https://github.com/typedclojure/typedclojure).

[Roughly 10,000 lines](https://github.com/typedclojure/typedclojure/compare/25a3433b1c0c05c4f06e2583440713834371d8c8...f56fe2c4eab493ab0ff4661c71e16412008fed9e) of refactoring, improvements, and feature work has culminated to a [working minimal project](https://github.com/typedclojure/typedclojure/tree/f56fe2c4eab493ab0ff4661c71e16412008fed9e/example-projects/minimal) that can check a `.cljc` file in both Clojure and ClojureScript.

The [work](https://github.com/typedclojure/typedclojure/blob/f56fe2c4eab493ab0ff4661c71e16412008fed9e/doc/clojurists-together-q3-2021-update2.md) I completed for the previous Clojurists Together funding for Typed Clojure has been transferred to ClojureScript, as my [proposal](https://www.clojuriststogether.org/news/q1-2022-funding-announcement/) speculated. The [minimal project](https://github.com/typedclojure/typedclojure/tree/f56fe2c4eab493ab0ff4661c71e16412008fed9e/example-projects/minimal) shows off how a type error involving `(clojure.core/inc a)` is presented as if `inc` were a regular function--in Clojure it is inlinable yet Typed Clojure prints `(inc a)` instead of `clojure.lang.Numbers/inc`, and in ClojureScript is it a macro call yet prints `((do inc) a)` instead of a `js*` call (some room for improvement).

A new macros namespace [typed.clojure](https://github.com/typedclojure/typedclojure/blob/f56fe2c4eab493ab0ff4661c71e16412008fed9e/typed/clj.runtime/src/typed/clojure.cljc)
has been created for cross-platform use. Instead of using `clojure.core.typed` or `cljs.core.typed`, you can use `typed.clojure`
and the correct implementation will be chosen automatically. A new namespace was created so then we can (eventually) target self-hosting ClojureScript forcing reader conditionals on users.

The base type environment for both Clojure and ClojureScript has been moved to [typed.ann.clojure](https://github.com/typedclojure/typedclojure/blob/f56fe2c4eab493ab0ff4661c71e16412008fed9e/typed/lib.clojure/src/typed/ann/clojure.cljc). It houses 2400 lines of annotations and serves as a real-world example of how to annotate functions, protocols, and records for multiple platforms using reader conditionals.

Typing rules for macros are now shared across Clojure and ClojureScript implementations. This means the [work](https://github.com/typedclojure/typedclojure/blob/f56fe2c4eab493ab0ff4661c71e16412008fed9e/doc/clojurists-together-q3-2021-update2.md) completed for the previous Clojurists Together project can be transferred to ClojureScript, such as the improved error messages for let-destructuring.
