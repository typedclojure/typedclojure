# Installing Typed Clojure

Typed Clojure is a Clojure library. It should be added as a dependency to your Clojure project. It is split into several subprojects, which you can mix and match depending on your needs.

See the [minimal example project](https://github.com/typedclojure/typedclojure/tree/main/example-projects/minimal) for a complete example.

## Runtime dependencies

Typed Clojure provides [JVM runtime dependencies](https://github.com/typedclojure/typedclojure/blob/main/typed/clj.runtime/README.md#releases-and-dependency-information) as a separate dependency. It provides facilities to record top-level annotations and a public API to run the type checker. It also provides wrapper macros for local annotations.

Typically, this should be added as a runtime dependency.

Typed Clojure can be also introduced without adding a runtime dependency. This is
especially useful for type checking libraries without adding an extra dependency to users of the library. See the [zero-deps example project](https://github.com/typedclojure/typedclojure/tree/main/example-projects/zero-deps). With this setup, the 
[JVM runtime dependencies](https://github.com/typedclojure/typedclojure/blob/main/typed/clj.runtime/README.md#releases-and-dependency-information) should be moved to a dev-time dependency, or be elided as it is a transitive dependency of the checker.

## Dev dependencies

Install the [type checker](https://github.com/typedclojure/typedclojure/blob/main/typed/clj.checker/README.md#releases-and-dependency-information) as a dev dependency. Without this dependency, calling the type checker will fail with an error like this:

```clojure
Could not locate typed/clj/checker__init.class, typed/clj/checker.clj or typed/clj/checker.cljc on classpath.
```
