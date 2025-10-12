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
