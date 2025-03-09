- BREAKING: Namespaces now require `:typed.clojure` metadata to be checked by `check-ns`
  - An automated script can upgrade your files
    0. ensure your source files are backed up
    1. set JVM property `-Dtyped.clojure.preserve-check-ns-after-opt-in=true`
    2. run the checker as usual
    3. files that would previously have been checked will be updated with the new metadata on-disk
    4. the JVM property can be removed
