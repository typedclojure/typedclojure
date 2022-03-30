# Minimal project

This project demonstrates how to type check Clojure[Script] files with Typed Clojure.

See [deps.edn](deps.edn) for required dependencies.

## Usage

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; type check a Clojure file
user=> (t/check-ns-clj 'typed-example.minimal)
;; type check a ClojureScript file
user=> (t/check-ns-cljs 'typed-example.minimal)
;; In a CLJ REPL, this is the same as check-ns-clj -- in CLJS JVM, similar to check-ns-cljs.
user=> (t/cns 'typed-example.minimal)
```
