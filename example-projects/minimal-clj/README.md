# Minimal project for checking Clojure

This project demonstrates how to type check Clojure files with Typed Clojure.

See [deps.edn](deps.edn) for required dependencies.

## Usage

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; type check a Clojure file
user=> (t/check-ns-clj 'typed-example.minimal-clj)
;; In a CLJ REPL, this is the same as check-ns-clj
user=> (t/cns 'typed-example.minimal-clj)
```
