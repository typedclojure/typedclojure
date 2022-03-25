# Malli type providers

This project demonstrates 

See [deps.edn](deps.edn) for required dependencies.

## Usage

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; even though there are no explicit Typed Clojure annotations
;; the namespace still type checks by converting Malli schemas to Typed Clojure types.
user=> (t/check-ns-clj 'typed-example.malli-type-providers)
```
