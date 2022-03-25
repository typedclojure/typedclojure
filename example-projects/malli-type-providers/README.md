# Malli type providers

This project demonstrates how Typed Clojure can automatically convert Malli schemas to use for type checking.

[typed-example.malli-type-providers](example-projects/malli-type-providers/src/typed_example/malli_type_providers.clj)
uses `malli.core/=>` to register a normal malli schema and then Typed Clojure uses it as an expected type to both
check and infer a var.

```clojure
(m/=> foo [:=> [:cat :int] :int])
(defn foo [t] (inc t))
(foo 1)
```

## Usage

See [deps.edn](deps.edn) for required dependencies.

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; even though there are no explicit Typed Clojure annotations
;; the namespace still type checks by converting Malli schemas to Typed Clojure types.
user=> (t/check-ns-clj 'typed-example.malli-type-providers)
```
