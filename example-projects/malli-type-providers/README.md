# Malli type providers

This project demonstrates how Typed Clojure can automatically convert Malli schemas to use for type checking.

The example namespace [typed-example.malli-type-providers](src/typed_example/malli_type_providers.cljc)
uses `malli.core/=>` to register a normal malli schema and then Typed Clojure uses it as an expected type to both
check and infer a var.

```clojure
(m/=> foo [:=> [:cat :int] :int])
(defn foo [t] (inc t))
(foo 1)
```

The namespace [typed-example.malli-extensible](src/typed_example/malli_extensible.cljc) shows how
Typed Clojure can be taught about new schemas.

The namespace [typed-example.malli-global-registry](src/typed_example/malli_global_registry.cljc) shows 
how to use Malli's global registry with Typed Clojure.

## Usage

See [deps.edn](deps.edn) for required dependencies.

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; even though there are no explicit Typed Clojure annotations
;; the namespace still type checks by converting Malli schemas to Typed Clojure types.
user=> (t/check-ns-clj 'typed-example.malli-type-providers)
user=> (t/check-ns-clj 'typed-example.malli-extensible)
```
