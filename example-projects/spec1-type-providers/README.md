# Spec1 type providers

This project demonstrates how Typed Clojure can automatically convert spec1 specs to use for type checking.

The example namespace [typed-example.spec1-type-providers](src/typed_example/spec1_type_providers.clj)
uses `fdef` to register a normal spec and then Typed Clojure uses it as an expected type to both
check and infer a var.

```clojure
(s/fdef foo :args (s/cat :t int?) int?)
(defn foo [t] (inc t))
(foo 1)
```

The namespace [typed-example.spec1-extensible](src/typed_example/spec1_extensible.clj) shows how
Typed Clojure can be taught about new specs.

## Usage

See [deps.edn](deps.edn) for required dependencies.

```clojure
clj -A:dev
user=> (require '[typed.clojure :as t])
;; even though there are no explicit Typed Clojure annotations
;; the namespace still type checks by converting spec1 specs to Typed Clojure types.
user=> (t/check-ns-clj 'typed-example.spec1-type-providers)
user=> (t/check-ns-clj 'typed-example.spec1-extensible)
```
