# typed.clj/spec

<a href='http://typedclojure.org'><img src='images/part-of-typed-clojure-project.png'></a>

<p>
  <a href='https://www.patreon.com/ambrosebs'><img src='../../doc/images/become_a_patron_button.png'></a>
  <a href='https://opencollective.com/typedclojure'><img src='../../doc/images/donate-to-our-collective.png'></a>
</p>

Type-like specs.

## Rationale

Higher-order functions are used frequently in Clojure programs, however
it is often a difficult and ad-hoc process to specify and generatively
test them using Clojure spec.

Typed Clojure has an established syntax for specifying many kinds
of higher-order functions. It was achieved by:

1. determining the primitive concepts used in Clojure (like `Int`, `Any`, `Bool`)
2. providing a metalanguage to manipulate these concepts (like `All`, `TFn`)

Clojure spec already provides similar primitives. The novelty of
`typed.clj/spec` is providing a _metalanguage_ for specs to
enable richer specifications.

## Quickstart

Note: `typed.clj/spec` currently depends on a (git) fork
spec-alpha2 (fixing CLJ-2561 and CLJ-2562). See `deps.edn`
for details.

```clojure
;; require these namespaces
(require '[clojure.alpha.spec :as s]
         '[clojure.alpha.spec.gen :as gen]
         '[typed.clj.spec :as t])

;; start writing polymorphic specs
(s/def ::identity
  #_"Polymorphic type for clojure.core/identity"
  (t/all :binder (t/binder :x (t/bind-tv))
         :body
         (s/fspec :args (s/cat :x (t/tv :x))
                  :ret (t/tv :x))))

;; using normal spec functions to use validate them 
(assert (s/valid? ::identity identity))
(assert (s/valid? ::identity (comp first
                                   (juxt identity identity)
                                   (fn [x] x))))
(assert (not (s/valid? ::identity (fn [x] nil))))

;; start a repl from ./script/repl in this repo
;; and see tests for more interesting specs.
;; eg.,
(require '[typed-test.clj.spec.transducers :as x])

;; is (map inc) a transducer from integer? to integer? (yes)
(assert
  (binding [s/*fspec-iterations* 5]
    (s/valid? (t/tapp ::x/Transducer {:in integer?
                                      :out integer?})
              (map inc))))

;; is (map str) a transducer from integer? to symbol? (no)
(assert (not
          (s/valid? (t/tapp ::x/Transducer {:in integer?
                                            :out symbol?})
                    (map str))))
```

## Tutorial

See [doc/tutorial.md](doc/tutorial.md).

## Examples

See tests for more examples:

- [clojure.core/comp](test/typed_test/clj/spec/comp.clj)
- [Transducers & Reducers](test/typed_test/clj/spec/transducers.clj)
- [clojure.core/identity](test/typed_test/clj/spec/identity.clj)
- [clojure.core/integer?](test/typed_test/clj/spec/integer_huh.clj)
- [clojure.core/memoize](test/typed_test/clj/spec/memoize.clj)
- [clojure.core/reduce](test/typed_test/clj/spec/reduce.clj)

## Releases and Dependency Information

Latest stable release is 1.0.10.

* [All Released Versions](https://clojars.org/typed.clj/spec)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  typed.clj/spec {:mvn/version "1.0.10"}
 ```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

- Note: use `clj -Sresolve` to resolve the `:tag` to a `:sha`

```clj
  typed.clj/spec {:git/url "https://github.com/typedclojure/typedclojure"
                  :deps/root "typed/clj.spec"
                  :tag "1.0.10"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[typed.clj/spec "1.0.10"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>typed.clj</groupId>
  <artifactId>spec</artifactId>
  <version>1.0.10</version>
</dependency>
```

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
