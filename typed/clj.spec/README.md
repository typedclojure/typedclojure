<!-- DO NOT EDIT! Instead, edit `dev/resources/root-templates/typed/clj.spec/README.md` and run `./script/regen-selmer.sh` -->
# typed.clj.spec

<a href='https://typedclojure.org'><img src='../../doc/images/part-of-typed-clojure-project.png'></a>

<p>
  <a href='https://www.patreon.com/ambrosebs'><img src='../../doc/images/become_a_patron_button.png'></a>
  <a href='https://opencollective.com/typedclojure'><img src='../../doc/images/donate-to-our-collective.png'></a>
</p>

Type-like specs.

## Rationale

Higher-order functions often contain fine-grained dependencies between
their arguments and return values. For example, the identity function requires
its output to exactly match its input.

`typed.clj.spec` enhances the expressivity of spec by giving you
the tools to specify such dependencies directly.
This spec _metalanguage_ treats these
kinds of dependencies as first-class concepts, so you can
specify and generatively test your favourite higher-order functions.

The aim of this library is to combine the expressive power of a type-like syntax
with the pragmatism and versatility of spec.
I'm really excited about the possibilities of bringing together
the advantages of both worlds.

I hope you enjoy using `typed.clj.spec`!

## Quickstart

Either:

1. Add a dependency to `typed.clj.spec` using your favourite
   build tool (see [Releases and Dependency Information](#releases-and-dependency-information)).

2. Clone this repository and run `./script/repl`
   in the `typed/clj.spec` directory to follow along.

To use the generative testing features of `typed.clj.spec`,
also add an explicit dependency to `[org.clojure/test.check "1.0.0"]`
(automatic if cloning this repository).

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

Latest stable release is 1.1.1.

* [All Released Versions](https://clojars.org/org.typedclojure/typed.clj.spec)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  org.typedclojure/typed.clj.spec {:mvn/version "1.1.1"}
```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

- Note: use `clj -Sresolve` to resolve the `:tag` to a `:sha`

```clj
  org.typedclojure/typed.clj.spec
  {:git/url "https://github.com/typedclojure/typedclojure"
   :deps/root "typed/clj.spec"
   :tag "1.1.1"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[org.typedclojure/typed.clj.spec "1.1.1"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>org.typedclojure</groupId>
  <artifactId>typed.clj.spec</artifactId>
  <version>1.1.1</version>
</dependency>
```

## Documentation

[API Reference](https://api.typedclojure.org/latest/typed.clj.spec/index.html)

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
