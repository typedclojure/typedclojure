{% do-not-edit-xml-comment %}
# typed.malli

<a href='{◊typedclojure-homepage◊}'><img src='../../doc/images/part-of-typed-clojure-project.png'></a>

<p>
  <a href='https://www.patreon.com/ambrosebs'><img src='../../doc/images/become_a_patron_button.png'></a>
  <a href='https://opencollective.com/typedclojure'><img src='../../doc/images/donate-to-our-collective.png'></a>
</p>

Malli integration.

## Releases and Dependency Information

Latest stable release is {◊typedclojure-stable-mvn-version◊}.

* [All Released Versions](https://clojars.org/{◊typedclojure-group-id◊}/typed.malli)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  {◊typedclojure-group-id◊}/typed.malli {:mvn/version "{◊typedclojure-stable-mvn-version◊}"}
```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

```clj
  {◊typedclojure-group-id◊}/typed.malli
  {:git/url "{◊typedclojure-git-https-url◊}"
   :deps/root "typed/malli"
   :git/tag "{◊typedclojure-stable-mvn-version◊}"
   :git/sha "{◊typedclojure-stable-sha◊}"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[{◊typedclojure-group-id◊}/typed.malli "{◊typedclojure-stable-mvn-version◊}"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>{◊typedclojure-group-id◊}</groupId>
  <artifactId>typed.malli</artifactId>
  <version>{◊typedclojure-stable-mvn-version◊}</version>
</dependency>
```

## Documentation

[API Reference](https://api.typedclojure.org/latest/typed.malli/index.html)

## Usage

Note: the following assumes you have the Clojure core type annotations and full type checker on your classpath.
Here is a quick way to create a REPL to follow this tutorial with:

```clojure
clj -Sdeps '{:deps {{◊typedclojure-group-id◊}/typed.malli {:mvn/version "{◊typedclojure-stable-mvn-version◊}"}
                    ;; only needed if omitting typed.clj.checker
                    {◊typedclojure-group-id◊}/typed.lib.clojure {:mvn/version "{◊typedclojure-stable-mvn-version◊}"}
                    {◊typedclojure-group-id◊}/typed.clj.checker {:mvn/version "{◊typedclojure-stable-mvn-version◊}"}}}'
```

Now you can execute each form by copying it into this REPL.

```clojure
(require '[malli.core :as m]
         '[typed.malli :as tm]
         '[typed.clojure :as t])
```

### Validation 

[typed.malli/validate](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-validate) is a wrapper for `malli.core/validate`.

See also:
- [typed.malli/validator](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-validator)
- [typed.malli/defvalidator](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-defvalidator)
- [typed.malli/explain](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-explain)

```clojure
;; normally, `validate` takes schemas
(m/validate int? "1")
;=> false

(m/validate int? 1)
;=> true

;; the typed variant (note `tm` namespace) takes Typed Clojure types
(tm/validate t/AnyInteger "1")
;=> false

(tm/validate t/AnyInteger 1)
;=> true

;; it can be used to coerce values in a way the type checker understands
(t/cf (fn [x]
        {:pre [(tm/validate t/AnyInteger x)]}
        (inc x))
      [t/Any :-> Number])
;=> [[t/Any -> Number] {:then tt, :else ff}]

;; tm/explain works oppositely
(t/cf (fn [x]
        {:pre [(not (tm/explain t/AnyInteger x))]}
        (inc x))
      [t/Any :-> Number])
;=> [[t/Any -> Number] {:then tt, :else ff}]
```

### Parsing values

[typed.malli/parse](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-parse) is a wrapper for `malli.core/parse`.

See also:
- [typed.malli/parser](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-parser)
- [typed.malli/defparser](https://api.typedclojure.org/latest/typed.malli/typed.malli.html#var-defparser)

```clojure
;; normally, `parse` takes schemas
(m/parse [:orn
          [:int int?]
          [:bool boolean?]]
         1)
;=> [:int 1]

;; `tm/parse` takes Typed Clojure types and infers the return value.
;; use `::tm/name` to specify `:orn` tags (bare union currently unsupported).
(t/defalias ParsableIntOrBool
  (t/U ^{::tm/name :int} t/AnyInteger
       ^{::tm/name :bool} t/Bool))

(tm/parse ParsableIntOrBool true)
;=> [:bool true] : (t/U
;                    (t/HVec [(t/Val :int) t/AnyInteger])
;                    (t/HVec [(t/Val :bool) t/Bool])
;                    (t/Val :malli.core/invalid))

;; example using `tm/parse` to type check a function
(t/cf
  (t/fn [a :- ParsableIntOrBool] :- t/AnyInteger
    (let [prs (tm/parse ParsableIntOrBool a)]
      (assert (not= ::m/invalid prs))
      (case (first prs)
        :int (second prs)))))
;=> [[ParsableIntOrBool -> t/AnyInteger] {:then tt, :else ff}]
```

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
