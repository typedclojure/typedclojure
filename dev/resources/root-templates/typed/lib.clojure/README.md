{% do-not-edit-xml-comment %}
# typed.lib.clojure

<a href='{◊typedclojure-homepage◊}'><img src='images/part-of-typed-clojure-project.png'></a>

Type annotations and macros for the base Clojure distribution.

## Releases and Dependency Information

Latest stable release is {◊typedclojure-stable-mvn-version◊}.

* [All Released Versions](https://clojars.org/{◊typedclojure-group-id◊}/typed.lib.clojure)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  {◊typedclojure-group-id◊}/typed.lib.clojure {:mvn/version "{◊typedclojure-stable-mvn-version◊}"}
 ```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

- Note: use `clj -Sresolve` to resolve the `:git/tag` to a `:git/sha`

```clj
  {◊typedclojure-group-id◊}/typed.lib.clojure
  {:git/url "{◊typedclojure-git-https-url◊}"
   :deps/root "typed/lib.clojure"
   :git/tag "{◊typedclojure-stable-mvn-version◊}"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[{◊typedclojure-group-id◊}/typed.lib.clojure "{◊typedclojure-stable-mvn-version◊}"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>{◊typedclojure-group-id◊}</groupId>
  <artifactId>typed.lib.clojure</artifactId>
  <version>{◊typedclojure-stable-mvn-version◊}</version>
</dependency>
```

## Documentation

[API Reference](https://api.typedclojure.org/latest/typed.lib.clojure/index.html)

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
