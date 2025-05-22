{% do-not-edit-xml-comment %}
# typed.cljs.analyzer

<a href='{◊typedclojure-homepage◊}'><img src='images/part-of-typed-clojure-project.png'></a>

Analyzer for JS Clojure, tuned for consumption by an optional type checker.

## Releases and Dependency Information

Latest stable release is {◊typedclojure-stable-mvn-version◊}.

* [All Released Versions](https://clojars.org/{◊typedclojure-group-id◊}/typed.cljs.analyzer)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  {◊typedclojure-group-id◊}/typed.cljs.analyzer {:mvn/version "{◊typedclojure-stable-mvn-version◊}"}
```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

```clj
  {◊typedclojure-group-id◊}/typed.cljs.analyzer
  {:git/url "{◊typedclojure-git-https-url◊}"
   :deps/root "typed/cljs.analyzer"
   :git/tag "{◊typedclojure-stable-mvn-version◊}"
   :git/sha "{◊typedclojure-stable-sha◊}"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[org.typedclojure/typed.cljs.analyzer "{◊typedclojure-stable-mvn-version◊}"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>{◊typedclojure-group-id◊}</groupId>
  <artifactId>typed.cljs.analyzer</artifactId>
  <version>{◊typedclojure-stable-mvn-version◊}</version>
</dependency>
```

## Documentation

[API Reference](https://api.typedclojure.org/latest/typed.cljs.analyzer/index.html)

## Differences from tools.analyzer

typed.cljs.analyzer is a heavily modified variant of tools.analyzer.
If you're familiar with the latter, here's what this library does differently.

- Adds an `:unanalyzed` AST node that just holds a `:form` and `:env`.
- Forms are analyzed lazily, with `:unanalyzed` nodes being used for immediate children.
- `:unanalyzed` nodes support a `:clojure.core.typed.analyzer/config` entry which will be associated
  onto whatever node it becomes when analyzed.
- `clojure.tools.analyzer.env` is not used.
- `resolve-{sym,ns}` are now dynamic variables that are implemented for each platform.
- `run-passes` only supports a single pass
- `uniquify-locals` is a default pass that is compatible with `:unanalyzed` nodes
- Gilardi scenario can be (carefully) managed (see `clojure.core.typed.analyzer.jvm.gilardi-test` in JVM implementation for a type system example)

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).

### tools.analyzer.js

Copyright © Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
