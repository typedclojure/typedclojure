<!-- DO NOT EDIT! Instead, edit `dev/resources/root-templates/typed/cljs.analyzer/README.md` and run `./script/regen-selmer.sh` -->
# typed.cljs.analyzer

<a href='https://typedclojure.org'><img src='images/part-of-typed-clojure-project.png'></a>

Analyzer for JS Clojure, tuned for consumption by an optional type checker.

## Releases and Dependency Information

Latest stable release is 1.1.3.

* [All Released Versions](https://clojars.org/org.typedclojure/typed.cljs.analyzer)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  org.typedclojure/typed.cljs.analyzer {:mvn/version "1.1.3"}
```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

- Note: use `clj -Sresolve` to resolve the `:tag` to a `:sha`

```clj
  org.typedclojure/typed.cljs.analyzer
  {:git/url "https://github.com/typedclojure/typedclojure"
   :deps/root "typed/cljs.analyzer"
   :tag "1.1.3"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[org.typedclojure/typed.cljs.analyzer "1.1.3"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>org.typedclojure</groupId>
  <artifactId>typed.cljs.analyzer</artifactId>
  <version>1.1.3</version>
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
