{% do-not-edit-xml-comment %}
# typed.clj/reader

<a href='{◊typedclojure-homepage◊}'><img src='../../doc/images/part-of-typed-clojure-project.png'></a>

<p>
  <a href='https://www.patreon.com/ambrosebs'><img src='../../doc/images/become_a_patron_button.png'></a>
  <a href='https://opencollective.com/typedclojure'><img src='../../doc/images/donate-to-our-collective.png'></a>
</p>

Reader for JVM Clojure.

## Releases and Dependency Information

Latest stable release is 1.0.12.

* [All Released Versions](https://clojars.org/typed.clj/reader)

[deps.edn](https://clojure.org/reference/deps_and_cli) JAR dependency information:

```clj
  typed.clj/reader {:mvn/version "1.0.12"}
```

[deps.edn](https://clojure.org/reference/deps_and_cli) Git dependency information:

- Note: use `clj -Sresolve` to resolve the `:tag` to a `:sha`

```clj
  typed.clj/reader {:git/url "{◊typedclojure-git-https-url◊}"
                    :deps/root "typed/clj.reader"
                    :tag "1.0.12"}
```

[Leiningen](https://github.com/technomancy/leiningen) dependency information:

```clojure
[typed.clj/reader "1.0.12"]
```

[Maven](https://maven.apache.org/) dependency information:

```XML
<dependency>
  <groupId>typed.clj</groupId>
  <artifactId>reader</artifactId>
  <version>1.0.12</version>
</dependency>
```

## Documentation

[API Reference](https://api.typedclojure.org/latest/typed.clj.reader/index.html)

## Differences from tools.reader

TODO

### Related work

@borkdude on #core-typed
```
I've also been doing something like this in edamame (also based on tools.reader), which is powering sci. Although it doesn't output an AST, it can include the original source string, so you can match on this in grasp:
https://github.com/borkdude/grasp#matching-on-source-string
It's basically read+string but also for all the inner nodes (edited) 
5:32
I've been considering outputting an AST, which could be useful for sci, but currently it doesn't need one really. And whitespace hasn't really been a concern.
5:34
Someone commented at the sci repo that with a tools.analyzer-like AST there could be some optimizations but I currently don't see how an AST instead of sexpr adds more information to do optimizations. I'll be keeping an eye on this to learn more. Thanks!
5:34
Currently you can output as AST-like thing with edamame using the :postprocess hook which allows you to wrap the node (and source string and location) into a container to preserve more info (like saving locations for keywords) (edited) 
5:37
Another interesting rewrite-clj alternative is https://github.com/carocad/parcera which is based on ANTLR (edited) 
```

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).

### tools.reader

Copyright © Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
