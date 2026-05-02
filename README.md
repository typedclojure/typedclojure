<!-- DO NOT EDIT! Instead, edit `dev/resources/root-templates/README.md` and run `./script/regen-selmer.sh` -->
<div>
  <a href='https://typedclojure.org'>
    <img src="doc/images/typed-clojure-an-optional-type-system-letterbox.png"
         alt="Typed Clojure">
  </a>
</div>

<div>
  <p>
    <a href="https://www.patreon.com/ambrosebs">
      <img src="doc/images/become_a_patron_button.png"
           alt="Become a patron">
    </a>
    <a href="https://opencollective.com/typedclojure">
      <img src="doc/images/donate-to-our-collective.png"
           alt="Donate to your OpenCollective">
    </a>
  </p>
</div>

Optional typing in Clojure, as a library.

## Usage Overview

tldr; see the [minimal example project](example-projects/minimal) for Typed Clojure setup.

Typed Clojure is separated into modules. You'll want the full type checker at development
time, and the runtime dependencies in production.

In Clojure CLI's `deps.edn`, this will look something like this:

```clojure
{:deps {org.typedclojure/typed.clj.runtime {:mvn/version "1.3.0"}}
 :aliases {:dev {:extra-deps {org.typedclojure/typed.clj.checker {:mvn/version "1.3.0"}}}}}
```

You can then start a dev repl with `clj -A:dev`.

In Leiningen's `project.clj`, it will look something like this:

```clojure
(defproject com.my-domain/a-project "1.0.0-SNAPSHOT"
  :dependencies [[org.typedclojure/typed.clj.runtime "1.3.0"]]
  :profiles {:dev {:dependencies [[org.typedclojure/typed.clj.checker "1.3.0"]]}})
```

Then, `lein repl` will automatically activate the `:dev` profile. Verify the type
checker is not included in uberjars with `lein with-profile uberjar deps :tree`.

## Example projects

- [minimal](example-projects/minimal) demonstrates how to statically type check Clojure namespaces
- [minimal-clj](example-projects/minimal-clj) demonstrates how to statically type check Clojure namespaces
- [malli-type-providers](example-projects/malli-type-providers) demonstrates how to statically type check using malli schemas
- [zero-deps](example-projects/zero-deps) demonstrates how to type check a library without introducing any runtime dependency on Typed Clojure (only dev-time).
- [symbolic-guide](example-projects/symbolic-guide) shows how to reason about symbolic execution in Typed Clojure.

## Releases and Dependency Information

Latest stable release is 1.3.0.

See modules for specific version coordinates:

### Clojure implementation

* [typed.clj.checker](typed/clj.checker/README.md): The JVM type checker
* [typed.clj.runtime](typed/clj.runtime/README.md): JVM Runtime dependencies
* [typed.clj.analyzer](typed/clj.analyzer/README.md): Analyzer for JVM Clojure
* [typed.malli](typed/malli/README.md): Malli integration.

### Implementation-agnostic

* [typed.cljc.analyzer](typed/cljc.analyzer/README.md): Implementation-agnostic base for Clojure analyzers

### Library Annotations

* [typed.lib.clojure](typed/lib.clojure/README.md): Base type annotations
* [typed.lib.core.async](typed/lib.core.async/README.md): Annotations for core.async
* [typed.lib.spec.alpha](typed/lib.spec.alpha/README.md): Annotations for spec.alpha

## Compatibility

Typed Clojure supports Clojure 1.12.3 and JDK 21+.

## Mailing List and Chat

Use the core.typed [mailing list](https://groups.google.com/forum/?fromgroups#!forum/clojure-core-typed) for Typed Clojure discussion.

[#core-typed on Clojurians Slack](https://clojurians.slack.com/app_redirect?channel=core-typed)

[Archive for #core-typed Slack](https://clojurians.zulipchat.com/#narrow/stream/180378-slack-archive/topic/core-typed)

## Documentation

See [wiki](https://github.com/clojure/core.typed/wiki).

[API Reference](https://api.typedclojure.org/latest/typed.clj.runtime/index.html)

[Ambrose's blog](https://blog.ambrosebs.com)

## Developer Information

- [Typed Clojure Contributor Code of Conduct](CODE_OF_CONDUCT.md)
- [Contributing guidelines](CONTRIBUTING.md)

## Contributors

Thanks to the following people for contributing to core.typed:

* Stephen Compall (S11001001)
* Andrew Brehaut (brehaut)
* Christopher Spencer (cspencer)
* Reid McKenzie (arrdem)
* Di Xu (xudifsd)
* Nicola Mometto (Bronsa)
* Chas Emerick (cemerick)
* Jake Goulding (shepmaster)
* Andy Fingerhut
* Aravind K N (arav93)
* Allen Rohner (arohner)
* Minori Yamashita (ympbyc)
* Kyle Kingsbury (aphyr)
* Nathan Sorenson
* Tobias Kortkamp (t6)
* Alejandro Gomez (dialelo)
* Piotr Jarzemski (kazuhiro)
* Oleksandr Yakushev (alexander-yakushev)
* David Miller (dmiller)

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).

### tools.analyzer

Copyright © Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).

### tools.analyzer.jvm

Copyright © Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).

### tools.analyzer.js

Copyright © Nicola Mometto, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
