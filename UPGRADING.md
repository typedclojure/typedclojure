# From typedclojure 1.0.14 to typedclojure 1.0.15

Change the following coordinates:

```
typed.clj/checker => org.typedclojure/typed.clj.checker
typed.clj/runtime => org.typedclojure/typed.clj.runtime
typed.clj/analyzer => org.typedclojure/typed.clj.analyzer
typed.clj/annotator => org.typedclojure/typed.clj.annotator
typed.clj/refactor => org.typedclojure/typed.clj.refactor
typed.clj/reader => org.typedclojure/typed.clj.reader
```

# From typedclojure 1.0.7 to typedclojure 1.0.10

Change the following coordinates:

```
typed/analyzer.js => typed.cljs/analyzer
typed/annotator.jvm => typed.clj/annotator
typed/lang.jvm => typed.clj/lang
```

# From typedclojure 1.0.3 to typedclojure 1.0.7

Change the following coordinates:

```
typed/runtime.jvm => typed.clj/runtime
typed/checker.jvm => typed.clj/checker
typed/analyzer.jvm => typed.clj/analyzer
```

# From typedclojure 1.0.2 to typedclojure 1.0.3

Change the following coordinates:

```
typed/lib.clojure => typed.lib/clojure
typed/lib.core.async => typed.lib/core.async
```

# From core.typed 1.0.0 to typedclojure 1.0.1

Change group ID from `org.clojure.typed` to `typed`, and
use https://clojars.org/ as your repository.

New homes for each package:

```
https://clojars.org/typed/analyzer.common
https://clojars.org/typed/analyzer.js
https://clojars.org/typed/analyzer.jvm
https://clojars.org/typed/annotator.jvm
https://clojars.org/typed/checker.js
https://clojars.org/typed/checker.jvm
https://clojars.org/typed/lang.jvm
https://clojars.org/typed/lib.clojure
https://clojars.org/typed/lib.core.async
https://clojars.org/typed/pom
https://clojars.org/typed/runtime.jvm
```
