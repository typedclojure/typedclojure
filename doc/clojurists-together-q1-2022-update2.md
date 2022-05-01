# Typed Clojure Update 2 of 2

The goal of [this project funded by Clojurists Together](https://www.clojuriststogether.org/news/q1-2022-funding-announcement/) is to
(resurrect) support for type checking ClojureScript files in [Typed Clojure](https://github.com/typedclojure/typedclojure).

Since the [last update](https://github.com/typedclojure/typedclojure/blob/main/doc/clojurists-together-q1-2022-update1.md), support for type-checking ClojureScript has improved enough to consider the CLJS type checker "resurrected" (but still young).

To stress test the ability for Typed Clojure to check `.cljc` files, I have been [type checking malli's implementation](https://github.com/frenchy64/malli/pull/1).

Checking in CLJS mode has revealed many bugs and shortcomings in the checker. Clojure mode has been outrageously successful--[it found a bug in malli](https://github.com/metosin/malli/pull/690)!

Some other feature work has been landing in Typed Clojure that has support for both Clojure and ClojureScript (enabled by this Clojurists Together project).
- [using malli schemas as types](https://www.patreon.com/posts/static-type-64236939)
- [using specs as types](https://www.patreon.com/posts/static-type-64321657) (CLJS WIP)
- [support defprotocol and implements? in CLJS](https://www.patreon.com/posts/typed-clojure-1-64869793)
- [checking programs without depending on Typed Clojure](https://www.patreon.com/posts/typed-clojure-1-65065388)
- [new syntactic sugar for function types](https://github.com/typedclojure/typedclojure/blob/main/CHANGELOG.md#1028)

See the [CHANGELOG.md](https://github.com/typedclojure/typedclojure/blob/main/CHANGELOG.md) for full details on everything else
accomplished in the time period since the last update (versions 1.0.21-1.0.28).
