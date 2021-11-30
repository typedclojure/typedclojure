# Typed Clojure Update 2 of 2

The goal of [this project funded by Clojurists Together](https://www.clojuriststogether.org/news/q3-2021-funding-announcement/) is to
improve static type error messages in [Typed Clojure](https://github.com/typedclojure/typedclojure),
specifically to replace expanded code in error messages with surface-level syntax.

In the first half of the project (last update), I concentrated on three main areas:
1. Increase direct support for problematic clojure.core macros
2. Improve error messages for inlining functions
3. Identify classes of implementation quirks in core Clojure macros to prepare for potential typing rules

In the second half (this update), I:
- Improved the experience of mixing clojure.spec and Typed Clojure
  - Typed Clojure no longer complains about the expansion of `s/def` `s/fdef`
  - Approach: implemented custom typing rules for both macros that ignores the body
  - [Commit](https://github.com/typedclojure/typedclojure/commit/2c423fb3daacaeb120b54a390c315588bba531a5)
- Trimmed error messages
  - Some hints in error messages have become irrelevant noise.
  - [Commit](https://github.com/typedclojure/typedclojure/commit/613691ff176d05dd886a5d387979868a5f5ab2bb)
- Created a proof-of-concept cljs.analyzer variant that can partially macroexpand
  - inspired by the inlining work from the first half of this project
  - Problem: CLJS uses lots of macros. Typed Clojure errors are even worse in CLJS.
    - eg., (+ 1 2) => (js* "(~{})" 1)
    - we want to pass (+ 1 2) to the type checker, not (js* "(~{})" 1)
      - but + is a macro call... not representable by cljs.analyzer
  - Solution: make partially expanded forms representable to the type checker
    - Type checker uses tools.analyzer-style AST's to analyze code
      - so does cljs.analyzer
    - I created typed.clj.analyzer to add an :unanalyzed form for partially expanded forms
      - but I didn't know how to port to cljs
  - Promising early results
    - unit tests for pausing the expansion of the children of each AST :op
  - Next: integrate into cljs checker (which was on pause for several years and needs a lot of other updates)
  - [Commit](https://github.com/typedclojure/typedclojure/commit/c8c2f870d7f58ff8a8abfaa83f939b5a824bd5cb)
