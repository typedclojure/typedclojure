# 1.0.18

- improve destructuring error messages for:
  - clojure.core/fn
  - clojure.core.typed/fn
- support :when/:while/:let in clojure.core/for typing rule
- create type rule for clojure.core/doseq with destructuring and support for all options
- create type rule for clojure.core/defn with improved destructuring error messages
- create type rule for clojure.core/defmethod with improved destructuring error messages
- check inlines before expansion to improve error messages
  - eg., now `clojure.core/inc` is appears in error messages instead of `clojure.lang.RT/inc`
- add `typed.clj.malli` project

# 1.0.17

- move ext.c.c.t typing rules from runtime jar to checker
- add spec{1,2} shim to prepare for spec1 support in typed.clj.spec
- add `clojure.core/for` typing rule with destructuring
- remove dynamic vars from uniquify analyzer pass
- add `clojure.core/let` typing rule
- add custom error message for vector destructuring
