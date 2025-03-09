# Caching Type checking results

Checking a form's type is an expensive operation.
Type checking the same expression in the same type environment
gives the same answer. We should use this information to avoid redundant
checks and improve performance.

## Identical namespace caching

The most common case is that a namepsace has not changed textually. This is the main
case targeted by tools.namespace, and while we do could do more granular caching, this
should always be checked first to avoid extra work.

If an entire namespace is exactly the same as the last time it was type checked,
we need to recheck it if:

1. a different Typed Clojure version is used
2. a transitive .clj dependency changes
3. a typing rule used to check the namespace changes
4. any type aliases/annotations used in the process of type checking the namespace have changed
- typed.cljc.checker.name-env/get-type-name
- typed.cljc.checker.var-env/lookup-Var-nofail

```clojure
(ns example
  (:require [typed.clojure :as t]
            [annotation-ns :as-alias ann]
            ;; 2) if dep.clj changes or any of its dependencies
            dep))

;; 4) if ann/Str changes its meaning
(t/ann foo [ann/Str :-> t/Bool])
(defn foo [x]
  ;; 4) if dep/string->boolean changes its annotation
  (dep/string->boolean x))
```

If we need to recheck it, ideally only part of the namespace needs to be rechecked (next section).

## Changed namespace caching

If a namespace has textual changes, we need to recheck the parts of the namespace
that changed.

The order of forms in a namespace often does not change the way each form type checks.
For example, if the entire namespace is a series of `defn`'s, we could theoretically type check
it in any order, or skip some.

When it's true, this insight simplifies identifying a form, which gives some flexibility
when looking up its type in a cache.

For example, a form could be identified as a cache key using a combination of:
- its ns form
- the form itself
- all used vars
- all type aliases

If it is a `defn`, perhaps the name of the `def` could also be used for faster lookups.

Here are some things that should invalidate the type of a form.

- a different Typed Clojure version is used
- a macro used by the form changes
- a typing rule used by the form changes
- a type annotation (transitively) used when checking a form changes
- the form itself changes

## Changes that shouldn't retrigger checking

- just a comment or docstring changes
- namespace alias changes name
- changes to a namespace dependency that isn't used in a particular form

## Side-effects of type checking

Sometimes we need to check a form for its side effects.

- checking a `defmulti` changes the type of a multimethod
