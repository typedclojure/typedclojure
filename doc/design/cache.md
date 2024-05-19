# Caching Type checking results

Checking a form's type is an expensive operation.
Type checking the same expression in the same type environment
gives the same answer. We should use this information to avoid redundant
checks and improve performance.

## Identical namespace caching

If an entire namespace is exactly the same as the last time it was type checked,
we need to recheck it if:

- a different Typed Clojure version is used
- a transitive .clj dependency changes
- a typing rule used to check the namespace changes

If we need to recheck it, ideally only part of the namespace needs to be rechecked (next section).

## Changed namespace caching

If a namespace has textual changes, we need to recheck the parts of the namespace
that changed.

The order of forms in a namespace often does not change the way each form type checks.
For example, if the the entire namespace is a series of `defn`'s, we could theoretically type check
it in any order, or skip some.

When it's true, this insight simplifies identifying a form, which gives some flexibility
when looking up its type in a cache.

For example, a form could be identified as a cache key using a combination of:
- its ns form
- the form itself

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
