# Checking namespaces

Typed Clojure is designed for the _top-level form_ as the smallest unit of checking. A work-stealing threadpool is used to parallelize the checking of top-level forms.

Due to the variety of strategies used
for type checking Clojure code, forms can take a long time to type check.
Parallelizing checking helps lower the chance that any one form becomes a
large bottleneck to checking.

Forms can be checked in any order. This drastically deviates from Clojure's
normal evaluation rules, so special considerations are needed to use Typed Clojure.

## One namespace per file

Every form in a namespace must be in the same namespace.

## Namespaces are required before checking

## Type annotations are registered before checking

## Expand-time side effects are not allowed

Typed Clojure will treat all macros as pure functions, and will expand
forms if the checker does not know how to proceed otherwise.

If a macro has a (non-thread-safe and non-idempotent) side effect when
expanded via `macroexpand-1`,
the checker must be taught another way of checking this macro via
a custom type rule, or simply be instructed to ignore code that would
trigger the side effects.

## Refining macros is not allowed
