# Typed Clojure Update 1 of 2

The goal of [this project funded by Clojurists Together](https://www.clojuriststogether.org/news/q3-2021-funding-announcement/) is to
improve static type error messages in [Typed Clojure](https://github.com/typedclojure/typedclojure),
specifically to replace expanded code in error message with surface-level syntax.

In the first half of the project, I have concentrated on three main areas:
1. Increase direct support for problematic clojure.core macros
2. Improve error messages for inlining functions
3. Identify classes of implementation defects in core Clojure macros to prepare for potential typing rules

## Increase direct support for problematic clojure.core macros

**Problem**: Typed Clojure expands macros that it does not have special rules for. This
works well when the expansion is simple (eg., `binding`, `future`, or `delay`), but this strategy
backfires horribly for complex macros like `doseq`.

For example, `doseq` does not have direct support in Typed Clojure `1.0.17` and any usage of it results
in an incomprehensible error message (note: `t/cf` type [c]hecks a [f]orm):

```clojure
$ clj -Sdeps '{:deps {org.typedclojure/typed.clj.checker {:mvn/version "1.0.17"}}}}'
Clojure 1.10.3
user=> (require '[clojure.core.typed :as t])
nil
user=> (t/cf (doseq [a [nil :a 3]] (inc a)))
Type Error (NO_SOURCE_PATH:1:7)
Loop requires more annotations


in:
(loop*
 [seq_30744
  (clojure.core/seq [nil :a 3])
  chunk_30745
  nil
  count_30746
  0
  i_30747
  0]
 (if
  (clojure.core/< i_30747 count_30746)
  (clojure.core/let
   [a (.nth chunk_30745 i_30747)]
   (do (inc a))
   (recur
    seq_30744
    chunk_30745
    count_30746
    (clojure.core/unchecked-inc i_30747)))
  (clojure.core/when-let
   [seq_30744 (clojure.core/seq seq_30744)]
   (if
    (clojure.core/chunked-seq? seq_30744)
    (clojure.core/let
     [c__6014__auto__ (clojure.core/chunk-first seq_30744)]
     (recur
      (clojure.core/chunk-rest seq_30744)
      c__6014__auto__
      (clojure.core/int (clojure.core/count c__6014__auto__))
      (clojure.core/int 0)))
    (clojure.core/let
     [a (clojure.core/first seq_30744)]
     (do (inc a))
     (recur (clojure.core/next seq_30744) nil 0 0))))))



Execution error (ExceptionInfo) at clojure.core.typed.errors/print-errors! (errors.cljc:274).
Type Checker: Found 1 error
```

We need explicit support for `doseq` and similar macros to both improve inference and error messages.

**Prior work**: `clojure.core.typed/doseq` is a wrapper macro that requires annotations for all bindings.
Besides the onerous task of local annotations, this was inadequate because `doseq`'s error message (above)
makes no mention of this alternative.

**Approach**: Create typing rule for `doseq` and several other `clojure.core` macros, and develop `:let/:while/:when` support
for list comprehension rules.

**Results**:

`doseq` is now supported and error messages are pleasant. Note the error msg for `inc` is also new--see next section.

```clojure
$ clj -Sdeps '{:deps {org.typedclojure/typed.clj.checker {:mvn/version "1.0.19"}}}}'
Clojure 1.10.3
user=> (require '[clojure.core.typed :as t])
nil
user=> (t/cf (doseq [a [nil :a 3]] (inc a)))
Type Error (NO_SOURCE_PATH:1:29)
Function inc could not be applied to arguments:


Domains:
        Number

Arguments:
        (t/U (t/Val 3) (t/Val :a) nil)

Ranges:
        Number




in:
(inc a)



Execution error (ExceptionInfo) at clojure.core.typed.errors/print-errors! (errors.cljc:276).
Type Checker: Found 1 error
```

**Commits**:
- [doseq rule + tests](https://github.com/typedclojure/typedclojure/commit/c2ee870edfd76a98d4f0d763aae4814b9f9250ef)
- [support clojure.core/for :when/:while/:let](https://github.com/typedclojure/typedclojure/commit/886fdcc6c182a39bae7926362cf118206d16f8e1)
- [defmethod rule](https://github.com/typedclojure/typedclojure/commit/f550286c2e39cc711c9da2b738fd60b4b8451cfa)
- [defn rule](https://github.com/typedclojure/typedclojure/compare/c2ee870edfd76a98d4f0d763aae4814b9f9250ef...b1c07def655e132a460aab324dac44f6f1df3b97)

## Improve error messages for inlining functions

**Problem**: inline functions are an experimental Clojure feature that enables the compiler to treat
a var as a macro in operator position and a function in higher-order contexts. Typed Clojure
expands inline functions for `:tag` inference purposes, but if a type error occurs in the inlined expansion, the original form is lost and the expansion
is blamed. This results in an unhelpful error message.

For example, `inc` blames its expansion `clojure.lang.Numbers/inc`:
```clojure
$ clj -Sdeps '{:deps {org.typedclojure/typed.clj.checker {:mvn/version "1.0.17"}}}}'
Clojure 1.10.3
user=> (require '[clojure.core.typed :as t])
nil
user=> (t/cf (inc nil))
Type Error (NO_SOURCE_PATH:1:7)
Static method clojure.lang.Numbers/inc could not be applied to arguments:


Domains:
        Number

Arguments:
        nil

Ranges:
        Number




in:
(clojure.lang.Numbers/inc nil)



Execution error (ExceptionInfo) at clojure.core.typed.errors/print-errors! (errors.cljc:274).
Type Checker: Found 1 error
```


**Prior work**: There is a similar problem in ClojureScript's compiler via the `js*` form. The ClojureScript analyzer added
`:js-op` metadata to the analyzed form so linters like Typed Clojure can infer better error messages. However this only
helped marginally as the expanded code was still checked, and the inlining was not always easy to infer (eg., different order
of arguments).

A briefly considered approach in fixing this problem was to define a custom typing rule for each of the ~80 clojure.core inline functions. This was
discarded in favor of the following once-and-for-all solution.

**Approach**: Check inlining _before_ expansion, and propagate tag information after type checking. This is not possible
if using tools.analyzer (as Typed Clojure did pre-2019), but is relatively straightforward with [typed.clj.analyzer](https://github.com/typedclojure/typedclojure/blob/main/typed/clj.analyzer/README.md) (see [maybe-check-inlineable](https://github.com/typedclojure/typedclojure/commit/2b3ba3bbfcf615b5d4e92b4e7bae7a356100c772#diff-a4006cf0fe797e50023948e873f147c6f37a8af7b354509709fdf29377c8954fR289) for the required juggling).

**Results**
This change improved error messages for [around 78 functions](https://github.com/typedclojure/typedclojure/commit/2b3ba3bbfcf615b5d4e92b4e7bae7a356100c772#diff-c32ff2e4f53b6e6da9e2a1b3f79e1f3f6cf7d080d7b59f9b1b682116c47c0e9dR205) in `clojure.core`. Now inline functions never blame their expansions and unsupported inline functions consistently throw type errors in first- and higher-order contexts
(instead of expanding in inline contexts and erroring in higher-order ones).

For example, `inc` now blames its form instead of its expansion (see `in: (inc nil)`).

```clojure
$ clj -Sdeps '{:deps {org.typedclojure/typed.clj.checker {:mvn/version "1.0.19"}}}}'
Clojure 1.10.3
user=> (require '[clojure.core.typed :as t])
nil
user=> (t/cf (inc nil))
Type Error (NO_SOURCE_PATH:1:7)
Function inc could not be applied to arguments:


Domains:
        Number

Arguments:
        nil

Ranges:
        Number




in:
(inc nil)



Execution error (ExceptionInfo) at clojure.core.typed.errors/print-errors! (errors.cljc:276).
Type Checker: Found 1 error
```


**Commits**
- [check inlines before expansion to improve error msg](https://github.com/typedclojure/typedclojure/commit/2b3ba3bbfcf615b5d4e92b4e7bae7a356100c772)

## Identify classes of implementation defects in core Clojure macros to prepare for potential typing rules

**Problem**: To improve static type error messages for a macro, a custom typing rule is needed.
However, typing rules for macros need to simulate the macro expansion of the original macro accurately in order to be sound.
Some macros in clojure.core are known to [leak implementation details](https://clojure.atlassian.net/browse/CLJ-2573)--this would
influence how typing rules are written, so we need to investigate similar issues for other macros.

**Prior work**:
- https://clojure.atlassian.net/browse/CLJ-2573

**Approach**: Study the definition of macros and try and break them.

**Results**: I found 5 classes of implementation leakage in core Clojure macros.
1. In macros that wrap try/finally around a body, `catch` syntax is leaked to the user.
```clojure
$ clj
Clojure 1.10.3
user=> (binding [] (catch Exception e :foo))
nil
user=> (locking 1 (catch Exception e :foo))
nil
```
2. In macros that wrap fn around a body, a recur target is available (also plays poorly with `:once` fns).
```clojure
$ clj
Clojure 1.10.3
user=> (delay (recur))
#object[clojure.lang.Delay 0x3d7fa3ae {:status :pending, :val nil}]
user=> (let [a (Object.)] (lazy-seq (prn a) (when a (recur))))
(#object[java.lang.Object 0x1a411233 "java.lang.Object@1a411233"]
nil
)
```
3. In macros that wrap fn around a body, pre/post syntax is leaked to the user.
```clojure
$ clj
Clojure 1.10.3
user=> (with-bindings [] {:pre [false]} 1)
Execution error (AssertionError) at user/eval164$fn (REPL:1).
Assert failed: false
```
4. Double macro expansion and evaluation.
```clojure
$ clj
Clojure 1.10.3
user=> (vswap! (do (prn "created") (volatile! 0)) inc)
"created"
"created"
1
```
5. Unreliable `:tag` propagation.
```clojure
$ clj
Clojure 1.10.3
user=> (set! *warn-on-reflection* true)
true
user=> (defmacro id [a] a)
#'user/id
user=> (vswap! (id (identity (volatile! 0))) inc)
Reflection warning, NO_SOURCE_PATH:1:1 - reference to field deref can't be resolved.
Reflection warning, NO_SOURCE_PATH:1:1 - call to method reset can't be resolved (target class is unknown).
1
```

As a result of this (and some prior) work, the following macros are now known to leak implementation details in some combination of the aforementioned ways and need special
handling in potential typing rules:
- `locking` ([upstream report](https://clojure.atlassian.net/browse/CLJ-2573)), `binding`, `with-bindings`, `sync`, `with-local-vars`, `with-in-str`, `dosync`, `with-precision`, `with-loading-context`, `with-redefs`, `delay`, `vswap!`, `lazy-seq`, `lazy-cat`, `future`, `pvalues`, `clojure.test/{deftest,deftest-,testing,with-test,with-test-out}`, `clojure.java.shell/with-sh-{dir,env}`, `clojure.test.tap/with-tap-output`, `clojure.pprint/with-pprint-dispatch`, `clojure.core.async/thread`, `clojure.core.logic.pldb/with-{db,dbs}`, `clojure.tools.trace/dotrace`, `clojure.test.check.properties/for-all`, `clojure.test.check.generators/let`, `clojure.java.jmx/with-connection`, `clojure.core.match.debug/with-recur`

**Reference**:
- [non-leaky clojure.core macros](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.core.html)
- [non-leaky clojure.test macros](https://frenchy64.github.io/fully-satisfies/latest/io.github.frenchy64.fully-satisfies.non-leaky-macros.clojure.test.html)
