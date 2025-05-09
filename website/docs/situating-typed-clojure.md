# Situating Typed Clojure

Typed Clojure is a verification tool for Clojure programs. Its goal is to
help develop correct Clojure programs.
There are many such tools for Clojure, so where does Typed Clojure fit?

## Clojure dynamic verifiers

The most popular verification tools available for Clojure use _dynamic_ verification.
They have the luxury of observing code running in real time and verifying
its behavior using
techniques like unit testing, property-based testing, runtime
instrumentation, assertions, and stubbing.
These include tools like clojure.test, test.check, spec, Malli,
and Plumatic Schema.

Spec popularized the technique of
automatically creating dynamic verification tools from a provided spec that could
help test whether Clojure programs implemented that spec.
For example, specifying a function enables instrumentation
and generative testing of that function against the spec.
This style of verification is very compelling as specs can be precise and expressive
and can yield powerful verification tools which catch bugs early.

Typed Clojure is very similar to (this narrow view of) spec, except it uses
_static_ verification.
Instead of running a program under various conditions to verify a _spec_
is implemented correctly,
Typed Clojure simulates running code
under a model of Clojure's semantics to verify whether a _type_ is satisfied.

Typed Clojure can interoperate with test.check, spec, and Malli
either to inherit their features or understand their constructs to
improve type checking. For example, types can be translated into generators,
and assertions made with Malli are recognized by Typed Clojure
as a type cast.

<!--
Typed Clojure's types are similar to spec or Malli schemas.
Heterogenous map types are the 
-->


## Clojure static verifiers

Two popular static verifiers for Clojure are Eastwood and clj-kondo.

Like Eastwood, Typed Clojure is provided as a library and shares an environment with
the code it is checking.
Unlike Eastwood, Typed Clojure does not attempt to preserve Clojure's
evaluation model during checking, so not all code can be checked.
The reason is so checking can be _parallelized_ similar to clj-kondo.

All three tools have no effect on compiled Clojure output.
This is normal for a static analyzer or linter, but more unusual
for a static type system. Typed Clojure is advertised
as an _optional_ static type system to emphasize this point.
Typed Clojure used to encourage using wrapper macros in user
code to aid the type checker, but this has since become
obsolete and it is now feasible to use Typed Clojure
[without a runtime dependency on Typed Clojure itself](https://github.com/typedclojure/typedclojure/tree/main/example-projects/zero-deps).

The strongest overlap in terms of static typing features
between these tools is clj-kondo's ability to support custom macro
rules. While this is needed less often in Typed Clojure since macros
can be freely expanded (even simple macros need a manually provided
"hook" in clj-kondo since
it cannot yet load the original macro definition), Typed Clojure
also supports this extension point for when the expanded macro is
too difficult to check automatically.

## Other static type systems

Typed Clojure is most comparable with static type systems for other
languages. It inherits many features directly from Typed Racket,
including occurrence typing,
ordered function intersections, and variable-arity polymorphism.
Clojure-specific extensions were added to support multimethods.

Typed Clojure's heterogeneous map types resemble TypeScript's
object types and interfaces. Like TypeScript's relationship with JavaScript,
Typed Clojure's design is strongly influenced by existing Clojure idioms
instead of reinventing a new way of writing Clojure.
Unlike TypeScript, Typed Clojure is very conservative by default,
treating unknown code as a type error instead of introducing
an unsound type.

Unlike Typed Racket, Typed Clojure is _not_ gradually typed.
Assertions are not generated to protect typed invariants from untyped code,
instead types can be translated to spec or Malli for instrumentation.
This is a manual process and results in redundant protection of typed
code from other typed code.

Typed Clojure's core checking algorithm is based on bidirectional checking,
and has nothing like Hindley-Milner global inference.
In terms of global annotations, it is more like Java than Haskell.

Typed Clojure uses symbolic execution to infer the parameter types
of local functions. It integrates with polymorphic types
so that local functions passed to higher-order functions have their
parameter types inferred.
This is Typed Clojure's most exotic feature compared to other type systems
and makes the type system trivially Turing-complete by type checking
a fixed-point combinator.
In spirit it vaguely resembles ML's let-polymorphism, where functions are
inlined in order to gather more concrete type information from their usages.


<!--
Dynamic verification works well for many Clojure applications,
especially checking "the edges" of programs. If you are happy
with the correctness and testing duration of your programs
you probably don't need Typed Clojure.

Some common problems with dynamic verification are

- the overhead of runtime checking and generative testing
- difficult to interpret and non-local error messages
- suboptimal support for higher-order functions, mutation,
  and channels.
  -->
