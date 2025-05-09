# Misuses of Typed Clojure

A persistent myth about Clojure is that it _needs_ a type system to
be a viable language.
A document introducing Typed Clojure might be a surprising place to bust this myth,
but Typed Clojure's primary goal of helping implement correct Clojure programs
extends to advising when to _not_ use Typed Clojure.

## Singlehandedly fixing Clojure's error messages

If you are unsatisfied with Clojure's error messages,
there are many steps you should take before
using Typed Clojure to address this problem.

Make sure you are comfortable reading a raw JVM stacktrace.
Since Clojure is utilitarian as a hosted language and
relies on runtime exceptions to signal errors as a dynamic language,
this is one of the most important skills as a Clojure developer.
Be aware of how your tool chain prints error messages,
and customize it to your needs.

Annotate functions with spec, Malli, or Plumatic Schema
and enable instrumentation.
This will help throw errors closer to the root cause of a problem.
Consider using the non-direct-linked Clojure jar during development
if you need more flexibility in instrumenting core functions.
Benchmark the costs of instrumentation
and consider enabling it in production for cheap or critical areas.

Instrument macros with spec. Errors thrown during macro
expansion are amongst the most opaque. See the built-in
specs for `let` and `fn` for inspiration.

Customize the printing of errors from spec or malli.
Plumatic Schema has excellent error messages by default,
at the expense of some expressivity and more limited data errors.

For your own code, use Clojure's `ex-info` to provide extra
dynamic context to improve traceability.
Use property-based testing to surface places where error messages
can be improved. Also test invalid inputs should at least
throw known exceptions.

## Removing dynamic checks

If you plan to replace dynamic assertions with static ones via Typed Clojure,
first be aware of the risks involved.

Typed Clojure operates on the (blatently false) assumption that only
typed code will call typed code. If untyped code breaks any invariant
assumed by Typed Clojure, then all bets are off.
This is not much different than spec's recommendation to turn off instrumentation
in production, however note spec's premise that you are generatively
testing your code with instrumentation enabled.

Even if your entire code base is typed, type checkers are
notoriously large, complex, and bug-prone. The simplicity of dynamic verification
is a strong point in its favor.

If you are having performance problems with generative tests, consider
running fewer iterations when this is most noticable.
If particular inline assertions are bottlenecks, consider distributing equivalent
checks amongst helper functions to minimize retraversing collections.
If you are having problems with flaky generative tests,
pin them with a known good seed during critical builds like deployments.
A flaky generative test could be a symptom of a real bug,
so note down failing seeds so you can address the root problem effectively.

## New to Clojure from a statically typed language

If you are new to Clojure from another statically typed
programming language, you might have the misconception that Clojure needs
a type system to be able to write correct programs.
The two most common reasons in my experience are that you are either
a veteran of statically typed languages, or your previous language was
a dynamic language that was difficult to use correctly without an optional
type system.

I encourage you to first explore Clojure's
existing dynamic verification tooling before committing to Typed Clojure.
Spec is a great place to start.
If you still miss static type checking,
Typed Clojure integrates with spec and Malli to add static checking
where you need it.

## Workplace rejects Clojure due to lack of type system

If you are in a situation that strictly demands Clojure have a static type system
before it is approved to be used, I do not suggest presenting Typed Clojure
as that solution. Instead, pair Clojure with clj-kondo.

Its static type system is permissive but also surprisingly effective
when paired with idiomatic Clojure code.
clj-kondo sparks joy by being very fast and only reporting problems
that have a low probability of being a false-positive.
This strategy is similar in spirit to type systems like Erlang's Dialyzer,
or even TypeScript's default treatment of ascribing code it does not
recognize as the dynamic type `any`.

clj-kondo has many applications beyond type checking that can address
other common concerns about dynamically typed languages, including
enforcing consistent style, dead-code detection, custom rules for macros,
and banning insecure parts of Clojure.

There is no shortage of testimonials that clj-kondo indeed
sparks joy, finds real issues in production Clojure code, and is improved
constantly with the prolific borkdude at the helm.
