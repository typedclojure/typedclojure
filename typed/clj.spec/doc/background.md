# Background

This document categorizes several groups of higher-order functions
common in Clojure programs, and demonstrates the trade-offs of 
specifying and testing them using Clojure spec.

## First-order dependencies

```clojure
clojure.core/
  identity, keyword, find-keyword, force, list, list*, vector, vec, split-at, boolean, key, val,
  concat, rand-nth, subvec, reverse, take, take-last, drop, drop-last, shuffle, take-nth,
  sequence, into, next, nnext, nthnext, butlast, last, rest, nfirst, ffirst, first, second,
  reduced, seq, class, repeat, interleave, interpose, cycle
clojure.set/
  rename-keys, rename, project, map-invert, index, join, difference
```

```clojure
(s/def ::identity-fspec-fn
  (s/fspec
    :args (s/cat :x any?)
    :fn (fn [{{:keys [x]} :args :keys [ret]}]
          (identical? ret x))
    :ret any?))
```

## Predicates

```clojure
clojure.core/
  delay?, false?, keyword?, map?, nil?, realized?, seq?, set?, some?, symbol?, true?, vector?, zero?
```

## Value processing functions

A large category of higher-order functions in Clojure fit the
following description:

1. Takes a function as an argument
2. Takes some other values as arguments
3. Feeds the values from step 2 into the function argument
4. Returns a result that entirely depends on the results of step 2 and 3.

```clojure
clojure.core/
  apply, drop-while, every?, filter, filterv, group-by, keep, keep-indexed, map, map-indexed,
  mapcat, mapv, merge-with, pcalls, pmap, remove, repeatedly, some, sorted-set-by, split-with,
  take-while, vary-meta
clojure.set/
  select
```

For our purposes, the characterizing features of these higher-order functions are:
1. the function argument is never passed its own output (unlike the first argument of `reduce`)
2. all values passed to the function argument are available to the higher-order function
   when called (unlike the arguments to `comp`)

## Folds

```
clojure.core
  reduce, reduce-kv, trampoline, iterate
```

## Function combinators

```clojure
clojure.core/
  fnil, comp, some-fn, every-pred, complement, partial, memoize, comparator
```

The situation is slightly worse for higher-order functions
that take functions as arguments

Clojure programmers 
and write
spec has demonstrated how to integrate generative testing
with a host of other features, but does not have a strong story
for polymorphic functions. This is a notable shortcoming because
programmers frequently 
of definition and precise usage of higher-order functions in Clojure.

The 

Most higher-order functions in Clojure are polymorphic with
non-trivial dependencies between arguments and/or return values.

but they are difficult to generatively test with clojure.spec.

Spec relies on `s/fspec`'s `:fn` parameter to encode dependencies
between arguments and/or return values. This has some limitations
and can be somewhat awkward to use.

## Transducers

Functions that take or return transducers.

```clojure
clojure.core/
  map, filter, into, transduce
```

# Future work

Higher-order functions that change mutable arguments:

```clojure
clojure.core/
  agent, alter, alter-meta!, alter-var-root!, commute, reset-meta!, send, send-off, send-via, swap!
```

Associative/Coll things:

```
clojure.core/
  assoc, assoc-in, conj, find, get, get-in, merge, peek, pop, update, update-in
```
