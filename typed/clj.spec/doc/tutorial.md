# Tutorial for typed.clj.spec

Welcome! typed.clj.spec is a library that provides generative-testing for
static types.

It's a lot of fun to play around with, but also packs a punch -- enjoy!

## Preliminaries

Follow the [Quickstart](../README.md#quickstart) to get a REPL running.

This tutorial also assumes these namespaces are loaded:

```clojure
(require '[clojure.alpha.spec :as s]
         '[clojure.alpha.spec.gen :as gen]
         '[typed.clj.spec :as t])
```

## Spec Preliminary: Function specs

Function types in typed.clj.spec are mostly provided by spec's `s/fspec`,
so let's review how to use it.

Say you want to generatively test a function that takes integers and
returns strings (this would be written `[Int -> Str]` in Typed Clojure
syntax). Here's how you'd do it in spec:

```clojure
;; 1. define an implementation
(defn f
  "Stringifies an integer"
  [x] (str x))

;; 2. register its spec
(s/def ::f (s/fspec :args (s/cat :x integer?)
                    :ret string?))

;; 3. generatively test implementation based on spec
(assert (s/valid? ::f f))
```

We can even provide our own implementation of `f` and peek
under the covers to see how it is tested.

```clojure
(assert (s/valid? ::f (fn [x] (prn "x" x) (str x))))
;"x" -1
;"x" 0
;"x" -1
;"x" -1
;"x" -1
;"x" 0
;"x" -1
;"x" -2
;"x" -4
;"x" -7
;"x" -1
;"x" -2
;"x" 365
;"x" 1
;"x" 5
;"x" -2345
;"x" 122
;"x" 101
;"x" 4082
;"x" 1
;"x" -128352
nil
```

You can control how many inputs are generated by `s/fspec` 
via `s/*fspec-iterations*`.

```clojure
(assert (binding [s/*fspec-iterations* 2]
          (s/valid? ::f (fn [x] (prn "x" x) (str x)))))
;"x" -1
;"x" -1
nil
```

typed.clj.spec provides other function types, but you probably won't need them
when you're starting out. We'll cover them later (see `t/fcase` if you're curious).

## Parameteric Polymorphism ("for all" types)

In a similar way that functions abstract over the computation
of a set of (return) values, parameteric polymorphism is a type-level
abstraction over a set of types.

For example, the runtime specification of `clojure.core/identity` is the
intersection of the following (infinite number of) rewrite rules of values to themselves:

```clojure
(identity 1) => 1
(identity 2) => 2
(identity true) => true
(identity false) => false
...
```

We abstract over these rewrite rules with a function value:

```
(defn identity [x] x)
```

A similar situation occurs at the type level. The type of `identity` is the intersection
of an infinite number of types of the form:

```clojure
(ann identity [Int -> Int])
(ann identity [Bool -> Bool])
(ann identity [(U Bool Int) -> (U Bool Int)])
...
```

Parameteric polymorphism (henceforce, just "polymorphism") enables abstraction
over these types.

```clojure
(ann identity (All [x] [x -> x]))
```

The previous type (in Typed Clojure syntax) reads: "for all types x, is a function from x to x".

Like a function, polymorphic types need to be applied (_instantiated_)
to be useful. Let's see how to write a polymorphic type in typed.clj.spec,
and then how to instantiate one.

### Writing polymorphic specs

The `t/all` macro introduces a polymorphic spec. A polymorphic spec
has two components: a binder and a body. The binder represents an
ordered list of of type variables which are scoped over the body.
Here's a polymorphic spec for `clojure.core/identity`.

```clojure
(s/def
  ::identity
  (t/all :binder (t/binder :x (t/bind-tv))
         :body (s/fspec :args (s/cat :x (t/tv :x))
                        :ret (t/tv :x))))
```

Compare this to the same type in Typed Clojure:

```clojure
(ann identity (All [x] [x -> x]))
```

Notice that the binder `[x]` is replaced with a `t/binder` call, and type variable
occurrences `x` are now `(t/tv :x)`. typed.clj.spec uses simple keywords
to refer to type variables.

Let's jump to the fun part: generating example calls!
Think of the craziest implementation of `identity` you
can muster, then throw it against `::identity`.

Here are some to get you started:

```clojure
(assert (s/valid? ::identity (fn [x] x)))
(assert (s/valid? ::identity #(-> {}
                                  (update % vector %)
                                  (update % peek)
                                  (get %))))
(assert (s/valid? ::identity (comp first
                                   (juxt #(apply % []) identity)
                                   (fn [x]
                                     (constantly x)))))
```

Here's my favorite variant. It uses a strongly normalizing (terminating) term that it's been proven
_untypable_ in [System F](https://en.wikipedia.org/wiki/System_F), aka., the second-order lambda
calculus. 

```clojure
(defn id [input]
  (let [I (fn [a] a)
        K (fn [b]
            (fn [c]
              b))
        D (fn [d]
            (d d))]
    (let [GR ((fn [x]
                (fn [y]
                  ((y (x I))
                   (x K))))
              D)]
      (GR (fn [_]
            (fn [_]
              input))))))
```

`GR` was discovered by [Giannini and Rocca](https://www.computer.org/csdl/proceedings-article/lics/1988/00005101/12OmNBQC87f) 
in the late 80's. You can read a bit more about it and why it's relevant to Typed Clojure on
p127 of [my dissertation](https://ambrosebs.com/files/ambrosebs-phd-thesis.pdf),
under "Higher-order Control Flow Analysis" -- basically, it's checkable via symbolic execution!
On p129 I talk about how I accidentally found how to use `GR` as the identity function
by playing around a bit with symbolic execution.

Try and verify yourself `id` actually implements `identity` by using generative testing.

### Instantiating polymorphic specs

```clojure
(s/def
  ::identity
  (t/all :binder (t/binder :x (t/bind-tv))
         :body (s/fspec :args (s/cat :x (t/tv :x))
                        :ret (t/tv :x))))
```

We can manually instantiate a polymorphic binder using `t/inst`.
Remember, `:x` stands for a spec here, so we provide a _substitution_
from type variable names to specs.

```clojure
(s/describe (t/inst ::identity {:x integer?}))
;=> (fspec :args (cat :x integer?) :ret integer?)


(s/describe (t/inst ::identity {:x any?}))
;=> (fspec :args (cat :x any?) :ret any?)
```

`t/inst` can also fill in any missing variables in a substitution.

```clojure
(s/describe (t/inst ::identity {}))
;=> (fspec :args (cat :x any?) :ret any?)
```

### Generation strategy

In the section introducing
`s/fspec` we already saw one way to peek under the covers
of the generating testing strategy.
Let's look from another angle.

One of my favorite features of spec is `gen/sample` ---
if you're unsure of what a spec generates, just sample it!
We can dissect the above spec similarly.

The first interesting part is the `:binder`.
It expects a _substitution_ that instantiates the
type variables, so then it can substitute away the `t/tv` forms
in the body to create a valid spec.

We can understand the shape of a substitution by calling
`gen/sample` on a `t/binder`.

```clojure
(-> (t/binder :x (t/bind-tv))
    s/gen 
    gen/sample)
;=> ({:x #{{C/U_080 -15/11,
;           518427724599601687001949897925738120598577465472666077502943101692301721 -!Fy6/uEIZY,
;           0.0013707074005742115 -27/44,
;           "Uh" 24/17,
;           :Yn0-n:_0 \`}}}
;    {:x #{#{()}}}
;    {:x #{[-6.0 33267649668820513]}}
;    {:x #{{}}}
;    {:x #{nil}}
;    {:x #{#{HDwv?}}}
;    {:x #{[[#{-3.824264209922906E-152
;              .Mg-
;              j+_
;              4.143384697366724E61
;              F?w+8**FG_/w_g?o
;              5/8
;              9100308681477659241077706867786655190902894931345205951770259557960950829084131025622763779598130888851398113843421240836538174787
;              "4&8.hyOFGkJRxK`IhN 97d%sC5%+msR$` Z`o+%oF`kwUv("
;              -6.792916776722165E-53
;              "K{2\\vh!+<]c$}]V).0"}
;            :f!+89:_!!hGd]]}}
;    {:x #{#{}}}
;    {:x #{()}}
;    {:x #{{() false}}})
```

```
... Tutorial work in progress ...
```
