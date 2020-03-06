# Intersections and negations

## Problem: (Not Integer)

- difficult to generate for

```
(s/or :integer integer?
      :not-integer? <???>)
```

- Semantic subtyping relation (or constraint solving) may be useful
  - some incantation to generate a subtype of (Not Integer)

## Problem: (IFn [Integer -> true] [(Not Integer) -> false])

- Same reasons as `(Not Integer)` is difficult

Workaround I thought might work, but doesn't:

```clojure
(t/all :binder (t/binder :a (bind-tv))
       :body (s/fspec :args (s/cat :a (tv :a))
                      ; unfortunately % is a spec, not a value. would need a subtyping
                      ; relation, back to where we started.
                      :ret (tv :a :wrap #(-> % ??? (not ??? (integer? ???))))))
```

Standard workaround with fspec :fn that doesn't s/gen well:

```clojure
(t/all :binder (t/binder :a (bind-tv))
       :body (s/fspec :args (s/cat :a (tv :a))
                      ; the :fn is very specific, s/fspec will generate
                      ; examples based on :args and :ret only. eg., {:args {:a 1} :ret false}
                      ; or {:args {:a 'a} :ret true}.
                      :fn (fn [{{:keys [a]} :args :keys [ret]}]
                            (= ret (integer? a)))
                      :ret boolean?))
```

## Problem: (I a b)

- can't use s/and because of the naive s/gen strategy
- again, possibly semantic subtyping to the rescue?
