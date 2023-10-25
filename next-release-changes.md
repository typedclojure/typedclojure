- introduce the kind of Types
  - `t/Type` is the kind of all types
  - can constrain by wrapping parens:
    - `(t/Type :< upper :> lower)`
- combine type and dotted variable environments
- Breaking: nesting dotted variable expansions are now disallowed
  - since the dotted variable is scoped as a regular var before
    the dots and shadows the dotted variable
  - e.g., `(t/All [b :..] ['[b :.. b] :.. b -> t/Any])`
- scope dotted variables as kind `(t/* t/Type)`
- check bounds when instantiating type functions and polymorphic types
- add t/Match
  - returns the result of the first (pattern) matching clause
```clojure
(t/Match nil nil :-> nil) ;=> nil
(t/Match t/Int
         nil :-> nil
         t/Int :-> t/Bool) ;=> t/Bool
(t/Match (t/Seqable t/Num)
         [[E :< t/Int]] (t/Seqable E) :-> '[':first E]
         [E] (t/Seqable E) :-> '[':second E])
;=> '[':second t/Num]"
```
- nil <: (ExactCount 0)
- add `t/SeqOn`
  - returns the return type of `seq` for given argument type
  - `(t/SeqOn nil) => nil`
  - `(t/SeqOn '[1 2]) => (t/HSeq :types ['1 '2])`
  - `(t/SeqOn (t/Seqable t/Any)) => (t/NilableNonEmptyASeq t/Any)`
- change `clojure.core/seq` to use `t/SeqOn`
- add new object path element `Seq`, which represents the result of calling `clojure.core/seq`
  - replaces old filters on `clojure.core/seq` annotation
- `cc/butlast` returns a nilable non-empty seq
- replacements in `override-class` are more intuitive
  - just provide replacements for the `clojure.core/bases` instead of `clojure.core/supers`
  - all ancestors must agree on their type parameters, like in Java
    - same class cannot extend both `(Seqable A)` and `(Seqable B)` transitively
        - must have a canonical list of arguments for each ancestor
- Fix [#121](https://github.com/typedclojure/typedclojure/issues/121): clojure.core.typed/envs was broken
- Fix [#122](https://github.com/typedclojure/typedclojure/issues/122): cyclic dependencies when using clojure.core.typed/ann without loading typed.clojure
- bump to clojure 1.12.0-alpha5, add jdk 21 to test matrix
- fix simplification of intersected RClass's by respecting variance
- fix overlap calculation of RClass's
