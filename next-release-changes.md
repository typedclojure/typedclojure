- improve `clojure.core/reductions` annotation
  - 2-arity must support zero-args on 1 arg
    - except when input collection is non-empty
  - add support for `reduced` values
  - return seq is always non-empty
- fix `clojure.core/reduce` annotation
  - result of calling first arg with zero-args does not unwrap `reduced`
  - use one type variable in 2-arity
- annotate `clojure.core/keep` transducer arity
- `clojure.core/keep` never returns nil, update annotation appropriately
- annotate transducers for `filter`, `remove`, `{take,drop}-while`, `partition-all`, `distinct`,
  `interpose`, `{map,keep}-indexed`, 
- return non-empty seq types in core annotations if infinite or known non-empty
- `keep` accepts `Seqable`, not just `Coll`
- annotate 1-arity of `drop-last`
- annotate `keyword` 1-arity to accept a var
- annotate clojure.core vars: `iteration`, `partitionv`, `parse-{uuid,boolean,double,long}`, `update-{keys,vals}`, `abs`, `NaN?`, `infinite?`, `*repl*`, `replace`,
  `partition-by`, `cat`, `dedupe`, `random-sample`, `eduction`, `tagged-literal{?}`, `reader-conditional{?}`, `unreduced`, `halt-when`, `ensure-reduced`, `completing`,
  `transduce`, `sorted-map-by`, `rational?`, `float?`, `{r}subseq`
- clojure.math annotations
- annotate 12 arities of `comp`
- add type parameter to clojure.lang.Sorted
- support seqables of map entries in `keys` and `vals`
- fix return type of `peek` in most general case (nilable return)
- iteratively check symbolic closures against types with invariant type variables
  - enables inference of `(reduce (fn [a b] (+ a b)) [1])`
- introduce wildcard type `t/?`, like `t/Any` but downcasted when checked against a more specific type
- don't recreate types in internal folding when types are unchanged after walking
