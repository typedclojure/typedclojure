- lib.core.async improvements
  - register annotations/extensions without loading core.async
  - don't load `typed.clojure`  in macros namespace
- add support for `(requiring-resolve 'fully-qualified/sym)`
  - assumes that `fully-qualified` is not an alias in any namespace
  - at type checking time, if `(requiring-resolve 'fully-qualified/sym)` evaluates
    to a var, and a type annotation is found for `fully-qualified/sym`, then
    returns type `(t/Var (t/TypeOf fully-qualified/sym))`
