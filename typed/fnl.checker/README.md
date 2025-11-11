# typed.fnl.checker

Type checker for Fennel code.

Note: Fennel type checking is not yet fully implemented. This is a stub for future development.

## Usage

```clojure
(require '[typed.fnl.checker :as fnl-check])

;; Check a single form
(fnl-check/check-form form expected expected-provided?)

;; Check a namespace
(fnl-check/check-ns 'my.fennel.namespace)

;; Get detailed check information
(fnl-check/check-form-info form)
```

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors

Distributed under the Eclipse Public License 1.0
