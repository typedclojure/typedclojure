# typed.fnl.reader

Reader for Fennel source code. Converts Fennel syntax to Clojure data structures.

This is a port of the Fennel parser (src/fennel/parser.fnl) to Clojure. It converts Fennel forms into Clojure-friendly data structures for use with the type checker.

## Usage

```clojure
(require '[typed.fnl.reader :as fnl-reader])

;; Read a single Fennel form
(fnl-reader/read-string "(fn demo-func [x] x)")
;; => (fn demo-func [x] x)

;; Read all forms from a string
(fnl-reader/read-all "(fn [x] x) (+ 1 2)")
;; => [(fn [x] x) (+ 1 2)]

;; Read forms from a file
(fnl-reader/read-file "path/to/file.fnl")
```

## License

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors

Distributed under the Eclipse Public License 1.0
