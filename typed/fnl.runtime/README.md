# typed.fnl.runtime

Runtime support for Typed Fennel - type annotations for Fennel code.

## Overview

This subproject provides runtime files for annotating Fennel code with types. It includes:

- **Macro module** (`src/typed/fennel.fnl`): Provides the `ann` macro for annotating variables with types
- **Type constructors**: Basic type constructors like `Str` for defining types

## Installation

Via [deps.fnl](https://gitlab.com/andreyorst/deps.fnl):

```clojure
{:deps {"https://github.com/typedclojure/typedclojure"
        {:type :git :sha "8b00133a4b3598967ad9ffaa3ed2d505af8720cb"}}}
```

## Usage

Import the macro module in your Fennel code:

```fennel
(import-macros t :typed.fennel)

;; Annotate a function with type signature
(t.ann demo-func [t.Str :-> t.Str])

;; Define the function
(fn demo-func [x]
  x)
```

## Development

### Running Tests

```bash
./script/test
```

This will run the demo file in `test/demo.fnl` using the vendored Fennel interpreter.

## License

typed.fnl.reader is copyright © 2016-2025 Calvin Rose and contributors, released under the MIT license.

All other code is:

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
