# typed.fnl.runtime

Runtime support for Typed Fennel - type annotations for Fennel code.

## Overview

This subproject provides runtime files for annotating Fennel code with types. It includes:

- **Macro module** (`src/typed/fennel.fnl`): Provides the `ann` macro for annotating variables with types
- **Type constructors**: Basic type constructors like `Str` for defining types
- **Vendored Fennel**: Includes Fennel interpreter for development and testing

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

Copyright © Ambrose Bonnaire-Sergeant, Rich Hickey & contributors.

Licensed under the EPL (see the file epl-v10.html).
