# clj-kondo Bug Reproduction

This directory contains a minimal reproduction case for a bug introduced in clj-kondo v2025.07.26.

## The Bug

Starting with clj-kondo v2025.07.26, macro hooks that use `requiring-resolve` at macro expansion time fail with:

```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

This appears to be related to changes in how clj-kondo initializes the SCI (Small Clojure Interpreter) context for macro expansion.

## Binary Search Results

- **Last GOOD version**: v2025.06.05 (commit: 341364fe5befe7e5908c085a6bf80a9042134c15)
- **First BAD version**: v2025.07.26 (commit: b77888454e0695905f225b2e88f6d3b1dd75259a)

Between these commits, there are 28 commits. The issue appears to be related to commit ff760b85 "Get rid of caching (#2574)" or surrounding optimization commits.

## Prerequisites

- Java 8 or later
- Clojure CLI tools (https://clojure.org/guides/install_clojure)

## Running the Reproduction

### Test with GOOD commit (v2025.06.05)

```bash
./test-good.sh
```

**Expected output:**
```
src/minimal_test.clj:9:20: error: Expected: number, received: keyword.
```

The macro hook successfully expands `t/def` and `t/defn`, only reporting the intentional error.

### Test with BAD commit (v2025.07.26)

```bash
./test-bad.sh
```

**Expected output:**
```
src/minimal_test.clj:6:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/minimal_test.clj:9:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/minimal_test.clj:9:20: error: Expected: number, received: keyword.
```

The SCI context errors appear in addition to the expected error, indicating the macro hooks fail to expand properly.

## What's Happening

The Typed Clojure macro hooks in `.clj-kondo/org.typedclojure/typed.clj.runtime/clojure/core/typed/macros.clj` use `requiring-resolve` to dynamically resolve helper functions at macro expansion time:

```clojure
(defmacro def [name & fdecl]
  (core/let [[docstring fdecl] ((requiring-resolve 'clojure.core.typed.internal/take-when) string? fdecl)
             ;; ...
```

In clj-kondo v2025.06.05 and earlier, the SCI context is properly initialized when these macros expand. In v2025.07.26 and later, the context is not available, causing the error.

## Files

- `src/minimal_test.clj` - Minimal Clojure file using Typed Clojure macros
- `deps.edn` - Dependencies with two aliases (`:good-clj-kondo` and `:bad-clj-kondo`)
- `test-good.sh` - Script to test with the good commit
- `test-bad.sh` - Script to test with the bad commit
- `.clj-kondo/` - clj-kondo configuration with Typed Clojure macro hooks (copied by test scripts)

## For clj-kondo Maintainers

This appears to be a regression introduced by changes to SCI context initialization around v2025.07.26. The macro hooks rely on `requiring-resolve` being available in the SCI context during macro expansion.

Possible commits of interest:
- ff760b85 "Get rid of caching (#2574)"
- 0c399693 "localized memoize (#2573)"
- b4978ccf "Per file cache"

These optimization/caching changes may have affected how the SCI context is set up or maintained during macro expansion.
