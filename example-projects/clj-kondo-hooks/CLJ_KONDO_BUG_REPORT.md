# Bug Report for clj-kondo Maintainers

## Summary

A regression was introduced in clj-kondo v2025.07.26 that breaks macro hooks using `requiring-resolve` at macro expansion time.

## Binary Search Results

- **Last working version**: v2025.06.05 (commit: 341364fe5befe7e5908c085a6bf80a9042134c15)
- **First broken version**: v2025.07.26 (commit: b77888454e0695905f225b2e88f6d3b1dd75259a)
- **Commits in between**: 28 commits

## The Error

When macro hooks use `requiring-resolve` during macro expansion, they fail with:

```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

## Root Cause Analysis

The issue appears to be related to changes in SCI (Small Clojure Interpreter) context initialization for macro expansion. Between v2025.06.05 and v2025.07.26, several optimization/caching commits were made:

- **ff760b85** - "Get rid of caching (#2574)" - Most likely culprit
- **0c399693** - "localized memoize (#2573)"
- **b4978ccf** - "Per file cache"

These changes likely affected how the SCI context is set up or maintained during macro expansion, causing `requiring-resolve` to fail when the context is not available.

## Minimal Reproduction

A complete minimal reproduction is available in the `clj-kondo-bug-reproduction/` directory. It demonstrates:

1. **Working behavior** (v2025.06.05): Macro hooks expand successfully
2. **Broken behavior** (v2025.07.26): SCI context errors appear

### Prerequisites

- Java 8 or later
- Clojure CLI tools

### Running the Reproduction

```bash
cd clj-kondo-bug-reproduction

# Test with good commit - works correctly
./test-good.sh

# Test with bad commit - shows SCI context error
./test-bad.sh
```

## Expected vs Actual Behavior

### Expected (v2025.06.05)

```
src/minimal_test.clj:9:16: error: Expected: number, received: keyword.
```

Only the intentional error is reported. The macro hooks successfully expand.

### Actual (v2025.07.26)

```
src/minimal_test.clj:6:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/minimal_test.clj:9:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/minimal_test.clj:9:21: error: Expected: number, received: keyword.
```

SCI context errors appear because the macro hooks fail to expand properly.

## Impact

This regression affects any clj-kondo macro hooks that use `requiring-resolve` or other functions requiring an active SCI context during macro expansion. This is a common pattern for macro hooks that need to resolve helper functions dynamically.

## Suggested Fix

The SCI context initialization logic that was changed between v2025.06.05 and v2025.07.26 should be reviewed. The context needs to be available during the full macro expansion phase, not just during evaluation.

Specifically, investigate commits:
1. ff760b85 "Get rid of caching (#2574)"
2. 0c399693 "localized memoize (#2573)"  
3. b4978ccf "Per file cache"

To ensure the SCI context (`sci.ctx-store/*ctx*`) is properly initialized and available when macro hooks are expanded.

## Additional Context

This was discovered while testing Typed Clojure's clj-kondo macro hooks. The hooks have worked correctly from clj-kondo v2024.03.13 through v2025.06.05, confirming this is a recent regression.
