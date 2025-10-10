# clj-kondo-hooks Test Failure Report

## Summary

The clj-kondo-hooks tests are failing due to a bug introduced in **clj-kondo v2025.07.26**.

## Binary Search Results

| Version | Release Date | Status | Notes |
|---------|--------------|--------|-------|
| v2024.03.13 | Mar 13, 2024 | ✅ GOOD | Tests pass |
| v2024.09.27 | Sep 27, 2024 | ✅ GOOD | Tests pass |
| v2024.11.14 | Nov 14, 2024 | ✅ GOOD | Tests pass |
| v2025.04.07 | Apr 7, 2025 | ✅ GOOD | Tests pass |
| v2025.06.05 | **Jun 5, 2025** | ✅ GOOD | **Last working version** |
| v2025.07.26 | **Jul 26, 2025** | ❌ BAD | **Bug introduced** |
| v2025.07.28 | Jul 28, 2025 | ❌ BAD | Bug persists |
| v2025.09.19 | Sep 19, 2025 | ❌ BAD | Bug persists |
| v2025.09.22 | Sep 22, 2025 | ❌ BAD | Bug persists (latest) |

## Root Cause

clj-kondo v2025.07.26 changed how it initializes the SCI (Small Clojure Interpreter) context for macro expansion.

The Typed Clojure macro hooks in `typed/clj.runtime/resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/clojure/core/typed/macros.clj` use `requiring-resolve` at macro expansion time (e.g., to resolve `clojure.core.typed.internal/parse-fn*`, `parse-defn*`, etc.).

In clj-kondo v2025.07.26 and later, the SCI context is not properly initialized when these macros expand, causing the error:

```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

## Error Examples

The failing version produces errors like:

```
src/typed_example/clj_kondo_hooks.clj:32:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/typed_example/clj_kondo_hooks.clj:32:19: error: Expected: number, received: keyword.
src/typed_example/clj_kondo_hooks.clj:42:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/typed_example/clj_kondo_hooks.clj:42:7: error: Unresolved symbol: tfn1
src/typed_example/clj_kondo_hooks.clj:49:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
src/typed_example/clj_kondo_hooks.clj:49:10: error: Unresolved symbol: a
src/typed_example/clj_kondo_hooks.clj:50:3: error: Unexpected usage of recur.
```

The working version produces only the expected errors (26 lines) without the SCI context errors.

## Conclusion

This is a **clj-kondo bug**, not a Typed Clojure issue. The macro hooks worked correctly in all versions through v2025.06.05.

## Recommendations

1. **Report to clj-kondo**: File an issue with the clj-kondo project about the SCI context initialization regression in v2025.07.26.

2. **Workaround**: Users can downgrade to clj-kondo v2025.06.05 or earlier until the bug is fixed.

3. **Alternative fix**: If clj-kondo cannot fix this, Typed Clojure's macro hooks would need to be rewritten to avoid using `requiring-resolve` at macro expansion time. However, this would require significant refactoring and should only be done if clj-kondo confirms this is intentional behavior rather than a bug.
