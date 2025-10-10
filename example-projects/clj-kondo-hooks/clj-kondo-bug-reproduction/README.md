# clj-kondo Bug Reproduction

Minimal reproduction of SCI context bug introduced in clj-kondo v2025.07.26.

## The Bug

Macro hooks using `requiring-resolve` fail with:
```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

## Files

- `reprod/test.clj` (6 lines) - Minimal macro definition and usage
- `.clj-kondo/reprod/reprod/hooks/reprod_hooks.clj` (6 lines) - Minimal macro hook with `requiring-resolve`
- `.clj-kondo/reprod/reprod/config.edn` (1 line) - Config mapping
- `deps.edn` (19 lines with comments) - Git dependencies for GOOD and BAD commits
- `test-good.sh` / `test-bad.sh` - Test scripts with expected output validation

Total: 24 lines of code + test scripts

## Run

Prerequisites: Java and Clojure CLI

```bash
./test-good.sh  # PASS - No errors
./test-bad.sh   # PASS - Shows expected SCI context error
```

Both scripts validate output against expected results verbatim and exit with appropriate status codes. No superfluous warnings.

## Root Cause

**GOOD commit** (99bb37c4a03c6c036067a5b24a8fe2d937f541e4):
- Committed: July 24, 2025 11:20 AM - "minor whitespace"  
- SCI version: 0.9.44
- SCI context properly initialized for macro expansion
- `requiring-resolve` works correctly in macro hooks

**BAD commit** (e43c24186bd77c659357f2ed1f862f80077d0f6a):
- Committed: July 24, 2025 11:37 AM - "Bump SCI"
- SCI version: 0.10.47 (bumped from 0.9.44)
- SCI context not available during macro expansion
- `requiring-resolve` fails with: "No context found in: sci.ctx-store/*ctx*"

**Distance between commits:** 0 (adjacent)

The bug was introduced when clj-kondo bumped SCI from version 0.9.44 to 0.10.47. This SCI version change broke the context initialization for macro hooks.
