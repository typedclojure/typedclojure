# clj-kondo Bug Reproduction

Minimal reproduction of SCI context bug in clj-kondo v2025.07.26.

## The Bug

Macro hooks using `requiring-resolve` fail with:
```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

## Files

- `reproduction.clj` (7 lines) - Defines and uses a minimal macro with `requiring-resolve`
- `.clj-kondo/reprod/reprod/hooks/reprod_hooks.clj` (6 lines) - Minimal macro hook
- `.clj-kondo/reprod/reprod/config.edn` (1 line) - Config mapping
- `deps.edn` (11 lines) - Git dependencies for GOOD (v2025.06.05) and BAD (v2025.07.26) commits
- `test-good.sh` / `test-bad.sh` - Test scripts with expected output validation

Total: 25 lines of code + test scripts

## Run

Prerequisites: Java and Clojure CLI

```bash
./test-good.sh  # PASS - No SCI context errors (v2025.06.05)
./test-bad.sh   # PASS - Shows expected SCI context error (v2025.07.26)
```

Both scripts validate output against expected results and exit with appropriate status codes.

## Root Cause

**GOOD commit** (341364fe5befe7e5908c085a6bf80a9042134c15 - v2025.06.05):
- SCI context properly initialized before macro expansion
- `requiring-resolve` works correctly in macro hooks
- Macro hook expands without errors

**BAD commit** (b77888454e0695905f225b2e88f6d3b1dd75259a - v2025.07.26):
- Likely culprit: ff760b85 "Get rid of caching (#2574)"  
- SCI context not available during macro expansion
- `requiring-resolve` fails with: "No context found in: sci.ctx-store/*ctx*"
- Caching refactor changed when/how SCI context is initialized

The caching refactor appears to have changed the initialization order, causing the SCI context to be unavailable when macro hooks are expanded.
