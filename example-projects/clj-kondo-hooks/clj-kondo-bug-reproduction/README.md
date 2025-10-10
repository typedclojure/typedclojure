# clj-kondo Bug Reproduction

Minimal reproduction of SCI context bug in clj-kondo v2025.07.26.

## The Bug

Macro hooks using `requiring-resolve` fail with:
```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

## Files

- `reproduction.clj` - Defines and uses a macro with `requiring-resolve`
- `.clj-kondo/reprod/reprod/hooks.clj` - The macro hook  
- `deps.edn` - Git dependencies for GOOD (v2025.06.05) and BAD (v2025.07.26) commits
- `test-good.sh` / `test-bad.sh` - Test scripts

## Run

Prerequisites: Java and Clojure CLI

```bash
./test-good.sh  # Works (v2025.06.05)
./test-bad.sh   # Fails with SCI error (v2025.07.26)
```

## Root Cause

GOOD commit (341364fe5befe7e5908c085a6bf80a9042134c15 - v2025.06.05):
- SCI context properly initialized for macro expansion
- `requiring-resolve` works in macro hooks

BAD commit (b77888454e0695905f225b2e88f6d3b1dd75259a - v2025.07.26):
- Likely culprit: ff760b85 "Get rid of caching (#2574)"
- SCI context not available during macro expansion
- `requiring-resolve` fails with context error

The caching refactor appears to have changed when/how the SCI context is initialized, breaking macro hooks that depend on it.
