# Bug Report for clj-kondo Issue Tracker

**version**

Reproduced with clj-kondo v2025.07.26 and later (including v2025.09.22).
Last working version: v2025.06.05.

**macro usage**

This bug affects macro hooks that use `requiring-resolve` during macro expansion.

**platform**

Both JVM and native versions exhibit the issue.

**problem**

After upgrading from SCI 0.9.44 to 0.10.47 in commit e43c24186bd77c659357f2ed1f862f80077d0f6a, macro hooks using `requiring-resolve` fail with:

```
error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
```

**repro**

Minimal reproduction available at:
https://github.com/typedclojure/typedclojure/tree/copilot/fix-clj-kondo-macro-hooks/example-projects/clj-kondo-hooks/clj-kondo-bug-reproduction

To reproduce locally:
```bash
git clone https://github.com/typedclojure/typedclojure
cd typedclojure
git checkout copilot/fix-clj-kondo-macro-hooks
cd example-projects/clj-kondo-hooks/clj-kondo-bug-reproduction
./test-good.sh  # Works with clj-kondo using SCI 0.9.44
./test-bad.sh   # Fails with clj-kondo using SCI 0.10.47
```

The reproduction contains:
- `reprod/test.clj` - minimal macro definition
- `.clj-kondo/reprod/reprod/hooks/reprod_hooks.clj` - minimal hook using `requiring-resolve`
- `deps.edn` - git dependencies for adjacent GOOD/BAD commits
- Two test scripts demonstrating working vs broken behavior

**config**

Config used (`.clj-kondo/reprod/reprod/config.edn`):
```clojure
{:hooks {:analyze-call {reprod.test/reprod reprod.reprod.hooks.reprod-hooks/reprod}}}
```

**expected behavior**

Macro hooks should be able to use `requiring-resolve` during macro expansion without SCI context errors. This worked in clj-kondo with SCI 0.9.44 but broke after upgrading to SCI 0.10.47.

The issue appears to be an incompatibility between clj-kondo's SCI integration and changes in SCI 0.10.x regarding context initialization. Either:
1. clj-kondo needs to update its SCI integration for 0.10.x APIs, or
2. This is a regression in SCI that should be fixed upstream

**additional context**

- Bug introduced in clj-kondo commit: e43c24186bd77c659357f2ed1f862f80077d0f6a ("Bump SCI")
- Previous working commit: 99bb37c4a03c6c036067a5b24a8fe2d937f541e4
- SCI versions: 0.9.44 (working) → 0.10.47 (broken)
- 71 commits between SCI versions
