# Bidirectional Documentation Testing with Typed Clojure

A system for maintaining synchronized code examples between markdown documentation and type-checked test files, optimized for technical writers.

## Overview

This system enables bidirectional editing: write code in either markdown documentation or test files, and the system keeps them synchronized. Each code block is uniquely identified by a UUID and tracked with version numbers to handle conflicts.

**Important:** Test file names follow Clojure's namespace resolution rules where hyphens in namespaces become underscores in file paths. For example:
- Namespace: `typed-test.doc.example-name-abc123`
- File path: `typed_test/doc/example_name_abc123.clj`

## Quick Start

### 1. Write a code block in markdown

```markdown
## Example

```clojure
(t/ann add [t/Int t/Int :-> t/Int])
(defn add [x y] (+ x y))
\```
```

### 2. Run the sync script

```bash
bb script/sync-doc-tests.clj website/docs/your-doc.md
```

The script will:
1. Generate a UUID for the code block (e.g., `abc12345`)
2. Insert metadata above the code block
3. Create a test file at `typed/clj.checker/test/typed_test/doc/your-doc_abc12345.clj`

### 3. Your markdown now looks like:

```markdown
## Example

<!-- doc-test: id=abc12345 version=1 type=success -->
```clojure
(t/ann add [t/Int t/Int :-> t/Int])
(defn add [x y] (+ x y))
\```
```

### 4. The generated test file:

```clojure
;; doc-test: id=abc12345 version=1 type=success
(ns typed-test.doc.your-doc-abc12345
  (:require [typed.clojure :as t]))

(t/ann add [t/Int t/Int :-> t/Int])
(defn add [x y] (+ x y))
```

### 5. Run the tests

```bash
bb script/run-doc-tests.clj
```

This will type-check all test files and verify they match their expected outcome based on the `type` metadata field.

## Metadata Format

Both markdown and test files contain metadata comments:

**Markdown:**
```html
<!-- doc-test: id=abc12345 version=1 type=success -->
```

**Clojure:**
```clojure
;; doc-test: id=abc12345 version=1 type=success
```

### Metadata Fields

- `id`: Permanent UUID (8 characters) linking code block to test file
- `version`: Integer version number for sync conflict resolution
- `type`: Expected type checking result (e.g., `success`, `fail`)

## Directory Structure

```
typed/clj.checker/test/typed_test/doc/
├── example_abc123.clj  # type=success
└── error_xyz789.clj    # type=fail
```

All test files are in the same directory. The `type` metadata field determines the expected type checking result.

## Bidirectional Sync

### Scenario 1: Edit Markdown

```bash
# 1. Edit code block in markdown
vim website/docs/topic.md

# 2. Increment version in metadata comment
<!-- doc-test: id=abc12345 version=2 type=success -->

# 3. Run sync
bb script/sync-doc-tests.clj website/docs/topic.md

# Result: Test file is updated with new code and version
```

### Scenario 2: Edit Test File

```bash
# 1. Edit test file
vim typed/clj.checker/test/typed_test/doc/topic_abc12345.clj

# 2. Increment version in metadata comment
;; doc-test: id=abc12345 version=2 type=success

# 3. Run sync
bb script/sync-doc-tests.clj website/docs/topic.md

# Result: Markdown code block is updated with new code and version
```

### Scenario 3: Version Conflict (ERROR)

If both markdown and test file are modified with the same version:

```
ERROR: Version conflict for code block abc12345
  Both have version 1 but different content
  Update version in either markdown or test file to resolve
```

**Resolution:** Increment the version number in whichever file has the desired content.

## Version Management

The version number determines which direction changes flow:

| Markdown Version | Test Version | Sync Direction |
|-----------------|--------------|----------------|
| 1 | 1 | ✓ No sync needed (content matches) |
| 2 | 1 | → Markdown to Test |
| 1 | 2 | ← Test to Markdown |
| 2 | 2 | ❌ ERROR if content differs |

**Best Practice:** Always increment the version when making changes.

## Changing Expected Outcome

To change whether a test should pass or fail, update the `type` field in the metadata:

```bash
# Edit the test file
vim typed/clj.checker/test/typed_test/doc/foo_abc12345.clj

# Change the metadata comment from:
;; doc-test: id=abc12345 version=1 type=success
# To:
;; doc-test: id=abc12345 version=2 type=fail

# Run sync to update markdown
bb script/sync-doc-tests.clj website/docs/foo.md
```

## Writing Code Examples

### Examples That Should Type-Check

Default behavior - just write the code:

```markdown
<!-- doc-test: id=generated version=1 type=success -->
```clojure
(t/ann add [t/Int t/Int :-> t/Int])
(defn add [x y] (+ x y))
(add 1 2)
\```
```

### Examples That Should Fail

Use `type=fail`:

```markdown
<!-- doc-test: id=generated version=1 type=fail -->
```clojure
(t/ann add [t/Int t/Int :-> t/Int])
(defn add [x y] (+ x y))
(add "not" "ints")  ; Type error
\```
```

## Scripts

### `script/sync-doc-tests.clj`

Bidirectional sync between markdown and test files.

```bash
# Sync single file
bb script/sync-doc-tests.clj website/docs/topic.md

# Sync all documentation
bb script/sync-doc-tests.clj --all
```

**What it does:**
- Assigns UUIDs to new code blocks
- Creates test files for new code blocks
- Syncs changes based on version numbers
- Detects and reports version conflicts
- Updates metadata in both directions

### `script/run-doc-tests.clj`

Run type checking on all documentation test files.

```bash
bb script/run-doc-tests.clj
```

**What it does:**
- Scans the test directory for all `.clj` files
- Type-checks each test file using `t/check-ns`
- Reads the `type` metadata to determine expected outcome
- Verifies results match expected outcomes
- Reports pass/fail summary
- Exits with error code if any tests fail

## Tips for Technical Writers

### Starting a New Document

1. Write your markdown with code blocks (no metadata needed)
2. Run `bb script/sync-doc-tests.clj your-doc.md`
3. Review generated test files
4. Run tests with `bb script/run-doc-tests.clj`

### Iterating on Examples

**Option A: Edit in Markdown**
1. Edit code block in markdown
2. Update `version=N` to `version=N+1`
3. Run sync script
4. Run tests to verify

**Option B: Edit in Test File**
1. Edit test file directly (better IDE support)
2. Update `version=N` to `version=N+1`
3. Run sync script
4. Markdown automatically updates

### Handling Type Errors

If a test fails unexpectedly:

```bash
# 1. Check what's wrong
bb script/run-doc-tests.clj

# 2. Either fix the code or mark as expected failure
vim typed/clj.checker/test/typed_test/doc/doc_abc12345.clj

# 3. If it should fail, change type to fail and increment version:
# Update: ;; doc-test: id=abc12345 version=2 type=fail

# 4. Sync back to markdown
bb script/sync-doc-tests.clj website/docs/doc.md

# 5. Verify
bb script/run-doc-tests.clj
```

## Troubleshooting

### "Version conflict" error

**Cause:** Both files were edited without version update, or same version used for different content.

**Solution:** 
1. Choose which version is correct (markdown or test)
2. Increment its version number
3. Run sync again

### Test file not found

**Cause:** Metadata has ID but test file doesn't exist.

**Solution:** Sync script will automatically recreate it.

### Code block not syncing

**Cause:** Missing or malformed metadata comment.

**Solution:** Check metadata format matches exactly:
```html
<!-- doc-test: id=abc12345 version=1 type=success -->
```
