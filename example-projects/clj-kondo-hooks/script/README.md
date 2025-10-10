# Using the Binary Search Scripts

This directory contains automated scripts for finding bugs in clj-kondo releases, commits, and dependencies.

## Quick Start

### 1. Find the Bad Release

Start with the release-level binary search (faster):

```bash
cd example-projects/clj-kondo-hooks
./script/bisect-release
```

This will:
- Test releases between 2024-2025
- Use pre-compiled binaries for speed
- Output the first bad release and the last good release

### 2. Find the Exact Bad Commit

Once you know the bad release, drill down to the exact commit:

```bash
./script/bisect-commit [GOOD_COMMIT] [BAD_COMMIT]
```

Or use the default known commits from v2025.06.05 to v2025.07.26:

```bash
./script/bisect-commit
```

This will:
- Use git dependencies to test specific commits
- Perform binary search on commits between the releases
- Output the exact commit that introduced the bug
- Verify that found commits are adjacent (distance = 0)

### 3. (Optional) Bisect a Dependency

**Only if** you've identified that the bad commit is a dependency version bump (e.g., "Bump SCI"), you can bisect the dependency itself:

```bash
./script/bisect-dependency <main-sha> <dep-coordinate> <good-dep-sha> <bad-dep-sha>

# Example:
./script/bisect-dependency e43c24186 org.babashka/sci a1b2c3d e5f6g7h
```

This will:
- Clone the dependency repository
- Binary search through dependency commits
- Test each dependency commit with the main project SHA
- Find the exact dependency commit that introduced the bug

**Warning:** Only use this when you're certain a dependency is at fault. It's time-consuming and should be used judiciously.

## How It Works

### bisect-release

1. Clones the clj-kondo repository to get release tags
2. Finds all releases in the 2024-2025 range
3. Performs binary search by:
   - Installing each release as a binary
   - Running `./script/test`
   - Comparing output with expected results
4. Narrows down to the first bad release

### bisect-commit

1. Takes a range of commits (or uses defaults)
2. Performs binary search by:
   - Updating `deps.edn` with a git dependency for each commit
   - Ensuring the clj-kondo binary doesn't take precedence
   - Running `./script/test` with the git dependency
3. Outputs the exact commit that introduced the bug
4. Verifies that found commits are adjacent (distance = 0)
5. Shows the commit message and suggests viewing the diff

### bisect-dependency

1. Takes a main project commit SHA and dependency information
2. Clones the dependency repository
3. Finds all commits between GOOD and BAD dependency SHAs
4. Performs binary search by:
   - Updating `deps.edn` to use the main SHA with each dependency commit
   - Running the test scripts
   - Determining if each commit is GOOD or BAD
5. Outputs the first BAD dependency commit
6. Checks if the found commits are adjacent

**When to use bisect-dependency:**
- The main project's BAD commit is a dependency version bump
- You've verified the old dependency version works
- You're certain the dependency is at fault
- You have time for thorough investigation

## Example Output

```
=== clj-kondo Release Binary Search ===

Finding release tags...
Total releases found: 15
Release range: v2024.02.12 to v2025.09.22

Starting binary search...

[8/15] Checking midpoint...
Testing release: v2025.04.07 (2025.04.07)
  Result: GOOD ✓

[12/15] Checking midpoint...
Testing release: v2025.07.26 (2025.07.26)
  Result: BAD ✗

...

=== Binary Search Complete ===

First BAD release found: v2025.07.26

Release commit: b77888454e0695905f225b2e88f6d3b1dd75259a

Previous GOOD release: v2025.06.05
Previous GOOD commit:  341364fe5befe7e5908c085a6bf80a9042134c15

To find the exact commit that introduced the bug, run:
  cd example-projects/clj-kondo-hooks
  ./script/bisect-commit 341364fe5befe7e5908c085a6bf80a9042134c15 b77888454e0695905f225b2e88f6d3b1dd75259a
```

## Prerequisites

- Java (for running Clojure)
- Clojure CLI tools
- Git
- curl, unzip (for downloading releases)

## Notes

- The bisect-commit script temporarily modifies `deps.edn` but restores it after each test
- The scripts assume tests pass when `./script/test` exits with status 0
- If you have a clj-kondo binary in your PATH, bisect-commit will temporarily rename it to ensure the git dependency is used
