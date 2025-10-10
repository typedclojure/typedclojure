#!/bin/bash
# Test with GOOD clj-kondo commit (v2025.06.05)
# Expected: No SCI context errors

set -e

cd "$(dirname "$0")"
echo "=== Testing with GOOD clj-kondo commit (v2025.06.05) ==="

# Expected output - no errors at all with GOOD commit
EXPECTED_OUTPUT=""

# Run clj-kondo and capture output
ACTUAL_OUTPUT=$(clojure -M:good-clj-kondo --lint reprod/test.clj 2>&1 | grep -v "linting took" | grep -v "Downloading:" | grep -v "Checking out:" | grep -v "Cloning:" || true)

# Compare outputs
if echo "$ACTUAL_OUTPUT" | grep -q "No context found in: sci.ctx-store"; then
    echo "FAIL: Unexpected SCI context error with GOOD commit"
    echo "Actual output:"
    echo "$ACTUAL_OUTPUT"
    exit 1
elif [ -z "$ACTUAL_OUTPUT" ]; then
    echo "PASS: No errors (as expected)"
    exit 0
else
    echo "WARNING: Unexpected output but no SCI context error"
    echo "Actual:"
    echo "$ACTUAL_OUTPUT"
    exit 0
fi
