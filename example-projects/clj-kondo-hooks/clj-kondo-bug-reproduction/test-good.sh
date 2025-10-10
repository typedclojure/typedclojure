#!/bin/bash
# Test with GOOD clj-kondo commit (v2025.06.05)
# Expected: No errors

set -e

cd "$(dirname "$0")"
echo "=== Testing with GOOD clj-kondo commit (v2025.06.05) ==="

# Expected output - empty (no errors)
EXPECTED_OUTPUT=""

# Run clj-kondo and capture output
ACTUAL_OUTPUT=$(clojure -M:good-clj-kondo --lint reprod/test.clj 2>&1 | grep -v "linting took" | grep -v "Downloading:" | grep -v "Checking out:" | grep -v "Cloning:" || true)

# Compare outputs - must match verbatim (both empty)
if [ "$ACTUAL_OUTPUT" = "$EXPECTED_OUTPUT" ]; then
    echo "PASS: Output matches expected (no errors)"
    echo "Actual output:"
    echo "$ACTUAL_OUTPUT"
    exit 0
else
    echo "FAIL: Output does not match expected"
    echo ""
    echo "Expected (empty):"
    echo "$EXPECTED_OUTPUT"
    echo ""
    echo "Actual:"
    echo "$ACTUAL_OUTPUT"
    exit 1
fi
