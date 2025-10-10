#!/bin/bash
# Test with BAD clj-kondo commit (v2025.07.26)
# Expected: SCI context error

set -e

cd "$(dirname "$0")"
echo "=== Testing with BAD clj-kondo commit (v2025.07.26) ==="

# Expected output - must contain the SCI context error
EXPECTED_SCI_ERROR="reproduction.clj:7:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!"

# Run clj-kondo and capture output
ACTUAL_OUTPUT=$(clojure -M:bad-clj-kondo --lint reproduction.clj 2>&1 | grep -v "linting took" | grep -v "Downloading:" | grep -v "Checking out:" | grep -v "Cloning:" || true)

# Check if the expected SCI context error appears
if echo "$ACTUAL_OUTPUT" | grep -q "No context found in: sci.ctx-store"; then
    echo "PASS: SCI context error appears as expected"
    echo "Error message:"
    echo "$EXPECTED_SCI_ERROR"
    exit 0
else
    echo "FAIL: Expected SCI context error not found"
    echo "Expected to find:"
    echo "$EXPECTED_SCI_ERROR"
    echo ""
    echo "Actual output:"
    echo "$ACTUAL_OUTPUT"
    exit 1
fi
