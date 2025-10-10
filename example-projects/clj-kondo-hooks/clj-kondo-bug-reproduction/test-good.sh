#!/bin/bash
# Test with GOOD clj-kondo commit (v2025.06.05)
# Expected: No SCI context errors

set -e

cd "$(dirname "$0")"
echo "=== Testing with GOOD clj-kondo commit (v2025.06.05) ==="

# Expected output (ignoring namespace mismatch warning which is not the bug)
EXPECTED_OUTPUT=$(cat <<'EOF'
reproduction.clj:1:5: error: Namespace name does not match file name: reprod.test
EOF
)

# Run clj-kondo and capture output
ACTUAL_OUTPUT=$(clojure -M:good-clj-kondo --lint reproduction.clj 2>&1 | grep -v "linting took" | grep -v "Downloading:" | grep -v "Checking out:" | grep -v "Cloning:" || true)

# Compare outputs
if echo "$ACTUAL_OUTPUT" | grep -q "No context found in: sci.ctx-store"; then
    echo "FAIL: Unexpected SCI context error with GOOD commit"
    echo "Actual output:"
    echo "$ACTUAL_OUTPUT"
    exit 1
elif [ "$ACTUAL_OUTPUT" = "$EXPECTED_OUTPUT" ]; then
    echo "PASS: Output matches expected (no SCI context errors)"
    exit 0
else
    echo "WARNING: Output differs but no SCI context error"
    echo "Expected:"
    echo "$EXPECTED_OUTPUT"
    echo ""
    echo "Actual:"
    echo "$ACTUAL_OUTPUT"
    exit 0
fi
