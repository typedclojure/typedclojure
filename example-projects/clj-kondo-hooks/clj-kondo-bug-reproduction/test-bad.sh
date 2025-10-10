#!/bin/bash
# Test with BAD clj-kondo commit (v2025.07.26)
# Expected: SCI context error

set -e

cd "$(dirname "$0")"
echo "=== Testing with BAD clj-kondo commit (v2025.07.26) ==="

# Expected output - the exact error message that should appear
read -r -d '' EXPECTED_OUTPUT <<'EOF' || true
reprod/test.clj:6:1: error: No context found in: sci.ctx-store/*ctx*. Please set it using sci.ctx-store/reset-ctx!
EOF

# Run clj-kondo and capture output
ACTUAL_OUTPUT=$(clojure -M:bad-clj-kondo --lint reprod/test.clj 2>&1 | grep -v "linting took" | grep -v "Downloading:" | grep -v "Checking out:" | grep -v "Cloning:" || true)

# Compare outputs - must match verbatim
if [ "$ACTUAL_OUTPUT" = "$EXPECTED_OUTPUT" ]; then
    echo "PASS: Output matches expected"
    echo "Actual output:"
    echo "$ACTUAL_OUTPUT"
    exit 0
else
    echo "FAIL: Output does not match expected"
    echo ""
    echo "Expected:"
    echo "$EXPECTED_OUTPUT"
    echo ""
    echo "Actual:"
    echo "$ACTUAL_OUTPUT"
    exit 1
fi
