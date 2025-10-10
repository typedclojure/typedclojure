#!/bin/bash
# Test with GOOD clj-kondo commit (v2025.06.05)
# Expected: No errors (macro hook expands successfully)

cd "$(dirname "$0")"
echo "=== Testing with GOOD clj-kondo commit (v2025.06.05) ==="
clojure -M:good-clj-kondo --lint reproduction.clj
