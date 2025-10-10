#!/bin/bash
# Test with BAD clj-kondo commit (v2025.07.26)
# Expected: SCI context error

cd "$(dirname "$0")"
echo "=== Testing with BAD clj-kondo commit (v2025.07.26) ==="
clojure -M:bad-clj-kondo --lint reproduction.clj
