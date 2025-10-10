#!/bin/bash

# Test with BAD clj-kondo commit (v2025.07.26 - b77888454e0695905f225b2e88f6d3b1dd75259a)
# Expected: SCI context errors in addition to the :expected-error

cd "$(dirname "$0")"

echo "=== Testing with BAD clj-kondo commit (v2025.07.26) ==="
echo "Commit: b77888454e0695905f225b2e88f6d3b1dd75259a"
echo ""

# Copy the clj-kondo config
rm -rf .clj-kondo/org.typedclojure
mkdir -p .clj-kondo/org.typedclojure/typed.clj.runtime
cp -r ../../../typed/clj.runtime/resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/* .clj-kondo/org.typedclojure/typed.clj.runtime/

# Run clj-kondo with the bad commit
clojure -M:bad-clj-kondo --lint src --config '{:output {:format :text :summary false}}'
