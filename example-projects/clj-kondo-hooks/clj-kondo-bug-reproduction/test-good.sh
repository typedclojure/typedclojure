#!/bin/bash

# Test with GOOD clj-kondo commit (v2025.06.05 - 341364fe5befe7e5908c085a6bf80a9042134c15)
# Expected: Only one error about :expected-error keyword

cd "$(dirname "$0")"

echo "=== Testing with GOOD clj-kondo commit (v2025.06.05) ==="
echo "Commit: 341364fe5befe7e5908c085a6bf80a9042134c15"
echo ""

# Copy the clj-kondo config
rm -rf .clj-kondo/org.typedclojure
mkdir -p .clj-kondo/org.typedclojure/typed.clj.runtime
cp -r ../../../typed/clj.runtime/resources/clj-kondo.exports/org.typedclojure/typed.clj.runtime/* .clj-kondo/org.typedclojure/typed.clj.runtime/

# Run clj-kondo with the good commit
clojure -M:good-clj-kondo --lint src --config '{:output {:format :text :summary false}}'
