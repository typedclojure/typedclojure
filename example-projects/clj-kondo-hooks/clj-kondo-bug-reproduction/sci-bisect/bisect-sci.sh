#!/bin/bash

set -e

SCI_SHA="$1"

if [ -z "$SCI_SHA" ]; then
  echo "Usage: $0 <sci-commit-sha>"
  exit 1
fi

# Update deps.edn with the SCI SHA
sed "s/TO_BE_REPLACED/$SCI_SHA/" deps.edn > deps-test.edn

# Run clj-kondo with this SCI version
cd ..
export CLJ_KONDO_TEST_OUTPUT=$(clojure -Sdeps "$(cat sci-bisect/deps-test.edn)" -M:test-sci --lint reprod/test.clj --config .clj-kondo 2>&1 || true)

# Check if the SCI context error appears
if echo "$CLJ_KONDO_TEST_OUTPUT" | grep -q "No context found in: sci.ctx-store"; then
  echo "BAD: SCI context error found with $SCI_SHA"
  exit 1
else
  echo "GOOD: No SCI context error with $SCI_SHA"
  exit 0
fi
