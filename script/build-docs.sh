#!/usr/bin/env bash
# Builds API docs into target/codox.

set -e

echo "Building API documentation..."

if [[ -d "target/codox" ]]; then
  rm -r target/codox
fi

./script/gen-doc.sh
