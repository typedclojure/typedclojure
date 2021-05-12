#!/usr/bin/env bash

set -e

mkdir -p target/codox

for d in typed/* ; do
  if [ -d "$d" ]; then
    echo "$d"
    cd $d
    ./script/gen-doc.sh
    cd ../..
    cp -r "$d/target/codox" "target/codox/typed.$(basename $d)"
  fi
done
