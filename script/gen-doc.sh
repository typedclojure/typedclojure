#!/usr/bin/env bash

set -e

mkdir -p target/codox

for d in typed/* ; do
  echo "$d"
  if [ -d "$d" ]; then
    cd $d
    #./script/gen-doc.sh
    cd ../..
    cp -r "$d/target/codox" "target/codox/typed.$(basename $d)"
  fi
done
