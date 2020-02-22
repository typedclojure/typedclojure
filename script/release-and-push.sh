#!/bin/sh

set -e

if [[ `git symbolic-ref --short HEAD` != 'master' ]]; then
  echo "Must release on the master branch only."
  exit 1
fi

mvn release:prepare release:perform
git push tc --tags
