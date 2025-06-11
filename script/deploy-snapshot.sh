#!/bin/bash

set -ex

if [[ "$GITHUB_ACTIONS" == 'true' && "$GITHUB_REPOSITORY" != 'typedclojure/typedclojure' ]]; then
  echo "Can only deploy snapshot in GitHub Actions from 'typedclojure/typed', in: ${GITHUB_REPOSITORY}"
  exit 0
fi

if [[ `git symbolic-ref --short HEAD` != 'main' ]]; then
  echo "Snapshots only deployed on the main branch. Doing nothing."
  exit 0
fi

echo "Deploying snapshot"
clojure -T:build deploy-snapshot
