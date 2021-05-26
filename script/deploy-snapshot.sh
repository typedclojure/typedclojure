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

TYPED_VERSION=$(mvn -q \
  -Dexec.executable="echo" \
  -Dexec.args='${project.version}' \
  --non-recursive \
  org.codehaus.mojo:exec-maven-plugin:1.6.0:exec)

if [[ "$TYPED_VERSION" == *-SNAPSHOT ]]; then
  echo "Deploying snapshot"
  mvn deploy
else
  echo "Not deploying snapshot, version does not end with '-SNAPSHOT': ${TYPED_VERSION}"
  exit 1
fi
