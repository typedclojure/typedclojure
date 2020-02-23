#!/bin/bash

set -e

TYPED_VERSION=$(mvn org.apache.maven.plugins:maven-help-plugin:2.1.1:evaluate -Dexpression=project.version)

if [[ "$TYPED_VERSION" == "*-SNAPSHOT" ]]; then
  echo "Deploying snapshot"
  mvn deploy
else
  echo "Not deploying snapshot, version does not end with '-SNAPSHOT': ${TYPED_VERSION}"
  exit 1
fi
