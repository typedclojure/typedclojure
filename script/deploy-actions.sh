#! /usr/bin/env bash
# Deploys either a snapshot or release in GitHub Actions.
# If the commit message is of the form:
#   [typedclojure-release] <release-version> <new-dev-version>

set -ex

if [[ "$GITHUB_ACTIONS" != 'true' ]]; then
  echo "Only call this script in GitHub Actions"
  exit 1
fi 

if [[ "$GITHUB_REPOSITORY" != 'typedclojure/typedclojure' ]]; then
  echo "This script only deploys in typedclojure/typedclojure, not $GITHUB_REPOSITORY. Doing nothing."
  exit 1
fi

COMMIT_MSG=$(git log --format=%B -n 1 "${GITHUB_SHA}")
echo $COMMIT_MSG
if [[ "$COMMIT_MSG" =~ ^\[typedclojure-release\] ]]; then
  #https://stackoverflow.com/a/9294015
  COMMIT_MSG_ARRAY=($COMMIT_MSG)
  ./script/release-and-push.sh "${COMMIT_MSG_ARRAY[1]}" "${COMMIT_MSG_ARRAY[2]}"
  ./script/build-and-upload-docs.sh
else
  ./script/deploy-snapshot.sh
  # temporary
  ./script/build-and-upload-docs.sh
fi
