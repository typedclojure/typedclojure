#!/bin/bash
#
# Cuts a release of typedclojure. Requires two arguments:
# - the release version to use
# - the development SNAPSHOT version to use
#
# eg., to cut version 1.0.0 and then move to 1.0.1-SNAPSHOT,
# call this script like so:
#
#   ./script/release-and-push.sh 1.0.0 1.0.1-SNAPSHOT
#
# and then push the resulting commit to the main branch.
# GitHub Actions will automatically deploy a 1.0.0 release and
# update the dev version to 1.0.1-SNAPSHOT.

set -e

if [[ "$GITHUB_ACTIONS" != 'true' ]]; then
  echo "Must release on GitHub Actions only."
  exit 1
fi

if [[ `git symbolic-ref --short HEAD` != 'main' ]]; then
  echo "Releases only triggered on the main branch. Doing nothing."
  exit 1
fi

if [[ "$GITHUB_ACTOR" != "frenchy64" ]]; then
  echo "Only maintainers may deploy a release. Doing nothing."
  exit 1
fi

if [[ "$GITHUB_REPOSITORY" != "typedclojure/typedclojure" ]]; then
  echo "Releases only allowed from typedclojure/typedclojure. Doing nothing."
  exit 1
fi

git config --local user.email "abonnairesergeant@gmail.com"
git config --local user.name "Ambrose Bonnaire-Sergeant"

RELEASE_VERSION=$1
DEVELOPMENT_VERSION=$2

if [[ -z "$RELEASE_VERSION" ]]; then
  echo "Must specify release version"
  exit 1
fi

if [[ -z "$DEVELOPMENT_VERSION" ]]; then
  echo "Must specify development version"
  exit 1
fi

# there's a chance this can fail and we have a partial
# release to Clojars. We minimize the damage by avoiding
# pushing back to main, but there's a chance the version
# was partially deployed. The correct fix (wrt clojars) is to simply
# deploy a new version (eg., if 1.0.0 fails, try 1.0.1 next).
( set -x;
mvn release:prepare release:perform \
  -DreleaseVersion="$RELEASE_VERSION" \
  -Dtag="$RELEASE_VERSION" \
  -DdevelopmentVersion="$DEVELOPMENT_VERSION"
  )

echo $RELEASE_VERSION > stable-version
echo "$(git rev-parse "$RELEASE_VERSION")" > stable-sha
echo $DEVELOPMENT_VERSION > current-version

./script/regen-selmer.sh
./script/update_release_notes.clj

git add .
git commit -m "Bump README versions for $RELEASE_VERSION"

# DON'T PRINT HERE
git push "https://${GITHUB_ACTOR}:${GITHUB_TOKEN}@github.com/${GITHUB_REPOSITORY}.git" main --tags
