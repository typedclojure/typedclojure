#!/bin/bash
#
# Prompts for next release version and dev version and
# creates a commit that triggers GitHub Actions to deploy
# the release version and bump the repo to the next dev
# version.

echo "Determining current version..."

## via mvn
TYPED_VERSION=$(mvn -q \
  -Dexec.executable="echo" \
  -Dexec.args='${project.version}' \
  --non-recursive \
  org.codehaus.mojo:exec-maven-plugin:1.6.0:exec)

# via current-version file
#TYPED_VERSION=$(bb -e '(-> "current-version" slurp str/trim print)')

if [[ "$TYPED_VERSION" == *-SNAPSHOT ]]; then
  echo "Current version: ${TYPED_VERSION}"

  #https://stackoverflow.com/a/125340
  DEFAULT_RELEASE_VERSION="${TYPED_VERSION%-SNAPSHOT}"
  #https://stackoverflow.com/a/2642592 (bash 3 compatible)
  read -p "Choose release version [${DEFAULT_RELEASE_VERSION}]: " RELEASE_VERSION
  RELEASE_VERSION=${RELEASE_VERSION:-${DEFAULT_RELEASE_VERSION}}

  # default: increment patch
  DEFAULT_DEV_VERSION=$(printf "%s-SNAPSHOT" $(./script/increment-semversion.sh -p ${RELEASE_VERSION}))
  read -p "Choose next dev version [${DEFAULT_DEV_VERSION}]: " DEV_VERSION
  DEV_VERSION=${DEV_VERSION:-${DEFAULT_DEV_VERSION}}

  git commit --allow-empty -m "$(printf "[typedclojure-release] %s %s" "${RELEASE_VERSION}" "${DEV_VERSION}")"
else
  echo "Can only prep a release from a SNAPSHOT version, found $TYPED_VERSION"
  exit 1
fi
