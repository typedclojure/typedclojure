#!/bin/bash

set -ex
SUBMODULE_ARRAY=($SUBMODULE)
# space separated list of submodules
# https://stackoverflow.com/a/30212526
read -ra SUBMODULE_ARRAY<<<"$SUBMODULE"
if [[ ${#SUBMODULE_ARRAY[@]} == 0 ]]; then
  echo "Empty submodule array!"
  exit 1
fi
if [[ -z $CLOJURE_VERSION ]]; then
  echo 'Must set $CLOJURE_VERSION'
  exit 1
fi
# threads seem to race to create this if it doesn't exist
mkdir -p ~/.config/clojure
echo "{}" > ~/.config/clojure/deps.edn
download_deps (){
  ./script/test -P -Sdeps '{:deps {org.clojure/clojure {:mvn/version "'$CLOJURE_VERSION'"}}}'
}
export -f download_deps
run_tests (){
  ./script/test -Sdeps '{:deps {org.clojure/clojure {:mvn/version "'$CLOJURE_VERSION'"}}}'
}
export -f run_tests
if [[ "${SKIP_DOWNLOAD}" != "true" ]]; then
  parallel --halt now,fail=1 -j 1 'cd {} && download_deps' ::: "${SUBMODULE_ARRAY[@]}"
fi
parallel --halt now,fail=1 'cd {} && run_tests' ::: "${SUBMODULE_ARRAY[@]}"
