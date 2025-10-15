#!/bin/bash

set -e

OLD_NAMESPACE=$1
NEW_NAMESPACE=$2

if [ -z $OLD_NAMESPACE ]; then 
  echo "Must provide old namespace"
  exit 1
fi

if [ -z $NEW_NAMESPACE ]; then 
  echo "Must provide old namespace"
  exit 1
fi

# 1. namespaced keywords/symbols
# 2. ending in space
# 3. ending in newline
# 4. ending in paren
# 5. ending in square bracket
git grep -l $OLD_NAMESPACE | xargs sed -i "s/${OLD_NAMESPACE}\//${NEW_NAMESPACE}\//g"
git grep -l $OLD_NAMESPACE | xargs sed -i "s/${OLD_NAMESPACE} /${NEW_NAMESPACE} /g"
git grep -l $OLD_NAMESPACE | xargs sed -i "s/${OLD_NAMESPACE}\$/${NEW_NAMESPACE}/g"
git grep -l $OLD_NAMESPACE | xargs sed -i "s/${OLD_NAMESPACE})/${NEW_NAMESPACE})/g"
git grep -l $OLD_NAMESPACE | xargs sed -i "s/${OLD_NAMESPACE}]/${NEW_NAMESPACE}]/g"
