#!/bin/bash

set -e

PUSHER_NAME=$1

if [[ -z "$PUSHER_NAME" ]]; then
  echo "Must provide pusher name"
  exit 1
fi

cd dev
clojure -M:check-ca -m typed.dev.check-ca $PUSHER_NAME
