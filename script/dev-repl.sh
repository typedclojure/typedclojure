#!/bin/bash

set -e

cd dev
clj -M:merge-deps
cd ../typed
clj -M:nREPL:spec-skip-macros "$@"
