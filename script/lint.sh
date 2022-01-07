#!/bin/bash

set -e

cd dev
./src/typed/dev/merge_deps.clj
cd ..
# work around problem reading typed-test.clj.common-tests, copied from tools.reader
clj -M:test:eastwood
