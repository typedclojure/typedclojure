#!/bin/bash

set -e

cd dev
clj -M:merge-deps
cd ../typed
# work around problem reading typed-test.clj.common-tests, copied from tools.reader
clj -M:eastwood
