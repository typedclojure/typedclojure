#!/bin/bash

set -e

cd dev
bb -m typed.dev.new-module "$@"
cd ..
./script/regen-selmer.sh
