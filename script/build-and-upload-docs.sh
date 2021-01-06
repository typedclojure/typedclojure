#!/usr/bin/env bash
# Builds and uploads API docs to provided bucket.
#
# Usage:
#  AWS_BUCKET="..." ./script/build-and-upload-docs.sh

set -e

if [[ -z "$AWS_BUCKET" ]]; then
  echo "Must provide AWS Bucket"
  exit 1
fi 

echo "Building API documentation..."

rm -r target/codox

./script/gen-doc.sh

echo "Uploading API documentation..."

aws s3 sync target/codox "s3://${AWS_BUCKET}/latest"
