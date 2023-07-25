#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)
cd $HERE

cd $HERE/release

# Build the main release image
sudo docker build --no-cache -t mctrace .
sudo docker save mctrace:latest | gzip > mctrace.tar.gz

# Build the tool-only image
sudo docker build - -f Dockerfile-for-Tool -t mctrace-tool
