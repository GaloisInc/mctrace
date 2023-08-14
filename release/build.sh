#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)
cd $HERE

# Build the main release image
sudo docker build --no-cache -t mctrace .
sudo docker save mctrace:latest | gzip > mctrace.tar.gz

# Build the tool-only image
sudo docker build - -t mctrace-tool < Dockerfile-for-Tool
sudo docker save mctrace-tool:latest | gzip > mctrace-tool.tar.gz
