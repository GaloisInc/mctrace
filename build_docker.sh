#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)
cd $HERE

sudo docker build --no-cache -t mctrace .
sudo docker save mctrace:latest | gzip > mctrace.tar.gz
