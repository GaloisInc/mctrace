#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
. env.sh

# Build mctrace itself
cd $HERE
cabal build :pkg:mctrace --enable-tests
