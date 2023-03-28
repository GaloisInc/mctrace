#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
. env.sh

# Build the x86 runtime with the host compiler
cd $HERE/mctrace/tests/library/X86
musl-gcc -o runtime.o -c runtime.c

# Build the powerpc runtime with the cross compiler built by
# dev_setup.sh
cd $HERE/mctrace/tests/library/PPC
powerpc-linux-muslsf-gcc -o runtime.o -c runtime.c

# Build mctrace itself
cd $HERE
cabal configure pkg:mctrace
cabal build :pkg:mctrace
