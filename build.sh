#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
. env.sh

cd $HERE/mctrace/tests/library/X86
musl-gcc -o runtime.o -c runtime.c

cd $HERE/mctrace/tests/library/PPC
powerpc-linux-muslsf-gcc -o runtime.o -c runtime.c

cd $HERE/mctrace/tests/full
make

cd $HERE
cabal configure pkg:mctrace
cabal build :pkg:mctrace
