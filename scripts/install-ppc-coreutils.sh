#!/usr/bin/env bash

# This script will download the sources for coreutils, patch it
# appropriately and build a static version for PPC 32-bit
# architecture and install the binaries in a specified location.

# The script is expected to run after `dev_setup.sh` and expects
# that the powerpc compiler is installed at the location specified
# there. Currently the compiler should be at:
# ../musl-gcc/output/bin/powerpc-linux-muslsf-gcc relative to this
# script.

set -e

HERE=$(cd `dirname $0`; pwd)
COMPILER=$(readlink -f "$HERE/../musl-gcc/output/bin/powerpc-linux-muslsf-gcc")
COREUTILS_URL=https://github.com/coreutils/coreutils.git
COREUTILS_REF=5891d28edebe229f1a6057275e281b10c1f2247b

# Check for arguments
if [[ -z "$1" ]]; then
    echo "Usage $0 <installation path>"
    echo
    echo "The installation path should be the path to a (potentially"
    echo "non-existent) directory that this script should create in"
    echo "which the compiler will be installed."
fi

INSTALL_ROOT=$(readlink -f "$1")

# Check for compiler
if [[ ! -x "$COMPILER" ]]; then
    echo "Unable to find powerpc compiler at $COMPILER"
fi

# We will work in a temporary directory
WORKING_DIR=$(mktemp -d)
cd $WORKING_DIR
echo "Working in $WORKING_DIR"

# Clone the repo, check out a fixed repo
git clone "$COREUTILS_URL"

cd coreutils
git checkout "$COREUTILS_REF"

# NOTE: Our patch is really a single change. So we just use sed to do it.
# If anything more complex is needed at some point, switch to a patch file
sed -ri 's/(.*gl_WARN_ADD.*WERROR_CFLAGS.*)/\#\1/i' configure.ac

# Bootstrap to create a configure file
./bootstrap

# Configure
CC="$COMPILER" CFLAGS='-static' LDFLAGS='-static' ./configure --prefix="$INSTALL_ROOT"

# Make and install
make && make install
