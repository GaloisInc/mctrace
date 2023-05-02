#!/usr/bin/env bash

# This script will download the source for coreutils, patch it
# appropriately and build a statically-linked version for the each of
# the architectures that we support and install the resulting binaries
# in architecture-specific directories.

# The script is expected to run after `dev_setup.sh` and expects that
# the required cross compilers are installed at the locations specified
# by the respective architecture-specific compiler variables at the
# bottom of this script.

set -e

HERE=$(cd `dirname $0`; pwd)

LOG=$(readlink -m "$HERE/../install-coreutils.log")

# The coreutils source that we are going to build
COREUTILS_URL=https://github.com/coreutils/coreutils.git
COREUTILS_REF=5891d28edebe229f1a6057275e281b10c1f2247b

# Requires: LOG set to log file path.
function logged {
    if ! [ -z "$LOG" ]
    then
        mkdir -p `dirname $LOG`
        echo $* >>$LOG
        if ! $* >>$LOG 2>&1
        then
            echo
            echo "An error occurred; please see $LOG"
            echo "Here are the last 50 lines:"
            tail -n 50 $LOG
            exit 1
        fi
    else
        $*
    fi
}

function notice {
    echo "[NOTICE] $*"
}

function install_coreutils {
    local arch_name=$1
    local compiler=$2
    local install_path=$3

    # Check for compiler
    if [[ ! -x "$compiler" ]]; then
        echo "Error: unable to find compiler at $compiler"
        echo "Did you run dev_setup.sh?"
        exit 1
    fi

    notice "Found $arch_name compiler at $compiler"

    # We will work in a temporary directory
    WORKING_DIR=$(mktemp -d)

    function remove_temp {
        rm -rf $WORKING_DIR
    }

    trap remove_temp EXIT

    cd $WORKING_DIR

    notice "Fetching coreutils source"

    # Clone the repo, check out a fixed repo
    logged git clone "$COREUTILS_URL"

    cd coreutils
    logged git checkout "$COREUTILS_REF"

    # NOTE: Our patch is really a single change. So we just use sed to
    # do it. If anything more complex is needed at some point, switch to
    # a patch file
    sed -ri 's/(.*gl_WARN_ADD.*WERROR_CFLAGS.*)/\#\1/i' configure.ac

    notice "Building coreutils for $arch_name"

    # Bootstrap to create a configure file
    logged ./bootstrap

    # Configure
    CC="$compiler" CFLAGS='-static' LDFLAGS='-static' logged ./configure --prefix="$install_path"

    # Create the installation directory
    mkdir -p $install_path

    # Make and install
    logged make
    logged make install

    notice "coreutils for $arch_name installed at $install_path"
}

PPC_COMPILER=$(readlink -f "$HERE/../musl-gcc/output/bin/powerpc-linux-muslsf-gcc")
PPC_PATH=$(readlink -m "$HERE/../coreutils/ppc")

ARM_COMPILER=$(readlink -f "$HERE/../musl-gcc-arm/output/bin/arm-linux-musleabi-gcc")
ARM_PATH=$(readlink -m "$HERE/../coreutils/arm32")

notice "Logging build output to $LOG"

install_coreutils "PowerPC" $PPC_COMPILER $PPC_PATH
install_coreutils "arm32" $ARM_COMPILER $ARM_PATH
