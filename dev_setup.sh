#!/usr/bin/env bash

# mctrace setup script
#
# Supported distribution(s):
#  * Ubuntu 20.04
#
# This script installs everything needed to get a working development
# environment for mctrace. Any new environment requirements should be
# added to this script.
#
# This script is designed to be safe to run from any directory, on any
# development environment, and in any intermediate state, i.e. if some
# steps have already been carried out, this script should gracefully
# find that out and continue. This helps keep this script useful as
# a way to incrementally ensure that we have captured environmental
# requirements, and it makes this script useful in CI, Docker, and
# ordinary development contexts. New steps added should also be
# implemented this way.

set -e

HERE=$(cd `dirname $0`; pwd)
LOG=$HERE/dev_setup.log

# Tool versions to use when installing ghcup for the first time.
GHC_VERSION=8.10.7
CABAL_VERSION=3.6.2.0

# The constraints file for cabal. This must match the version of GHC
# that we're using for development.
FREEZE_FILE=cabal.project.freeze.ghc-8.10.7

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
    fi
}

function notice {
    echo "[NOTICE] $*"
}

function in_path {
    local prog=$1
    which $1 >/dev/null 2>/dev/null
}

function install_system_packages {
    notice "Installing required system packages (requires sudo)"

    logged sudo apt-get install --yes \
        build-essential \
        musl-tools \
        llvm-12-dev \
        llvm-12-runtime \
        libgmp-dev \
        zlib1g-dev
}

function install_ghcup {
    if ! in_path ghcup
    then
        notice "Installing ghcup, GHC, and cabal (this may take a while)"

        script=$(mktemp)
        curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org > $script

        function remove_temp {
            rm $script
        }

        trap remove_temp EXIT

        BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
            BOOTSTRAP_HASKELL_GHC_VERSION=$GHC_VERSION \
            BOOTSTRAP_HASKELL_CABAL_VERSION=$CABAL_VERSION \
            BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
            BOOTSTRAP_HASKELL_ADJUST_BASHRC=1  \
            bash $HERE/vendor/bootstrap-haskell >>$LOG 2>>$LOG
    else
        notice "ghcup already installed."
    fi
}

function symlink_cabal_config {
    local freeze_dest=cabal.project.freeze
    local config_dest=cabal.project
    local config_src=cabal.project.dist

    cd $HERE
    if [ ! -e $freeze_dest ]
    then
        notice "Symlinking $FREEZE_FILE to $freeze_dest"
        ln -s $FREEZE_FILE $freeze_dest
    else
        notice "$freeze_dest already exists, skipping"
    fi

    if [ ! -e $config_dest ]
    then
        notice "Symlinking $config_src to $config_dest"
        ln -s $config_src $config_dest
    else
        notice "$config_dest already exists, skipping"
    fi
}

function update_submodules {
    cd $HERE
    notice "Updating submodules"
    logged git submodule update --init
}

notice "Starting setup, logging to $LOG."

install_system_packages
install_ghcup
symlink_cabal_config
update_submodules

notice "Done."
