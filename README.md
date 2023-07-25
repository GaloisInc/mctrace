
Introduction
============

This repository contains the source code and build system for the
MCTrace binary instrumentation tool. The MCTrace tool enables users
to modify binaries, inserting instrumentation into them in order to
collect fine-grained tracing information. For information on the
MCTrace tool's design and usage, please see `MCTRACE.md`. This document
covers instructions for building MCTrace from source for development or
releases.

Building MCTrace
================

MCTrace can be built for one of two purposes: either for local
development in a Haskell build environment, or for release as a Docker
image. Instructions for each method are detailed below.

Development Build Instructions
------------------------------

The development environment setup and build processes are automated.
The build process requires Ubuntu 20.04. To perform a one-time setup of
the development environment including the installation of LLVM, cross
compilers, and other required tools, run the development setup script:

```
./dev_setup.sh
```

Once the development environment is set up and the required tools are
installed, MCTrace can be built with the build script:

```
./build.sh
```

After the build has completed, various cross compilers and other tools
can be brought into the `PATH` for easier access with:

```
. env.sh
```

To build the example test programs for `x86_64` and instrument them
using various testing probes, run:

```
make -C mctrace/tests/full
```

To do the same for PowerPC, run:

```
make -C mctrace/tests/full ARCH=PPC
```

The `mctrace` tool can be run manually by running:

```
cabal run mctrace <args>
```

For more details on using the `mctrace` tool, see `MCTRACE.md`.

Release Build Instructions
--------------------------

To build the release Docker image, execute the following from the root
of the repository:

```
cd release
./build.sh
```

This will build two docker images: 
- A self-contained image that contains MCTrace, its dependencies, associated tools, and examples. For information on using former image, please see `release/README.md`.

- A minimal image containing just MCTrace and its dependencies. A helper script, `release/mctrace` has been provided to run the command in a container. Note that paths passed to this script should be relative to the root of the repository and paths outside of the repository will not accessible.

Acknowledgements
================

This material is based upon work supported by the United States Air
Force AFRL/SBRK under Contract No. FA8649-21-P-0293.

(c) 2022-2023 Galois, Inc.
