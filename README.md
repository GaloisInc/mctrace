
Introduction
============

This repository contains the source code and build system for the
MCTrace binary instrumentation tool. The MCTrace tool enables users to
modify binaries, inserting instrumentation into them in order to collect
fine-grained tracing information. For information on the MCTrace
tool's design and usage, place see MCTRACE.md. This document covers
instructions for building MCTrace from source for development or
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

Release Build Instructions
--------------------------

To build the release Docker image, execute the following from the root
of the repository:

```
cd release
./build.sh
```

This will build a self-contained image that contains MCTrace, its
dependencies, associated tools, and examples. For information on using
the release image, please see `release/README.md`.

Testing the Tools
-----------------

To test the tools, first build a test binary:

```
cd mctrace/tests/full/ && make
```

An example probe is available in =mctrace/test/eval=. To instrument this
binary with the probe, from the same directory, execute:

```
mctrace instrument \
    --binary=read-syscall --output=/tmp/read-syscall.instrumented \
    --var-mapping=/tmp/read-syscall.mapping.json --persistence-file=/tmp/telemetry.bin \
    --script=../eval/single-add-probe.d
```

This produces the instrumented binary =/tmp/read-syscall.instrumented=
as well as a mapping file =/tmp/read-syscall.mapping.json=, which
we require later to extract telemetry information. Then run the
instrumented binary:

```
/tmp/read-syscall.instrumented
```

This creates the file =/tmp/telemetry.bin= that contains the telemetry
information in binary format. To interpret these results, execute:

```
mctrace extract \
    --var-mapping=/tmp/read-syscall.mapping.json \
    --persistence-file=/tmp/telemetry.bin
```

This should display the set of variables defined in the probes and their
values.

Other binaries can be instrumented, run and interpreted in a similar
fashion.

Acknowledgements
================

This material is based upon work supported by the United States Air
Force AFRL/SBRK under Contract No. FA8649-21-P-0293.

(c) 2022-2023 Galois, Inc.
