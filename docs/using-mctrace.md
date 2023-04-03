Using Mctrace
=============

This document provides information how to use the Mctrace tool to
instrument a binary with Dtrace-style probes.

Using Mctrace requires:

* An `x86_64` or PowerPC ELF binary to instrument,
* An object file implementing the Mctrace Platform API (see below), and
* A Dtrace probe script containing the probes that will be used to
  modify the provided ELF binary.

Mctrace is run as follows:

```
mctrace instrument
    --binary=<path to elf binary>
    --output=<output file path>
    --library=<platform API implementation object file>
    --var-mapping=<output variable mapping JSON file path>
    --persistence-file=<output file path for persisted data>
    --script=<input file path to Dtrace probe script>
```

For example,

```
mctrace instrument
    --binary=my_program
    --output=my_program.instrumented
    --library=my_platform.o
    --var-mapping=mapping.json
    --persistence-file=persistence.bin
    --script=my_dtrace_probes.d
```

The Platform API
----------------

The Platform API is the API for the platform-specific features that must
be implemented for the environment in which an instrumented binary will
be run. A binary can be instrumented only once a complete Platform API
implementation is provided as object code. A complete implementation of
the Platform API must provide implementations of all of the functions
the in file `include/platform_api.h` provided in this distribution.
the Once compiled, the platform API implementation can be provided to
the `mctrace` as the `--library` argument above.

Supported Dtrace API
--------------------

Mctrace uses the Dtrace language as the means for expressing how it
should modify its input binary. While Mctrace does not implement all of
the Dtrace language, some core Dtrace language features are supported:

* Probe pattern matching
* Probe descriptions
* Global variables
* The `timestamp` variable
* The `ucaller` variable

In addition to core Dtrace language features, Mctrace supports the
following Dtrace actions specific to Mctrace:

* `send(uint64_t value)`: an abstract platform-specific value
  exfiltration action. Mctrace translates calls to this action into
  calls to the platform API's implementation of the `send()` function.

Example Dtrace probe scripts demonstrating Mctrace's features can be
found in `dtrace-examples/` in this distribution.