
Introduction
============

The MCTrace tool enables users to insert instrumentation into binaries
in order to collect fine-grained tracing information. MCTrace functions
similarly to DTrace but does not require any operating support (or even
an operating system). The input format of MCTrace is a subset of the
DTrace probe script language. Prior knowledge of DTrace concepts and
terminology is assumed in this document.

Concept of Operations
=====================

After identifying a program to be instrumented (e.g., for debugging
purposes or to collect telemetry), the steps to use MCTrace are as
follows:

1. Write a DTrace script that collects the desired data. (See
   `DTRACE.md` for details about supported DTrace language features.)
2. Run the `mctrace` tool in `instrument` mode to insert probes into the
   binary as directed by the script.
3. Run the instrumented binary.
4. Collect telemetry data emitted by the instrumented binary and use
   `mctrace` and associated scripts to decode it.

The following sections provide more details about the steps listed
above.

How MCTrace Works
=================

Using MCTrace requires:

* A PowerPC or `x86_64` ELF binary to instrument,
* An object file implementing the MCTrace Platform API (see below), and
* A DTrace probe script containing the probes that will be used to
  modify the provided ELF binary.

MCTrace works by producing a modified version of its input binary that
calls Dtrace probes at the points described in the DTrace probe script.
MCTrace compiles DTrace probe scripts into native code using a compiler
backend (e.g., LLLVM). It then uses binary rewriting to insert the
generated probes into the binary. Through static analysis, it identifies
program locations corresponding to DTrace probe providers; at each
provider site, it inserts calls to the compiled probes.

Some supported DTrace language features need access to platform-specific
functionality such as memory allocation. Since the DTrace code
will run within the context of the modified binary rather than an
operating system kernel, MCTrace requires some additional code to
provide access to such platform-specific features. The MCTrace tool
provides the input program with access to this platform-specific
functionality by way of an object file of compiled code called the
Platform Implementation. The object code that implements the required
functions must conform to a set of C function prototypes called the
Platform API. A complete implementation of the Platform API must
provide implementations of all of the functions the in header file
`mctrace/tests/library/include/platform_api.h` provided in the MCTrace
GitHub repository. Once compiled, the platform API implementation must
be provided to the `mctrace` as the `--library` argument when invoking
the `mctrace` tool.

For demonstration purposes, simple platform API implementations for
PowerPC and `x86_64` Linux user space are available in the repository
in `mctrace/tests/library/` and in the Docker release image in
`examples/library/` in both source and object code forms.

Example
-------

The following example demonstrates how an `x86_64` binary `foo` would be
instrumented and how its produced telemetry would be obtained. In this
example, the user wants to count the number of times that the program
performs a file read by measuring the number of calls to the `read`
system call. The probe script to accomplish this is as follows:

```
int read_calls;

::read:entry {
  read_calls = read_calls + 1;
  send(0);
}
```

This probe increments the `read_calls` DTrace variable just prior to
each call to the `read` system call. Futhermore, the complete set of
DTrace global variables (containing only `read_calls` in this case) is
then transmitted as telemetry according to the platform implementation
of `send()`.

Once the probes are written, `mctrace` would be invoked as follows.

- The `foo` binary and the `probes.d` probe script are provided to
  `mctrace` as inputs.
- The `--output` option tells `mctrace` where the instrumented binary
  should be written.
- The `--var-mapping` option tells `mctrace` where to record metadata
  that allows it to later decode telemetry data.
- The `--library` option tells `mctrace` where to find the platform
  implementation object code.
- The `--var-mapping` option tells `mctrace` where to write its metadata
  describing the encoding of the telemetry data stream.

```
$ mctrace instrument --binary=foo \
                     --output=foo.instrumented \
                     --var-mapping=foo.mapping.json \
                     --library=mctrace/tests/library/platform_impl.o \
                     --script=probes.d
```

The resulting binary can then be run:

```
$ ./foo.instrumented 2>telemetry.bin
```

Once the instrumented binary has finished running, the file
`telemetry.bin` will contain the binary telemetry data corresponding
to the value of `read_calls` after each invocation of the probe. The
telemetry data must be decoded:

```
$ extractor.py foo.mapping.json --columns < telemetry.bin
```

The `extractor.py` script uses the telemetry mapping file to decode the
written telemetry data.

NOTE: In this example, the instrumented binary was run with a `stderr`
redirect. This is because the `x86_64` platform implementation for
`send()` used in this example writes the DTrace global variable
data to `stderr`. In practice, `send()` would trigger some other
platform-specific mode of data transmission such as sending packets on a
network connection, writing to a local bus, etc.

For full details of the `mctrace` tool's command line usage, run
`mctrace instrument --help`.

Current Limitations of MCTrace
==============================

MCTrace has the following limitations:

 - MCTrace supports only statically-linked input binaries.
 - In addition to the core DTrace language features listed above,
   MCTrace also supports some additional DTrace constructs specific to
   MCTrace:
   - A `send` action of the form: `send(<numeric channel ID>)`. Invoking
     `send` will result in an invocation of the `platform_send` function
     in the Platform API with that channel ID and the global data store.
     See the MCTrace Telemetry section below for details.
   - A `copyint32(<address>)` subroutine that returns the 32-bit value
     from the specified location.
 - In the current implementation, `arg0` always returns a 32-bit value
   (on both PowerPC and `x86-64` platforms). Similarly, `copyint32`
   takes a 32-bit address as its input on both platforms (and returns
   a 32-bit value). As a result, these work best on the PowerPC 32-bit
   platform since the argument and return value width match the
   architecture.
 - Platform API implementations are subject to the following
   restrictions:
   - Functions in the Platform API implementation must be self-contained
     and cannot call other functions even in the same object file.
   - Functions cannot make use of global variables.
