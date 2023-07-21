
Introduction
============

The MCTrace tool enables users to insert instrumentation into binaries
in order to collect fine-grained tracing information. MCTrace functions
similarly to DTrace but does not require any operating support (or even
an operating system). The input format of MCTrace is a subset of the
DTrace probe script language.

Concept of Operations
=====================

Once the user has identified a program that they would like to
instrument (e.g., for debugging purposes or to collect telemetry), they
would:

1. Write a DTrace script that collects the desired data
2. Run the `mctrace` tool in `instrument` mode to insert probes into the
   binary as directed by the script
3. Run the instrumented binary
4. Run the `mctrace` tool in `extract` mode to interpret the resulting
   telemetry, converting it to JSON for further analysis

How MCTrace Works
=================

Using MCTrace requires:

* A PowerPC or `x86_64` ELF binary to instrument,
* An object file implementing the MCTrace Platform API (see below), and
* A Dtrace probe script containing the probes that will be used to
  modify the provided ELF binary.

MCTrace works by producing a modified version of its input binary that
calls Dtrace probes at the points described in the Dtrace probe script.

Some supported Dtrace language features need access to platform-specific
functionality (such as memory allocation and data exfiltration).
The MCTrace user provides that functionality to the tool in the
form of a compiled object file. The set of platform-specific
functions that MCTrace expects in the object file are called the
"Platform API." A complete implementation of the Platform API must
provide implementations of all of the functions the in header file
`examples/library/include/platform_api.h` provided in this distribution.
Once compiled, the platform API implementation must be provided to the
`mctrace` as the `--library` argument when invoking the `mctrace` tool.

For demonstration purposes, simple platform API implementations
for PowerPC and `x86_64` Linux user space are available at
`examples/library/PPC` and `examples/library/X86`, respectively, in both
source and object code forms.

Example
-------

In the following example, the user wants to instrument `foo.exe` to
collect telemetry. Assume that they want to count the number of times
the program reads from a file. Their probe script would look like:

```
int read_calls;
::read:entry {
  read_calls = read_calls + 1;
}
```

This probe increments the `read_calls` variable every time the `read`
system call is entered. Note that all probe variables are zero
initialized. The user would invoke `mctrace` as follows:

```
# Instrument the binary
$ mctrace instrument --binary=foo.exe \
                     --output=foo.instrumented.exe \
                     --var-mapping=foo.mapping.json \
                     --persistence-file=/tmp/foo.telemetry.bin \
                     --script=probes.d

# Run the binary
$ ./foo.instrumented.exe

# Interpret the collected telemetry
$ mctrace extract --var-mapping=foo.mapping.json \
                  --persistence-file=/tmp/foo.telemetry.bin

> {"read_calls": 1}
```

- The `foo.exe` binary and the `probes.d` probe script are provided to
  `mctrace` as inputs.
- The `output` option tells `mctrace` the name of the binary to produce
- The `persistence-file` option tells `mctrace` where the instrumented
  binary should save its collected telemetry when it runs (note that any
  existing files at that location will be overwritten)
- The `var-mapping` option tells `mctrace` where to record metadata that
  allows it to later interpret the collected telemetry

The `extract` mode of the tool uses the mapping file to interpret the
persisted telemetry. In this example, the program only called `read`
once.

Design
======

MCTrace uses DTrace scripts as input. The DTrace scripting language is
a simple imperative language inspired by C that is based on the concept
of *probes*. Users define probes, which are sequences of code that run
at instrumentation points. The set of instrumentation points are known
as *probe providers*; different operating systems (or in the case of
MCTrace, probe compilers) support different providers.

MCTrace compiles DTrace probe scripts into native code using a compiler
backend (e.g., LLLVM). It then uses binary rewriting to insert the
generated probes into the binary. Through static analysis, it identifies
program locations corresponding to DTrace probe providers; at each
provider site, it inserts calls to the compiled probes.

The storage backend for telemetry data will ultimately be configurable
to support a wide range of systems. At the moment, telemetry is stored
in a memory mapped region backed by a file on disk. The region is
allocated at program startup. Each probe modifies values in that region.
When the program exits, the telemetry information persists in the file.
Users can then collect this telemetry information for offline analysis.
When instrumenting a binary with a set of probes, `mctrace` can emit a
mapping file that describes the location of each probe variable in the
collected telemetry.

Supported Dtrace API
====================

MCTrace uses the Dtrace language as the means for expressing how it
should modify its input binary. While MCTrace does not implement all of
the Dtrace language, some core Dtrace language features are supported:

* Probe pattern matching
* Probe descriptions
* Global variables
* `timestamp`, `ucaller` and `arg0` builtins

Example Dtrace probe scripts demonstrating MCTrace's features can be
found in `examples/eval/` in this distribution.

Features and Limitations of MCTrace
===================================

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

Acknowledgements
================

This material is based upon work supported by the United States Air
Force AFRL/SBRK under Contract No. FA8649-21-P-0293.

(c) 2022-2023 Galois, Inc.
