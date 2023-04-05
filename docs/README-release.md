# Introduction

The MCTrace tool enables instrumentation of binaries in order to
collect fine-grained tracing information. It provides functionality
similar to what is provided by DTrace but does not require native
operating system support. The input language of MCTrace is the DTrace
language, but it currently supports a subset of the probes, actions,
and language features supported by DTrace.

# Features of This Demonstration

This demonstration includes example PowerPC binaries that run in
Linux user space. Future work will involve supporting "bare-metal"
PowerPC programs; that work is ongoing and will be featurd in a future
demonstration.

The binaries included in this demonstration are statically linked. At
this time, MCTrace supports only statically-linked input binaries.

This demonstration includes example Dtrace probes that use the built-in
Dtrace `timestamp` variable. Future work will include support for
`ucaller`, a `copy` subroutine, and a `send` action for writing data to
a platform-specific location.

At this time, functions provided within the Platform API implementation
object code cannot call each other. Each function must be
self-contained. Future work will address this limitation.

# Setting up MCtrace

A self-contained version of MCTrace is included in this folder as a
docker image. To load and run the image:

```
docker image load -i mctrace.tar
docker run -it -w /mctrace-test mctrace-local
```

This should leave you in a bash shell in the directory `/mctrace-test`.

# The MCTrace Docker Image

The current version of MCTrace is capable of instrumenting x86-64
binaries. A port to PowerPC is currently in progress and is expected to
be available soon.

The docker image contains a few x86-64 test programs and example
probes that can be used to exercise MCTrace. In particular the folder
`/mctrace-test/examples/eval` contains a collection of probes primarily
derived from those provided to us by WebSensing (the probes have been
modified slightly after discussions with WebSensing to fit the currently
supported DTrace syntax in MCTrace), `/mctrace-test/examples/full`
contains sources and binaries for a few test programs. We have also
included a few binaries from a statically compiled version of GNU
coreutils in `/mctrace-test/examples/extras`.

MCTrace has been architected to allow users to inject a collection of
support functions in to the binary (along with the probes). These
support functions can be invoked by the probes and allow it to access
platform-specific facilities (e.g., memory allocation, timestamps) and
support context-specific data ex-filtration (e.g. publishing data on a
CAN bus). In the general case, these support functions are provided by
the users of MCTrace, for testing purposes, a simple library of support
functions is available at `/mctrace-test/examples/library/X86` and
includes both sources and binaries.

# Using MCTrace

To instrument a binary with a probe, execute:

    mctrace instrument --binary=/mctrace-test/examples/full/read-write-syscall \
       --output=/tmp/read-write-syscall.instrumented \
       --library=/mctrace-test/examples/library/X86/runtime.o \
       --var-mapping=/tmp/read-write-syscall.mapping.json \
       --script=/mctrace-test/examples/eval/multiple-probe.d

-   The `--binary` and the `--script` options tell mctrace to instrument
    the specified binary with the given probe script
-   The `--output` option specifies the name for the instrumented binary
-   The `--library` option specifies the path to the library of support
    functions
-   The `--var-mapping` option tells `mctrace` where to record metadata
    that allows it to later interpret the collected telemetry

The above command instruments the binary with probes that triggers at
the start of a `read` or `write` function and counts the number of
invocations. Note that the instrumentation command produces a
significant amount of DEBUG logs, that can be ignored at the moment.

In addition, and for demonstration purposes, each probe when triggered
invokes the external support function `send` at the end of its
execution. The current test implementation of `send` pushes the set of
telemetry variables, in a compact binary format, to the standard error
(a more canonical implementation on an embedded device might push data
to a bus). A script `extractor.py` has been included with the image to
help interpret this data.

To invoke the instrumented binary and extract data:

    /tmp/read-write-syscall.instrumented 2>&1 >/dev/null | extractor.py /tmp/read-write-syscall.mapping.json --extract

This should produce the following output:

    {"read_count":1,"write_count":0}
    {"read_count":2,"write_count":0}
    {"read_count":3,"write_count":0}
    {"read_count":4,"write_count":0}
    {"read_count":4,"write_count":1}
    {"read_count":4,"write_count":2}
    {"read_count":4,"write_count":3}
    {"read_count":4,"write_count":4}
    {"read_count":4,"write_count":5}
    {"read_count":4,"write_count":6}
    {"read_count":4,"write_count":7}

-   Note that `2>&1 >/dev/null` has the effect of piping the standard
    error to the next command, while suppressing the standard output of
    the command. This is not required for a simple program like this,
    but is convenient when instrumenting programs that also produces
    standard output.

-   The `extractor.py` script offers a few other conveniences when
    extracting data from instrumented programs; for example it can
    produce columnar outputs and filter columns. `extractor.py --help`
    should detail these options.

-   Here are a few other binary / probe combinations to exercise and can
    be run similar to the one above:

    | Binaries                                   | Probe                                       |
    | ------------------------------------------ | ------------------------------------------- |
    | `examples/full/alloc-dealloc-fread-fwrite` | `examples/eval/fopen-calloc-fclose-probe.d` |
    | `examples/full/slow-read-write`            | `examples/eval/write-timing-probe.d`        |
    | `examples/full/read-write-syscall`         | `examples/eval/graph-probe.d`               |
    | `examples/extras/cat`                      | `examples/eval/multiple-probe.d`            |

    The first two probes above measure timing across different calls,
    while the last one instruments *all* functions in the binary and
    captures the program counter when triggered.
