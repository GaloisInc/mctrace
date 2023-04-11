Introduction
============

The MCTrace tool enables instrumentation of binaries in order to
collect fine-grained tracing information. It provides functionality
similar to what is provided by DTrace but does not require native
operating system support. The input language of MCTrace is the DTrace
language, but it currently supports a subset of the probes, actions,
and language features supported by DTrace.

How MCTrace Works
=================

Using MCTrace requires:

* An PowerPC or `x86_64` ELF binary to instrument,
* An object file implementing the MCTrace Platform API (see below), and
* A Dtrace probe script containing the probes that will be used to
  modify the provided ELF binary.

MCTrace works by modifying the input binary to insert calls to Dtrace
probes at the points described in the Dtrace probe script. Some Dtrace
language features that MCTrace supports need access to platform-specific
functionality (such as memory allocation or data exfiltration). This
set of platform-specific functions that MCTrace needs are called the
"Platform API." To use MCTrace, an object file implementing the Platform
API must be provided. A complete implementation of the Platform API
must provide implementations of all of the functions the in header file
`examples/library/include/platform_api.h` provided in this distribution.
Once compiled, the platform API implementation can be provided to the
`mctrace` as the `--library` argument when invoking the `mctrace` tool.

For demonstration purposes, simple platform API implementations
for PowerPC and `x86_64` Linux user space are available in
the Docker image at `/mctrace-test/examples/library/PPC` and
`/mctrace-test/examples/library/X86`, respectively, and include both
source and object code.

Supported Dtrace API
--------------------

MCTrace uses the Dtrace language as the means for expressing how it
should modify its input binary. While MCTrace does not implement all of
the Dtrace language, some core Dtrace language features are supported:

* Probe pattern matching
* Probe descriptions
* Global variables
* The `timestamp` variable

Example Dtrace probe scripts demonstrating MCTrace's features can be
found in `examples/eval/` in this distribution.

Features and Limitations of This Demonstration
----------------------------------------------

In addition to our own example programs, this demonstration includes
a small collection of statically-linked PowerPC and `x86_64` binaries
from the `coreutils` distribution and a selection of probes that can be
used to instrument them. The current demonstration has the following
limitations:

 - At this time, MCTrace supports only statically-linked input binaries.
 - The binaries run in Linux userspace for their respective
   architectures. Future work will involve supporting "bare-metal"
   PowerPC programs.
 - The only DTrace built-in variable currently supported is `timestamp`.
   Future work will include support for `ucaller`, a `copy` subroutine,
   and an *explicit* send action for writing data to a platform-specific
   location.
 - Platform API implementations are subject to the following
   restrictions:
   - Functions in the Platform API implementation must be self-contained
     and cannot call other functions even in the same object file.
   - Functions cannot make use of global variables.

   We expect to relax part of these restrictions in a future version.

Running the Demonstration
=========================

This document is provided with a self-contained version of MCTrace as a
Docker image. To load and run the image, run the following commands:

```
docker image load -i mctrace.tar
docker run -it -w /mctrace-test mctrace
```

This will drop you into a bash shell within the Docker container in the
directory `/mctrace-test` where you can use `mctrace` to instrument
binaries. We discuss the details of the `mctrace` tool in the following
sections.

The MCTrace Docker Image
------------------------

The current version of MCTrace is capable of instrumenting PowerPC and
`x86_64` binaries.

The docker image provided with this README contains PowerPC and `x86_64`
test programs and example probes that can be used to exercise MCTrace.
Important folders are as follows:

 * `/mctrace-test/examples/eval` contains a collection of probes
   primarily derived from those provided to us by WebSensing. The probes
   have been modified slightly after discussions with WebSensing to fit
   the currently supported DTrace syntax in MCTrace.
 * `/mctrace-test/examples/full` contains source code and binaries for
   bundled test programs.
 * `/mctrace-test/examples/binaries` contains binaries from a statically
   compiled version of GNU coreutils for use with `mctrace`.

Using MCTrace
-------------

MCTrace is run as follows:

```
mctrace instrument
    --binary=<path to elf binary>
    --output=<output file path>
    --library=<platform API implementation object file>
    --var-mapping=<output variable mapping JSON file path>
    --script=<input file path to Dtrace probe script>
```

For example,

```
mctrace instrument
    --binary=my_program
    --output=my_program.instrumented
    --library=my_platform.o
    --var-mapping=mapping.json
    --script=my_dtrace_probes.d
```

For example, the `read-write-syscall-PPC.1.inst` binary in this
distribution is the instrumented version of the PowerPC build of
`read-write-syscall` and was instrumented with the following `mctrace`
command:

```
mctrace instrument --binary=/mctrace-test/examples/full/read-write-syscall \
   --output=/mctrace-test/examples/full/read-write-syscall-PPC.1.inst \
   --library=/mctrace-test/examples/library/PPC/platform_impl.o \
   --var-mapping=/mctrace-test/examples/full/read-write-syscall-PPC.1.json \
   --script=/mctrace-test/examples/eval/single-add-probe.d
```

- The `--binary` and the `--script` options tell mctrace to instrument
  the specified binary with the given probe script.
- The `--output` option specifies the name for the instrumented binary.
- The `--library` option specifies the path to the Platform API
  implementation.
- The `--var-mapping` option tells `mctrace` where to record metadata
  that allows it to later interpret the collected telemetry.

The above command instruments the binary with probes that triggers
at the start and end of the `write` function and computes timing
information for the call. Note that the instrumentation command produces
a significant amount of DEBUG logs, that can be ignored at the moment.

In addition, and for demonstration purposes, each probe when triggered
invokes the Platform API function `send` at the end of its execution.
As mentioned above, an explicit `send` action will be implemented in
a future version to allow probes to have more control over when data
should be exfiltrated and this implicit call to `send` will be retired.
The current test implementation of `send` pushes the set of telemetry
variables, in a compact binary format, to the standard error (a more
canonical implementation on an embedded device might push data to a
bus). A script `extractor.py` has been included with the image to help
interpret this data.

To invoke the instrumented binary and extract data:

    ./read-write-syscall-PPC.instrumented 2>&1 >/dev/null | extractor.py /mctrace-tests/examples/full/read-write-syscall-PPC.json --extract --big-endian

This produces output similar to the following:

    {"write_count":1,"write_elapsed":0,"write_ts":1681222607714558552}
    {"write_count":1,"write_elapsed":162240,"write_ts":1681222607714740774}
    {"write_count":2,"write_elapsed":0,"write_ts":1681222607714798744}
    {"write_count":2,"write_elapsed":1740,"write_ts":1681222607714800756}
    {"write_count":3,"write_elapsed":0,"write_ts":1681222607714801836}
    {"write_count":3,"write_elapsed":1309,"write_ts":1681222607714803400}
    {"write_count":4,"write_elapsed":0,"write_ts":1681222607714804181}
    {"write_count":4,"write_elapsed":1344,"write_ts":1681222607714805742}
    {"write_count":5,"write_elapsed":0,"write_ts":1681222607714806536}
    {"write_count":5,"write_elapsed":1228,"write_ts":1681222607714807992}
    {"write_count":6,"write_elapsed":0,"write_ts":1681222607714808768}
    {"write_count":6,"write_elapsed":1222,"write_ts":1681222607714810214}
    {"write_count":7,"write_elapsed":0,"write_ts":1681222607714811076}
    {"write_count":7,"write_elapsed":1257,"write_ts":1681222607714812555}

- Note that `2>&1 >/dev/null` has the effect of piping the standard
  error to the next command while suppressing the standard output of the
  command. We do this because the provided platform API implementations
  writes `send()` data to `stderr` and we need that data to be piped to
  the extractor script.

- The `extractor.py` script offers a few other conveniences when
  extracting data from instrumented programs; for example it can produce
  columnar outputs and filter columns. See `extractor.py --help` for
  details on these options.

- The table below lists a few other binaries for PowerPC and `x86_64` as
  well some example probes that can be used to instrument each binary.
  Note that many other combinations of example programs and probes
  can work together; the full list of combinations can be found in
  `examples/full/Makefile`.

    | Binaries                                                                                           | Probe                                       |
    | ---------------------------------------------------------------------------------------------------| ------------------------------------------- |
    | `examples/full/alloc-dealloc-fread-fwrite-PPC` <br> `examples/full/alloc-dealloc-fread-fwrite-X86` | `examples/eval/fopen-calloc-fclose-probe.d` |
    | `examples/full/slow-read-write-PPC` <br> `examples/full/slow-read-write-X86`                       | `examples/eval/write-timing-probe.d`        |
    | `examples/full/read-write-syscall-PPC` <br> `examples/full/read-write-syscall-X86`                 | `examples/eval/graph-probe.d`               |
    | `examples/binaries/PPC/cat`                                                                        | `examples/eval/cat-probe.d`                 |
    | `examples/binaries/PPC/sha256sum`                                                                  | `examples/eval/sha256sum-probe.d`           |

  The first two probes above measure timing across different calls,
  while the third one instruments *all* functions in the binary.
