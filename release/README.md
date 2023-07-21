
Running the Demonstration
=========================

This document is provided with a self-contained build of MCTrace as a
Docker image. To load and run the image, run the following commands:

```
docker image load -i mctrace.tar.gz
docker run -it -w /mctrace-test mctrace
```

This will drop you into a bash shell within the Docker container in the
directory `/mctrace-test` where you can use `mctrace` to instrument
binaries. We discuss the details of the `mctrace` tool in the following
sections. All relative paths mentioned in this document are relative to
`/mctrace-test`.

Docker Image Contents
---------------------

The docker image provided with this README contains PowerPC and `x86_64`
test programs and example probes that can be used to exercise MCTrace.
Important folders are as follows:

 * `examples/eval` contains a collection of probes primarily derived
   from those provided to us by WebSensing. The probes have been
   modified slightly after discussions with WebSensing to fit the
   currently supported DTrace syntax in MCTrace.
 * `examples/full` contains source code and binaries for bundled test
   programs.
 * `examples/binaries` contains binaries from a statically compiled
   version of GNU coreutils for use with `mctrace`.

Using MCTrace in this demonstration
-----------------------------------

The `mctrace` tool is in the shell `PATH` when running within Docker,
so no special steps are needed to be able to invoke the executable.
For instructions on how to run the `mctrace` tool with the appropriate
command-line arguments, see `MCTRACE.md` included in the Docker image.

As an example, the `read-write-syscall-PPC.4.inst` binary in this
distribution is the instrumented version of the PowerPC binary
`read-write-syscall-PPC` and was instrumented with the following
`mctrace` command:

```
mctrace instrument --binary=/mctrace-test/examples/full/read-write-syscall-PPC \
   --output=/mctrace-test/examples/full/read-write-syscall-PPC.4.inst \
   --library=/mctrace-test/examples/library/PPC/platform_impl.o \
   --var-mapping=/mctrace-test/examples/full/read-write-syscall-PPC.4.json \
   --script=/mctrace-test/examples/eval/write-timing-probe.d
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

As mentioned above, an explicit `send` action will be implemented in
a future version to allow probes to have more control over when data
should be exfiltrated and this implicit call to `send` will be retired.
The current test implementation of `send` pushes the set of telemetry
variables, in a compact binary format, to the standard error (a more
canonical implementation on an embedded device might push data to a
bus). A script `extractor.py` has been included with the image to help
interpret this data.

To invoke the instrumented binary and extract data:

    /mctrace-test/examples/full/read-write-syscall-PPC.4.inst 2>&1 >/dev/null | \
        extractor.py /mctrace-test/examples/full/read-write-syscall-PPC.4.json --extract --big-endian

This produces output similar to the following:

    {"write_count":1,"write_elapsed":162240,"write_ts":1681222607714740774}
    {"write_count":2,"write_elapsed":1740,"write_ts":1681222607714800756}
    {"write_count":3,"write_elapsed":1309,"write_ts":1681222607714803400}
    {"write_count":4,"write_elapsed":1344,"write_ts":1681222607714805742}
    {"write_count":5,"write_elapsed":1228,"write_ts":1681222607714807992}
    {"write_count":6,"write_elapsed":1222,"write_ts":1681222607714810214}
    {"write_count":7,"write_elapsed":1257,"write_ts":1681222607714812555}

- Note that `2>&1 >/dev/null` has the effect of piping the standard
  error to the next command while suppressing the standard output of the
  command. We do this because the provided platform API implementations
  writes `send()` data to `stderr` and we need that data to be piped to
  the extractor script.

- When extracting telemetry data from instrumented PowerPC binaries, the flag
  `--big-endian` must be passed to the extractor script as in the command above.
  The flag should be elided when working with `x86_64` binaries.

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
    | `examples/full/array-sum-PPC`                                                                      | `examples/eval/copy-probe.d`                |
    | `examples/binaries/PPC/cat` <br> `examples/binaries/X86/cat`                                       | `examples/eval/cat-probe.d`                 |
    | `examples/binaries/PPC/sha256sum` <br> `examples/binaries/X86/sha256sum`                           | `examples/eval/sha256sum-probe.d`           |

  The first two probes above measure timing across different calls,
  while the third one instruments *all* functions in the binary.
