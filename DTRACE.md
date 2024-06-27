
Supported DTrace Language Features
==================================

MCTrace uses a subset of the DTrace language to allow users to describe
how input binaries should be instrumented. The following DTrace language
features are supported:

* Probe descriptions and probe syntax
* Probe name pattern-matching. Supported metacharacters are `*`, `?`,
  `[...]`, and `\`.
* Global variables
* Constants with suffixes `u`, `U`, `l`, `L`, `ul`, `UL`, `ll`, `LL`,
  `ull`, and `ULL`
* Data types:
  * `int`
  * `char`
  * `short`
  * `long`
  * `long long`
* Arithmetic operators `+`, `-`, and `*` supported on integer types only
* Builtins:
  * `int arg0`
  * `timestamp`
  * `long ucaller`

Example DTrace probe scripts demonstrating MCTrace's features can be
found in `mctrace/tests/eval/` in the MCTrace repository as well as in
the `examples` directory in the release Docker image.

MCTrace-specific DTrace extensions
----------------------------------

In addition to the language features listed above, MCTrace's support for
DTrace includes the following additional features that are specific to
MCTrace's version of DTrace:

* A `send(int channel_id)` action for telemetry exfiltration (see
  `MCTRACE.md` for details)
* A `copyint32(int addr)` subroutine for copying 32-bit values from
  memory
