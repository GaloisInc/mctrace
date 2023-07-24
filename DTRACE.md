
Supported DTrace Language Features
==================================

MCTrace uses the DTrace language as the means for expressing how it
should modify its input binary. While MCTrace does not implement all of
the DTrace language, the following core DTrace language features are
supported:

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
  * `arg0`
  * `timestamp`
  * `ucaller` (has a 64-bit type on both `x86_64` and PowerPC
    architectures)

Example DTrace probe scripts demonstrating MCTrace's features can be
found in `mctrace/tests/eval/` in the MCTrace GitHub repository.
