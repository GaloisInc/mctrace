
Supported DTrace API
====================

MCTrace uses the DTrace language as the means for expressing how it
should modify its input binary. While MCTrace does not implement all of
the DTrace language, some core DTrace language features are supported:

* Probe pattern matching
* Probe descriptions
* Global variables
* `timestamp`, `ucaller` and `arg0` builtins

Example DTrace probe scripts demonstrating MCTrace's features can be
found in `mctrace/tests/eval/` in the MCTrace GitHub repository.
