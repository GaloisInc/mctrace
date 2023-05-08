This directory contains the `program_c` Challenge 10 source code built
as a Linux userspace PowerPC binary, statically linked, and instrumented
by Mctrace using the probe file contained in this directory. The
program is a modified verison of C hallenge 10 that reads CAN frames
directly from `program_c_read.bin` rather than using sockets, since this
program must be run in QEMU and QEMU has no support for the CAN socket
interface.
