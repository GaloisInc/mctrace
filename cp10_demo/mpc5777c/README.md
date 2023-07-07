This directory contains the `mpc5777c_dev_c10.elf` bare-metal binary
with two example binaries that have been instrumented by Mctrace using
different probes.
  * `mpc5777c_dev_c10.elf.inst_console_println`: emits a timestamp 
  and elapsed time for all `console_println` calls
  * `mpc5777c_dev_c10.elf.inst_can`: emits a timestamp 
  and elapsed time for all `can_read` and `can_write` calls

These can be rebuilt using the `./run.sh` script from the
`mctrace` docker shell. i.e:
```
docker run -it -w /mctrace-test mctrace
cd /mctrace-test/cp10_demo/mpc5777c
./run.sh
```