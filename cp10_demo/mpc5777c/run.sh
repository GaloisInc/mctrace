#!/usr/bin/env bash

set -e
: ${MCTRACE_ROOT:=/mctrace-test}
HERE=$(cd `dirname $0`; pwd)

cd $HERE
rm -f mpc5777c_dev_c10.elf.inst_*

# instrument console_println 
mctrace instrument \
    --binary=mpc5777c_dev_c10.elf \
    --output=mpc5777c_dev_c10.elf.inst_console_println \
    --library=${MCTRACE_ROOT}/mctrace/tests/library/PPC/platform_impl.o \
    --var-mapping=program_c_mapping.json \
    --script=mpc5777c_console_println.d \
    --text-section .text_booke

# instrument can_read and can_write
mctrace instrument \
    --binary=mpc5777c_dev_c10.elf \
    --output=mpc5777c_dev_c10.elf.inst_can \
    --library=${MCTRACE_ROOT}/mctrace/tests/library/PPC/platform_impl.o \
    --var-mapping=program_c_mapping.json \
    --script=mpc5777c_can.d \
    --text-section .text_booke