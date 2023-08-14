#!/usr/bin/env bash

set -e
: ${TARGET_MCTRACE_LIB?"Missing TARGET_MCTRACE_LIB"}
HERE=$(cd `dirname $0`; pwd)

MCTRACE=mctrace
if ! which $MCTRACE
then
    MCTRACE=$(find $HERE/../../dist-newstyle -name mctrace -type f)
fi

cd $HERE
rm -f mpc5777c_dev_c10.elf.inst_*

# instrument console_println 
$MCTRACE instrument \
    --binary=mpc5777c_dev_c10.elf \
    --output=mpc5777c_dev_c10.elf.inst_console_println \
    --library=${TARGET_MCTRACE_LIB}/PPC/platform_impl.o \
    --var-mapping=program_c_mapping.json \
    --script=mpc5777c_console_println.d \
    --text-section .text_booke

# instrument can_read and can_write
$MCTRACE instrument \
    --binary=mpc5777c_dev_c10.elf \
    --output=mpc5777c_dev_c10.elf.inst_can \
    --library=${TARGET_MCTRACE_LIB}/PPC/platform_impl.o \
    --var-mapping=program_c_mapping.json \
    --script=mpc5777c_can.d \
    --text-section .text_booke
