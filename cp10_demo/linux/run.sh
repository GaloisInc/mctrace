#!/usr/bin/env bash

set -e

HERE=$(cd `dirname $0`; pwd)

cd $HERE
rm -f program_c_write.bin
./cp10_program_c_ppc32.mctrace 2>telemetry.bin
../../scripts/extractor.py program_c_mapping.json --big-endian --columns < telemetry.bin
