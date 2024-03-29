#!/usr/bin/env python3

import argparse
import json
import os
import shlex
import shutil
import sys
import subprocess
import tempfile


parser = argparse.ArgumentParser(
    description='Helper script to interpret streamed data from instrumented binaries'
)
parser.add_argument("var_mapping")
parser.add_argument('--big-endian', action='store_true', default=False, help="Process the results as big-endian values")
group = parser.add_mutually_exclusive_group()
group.add_argument('--hex', help="Display the raw bytes as hex", action='store_true')
group.add_argument('--extract', help="Display the results after passing it to mctrace extract command", action='store_true')
group.add_argument('--columns', nargs='?', const=1, help="Display the results in a columnar format. For displaying specific columns, pass a comma separated list")
args = parser.parse_args()

total_bytes = 0
mapping = {}
with open(args.var_mapping) as f:
    mapping = json.load(f)

    # NOTE: Currently MCTrace does not pack its data, every
    # field is stretched out to 64 bits / 8 bytes.
    # FIXME: Change this when that changes
    total_bytes = len(mapping) * 8

# Determine cabal command. If an environment variable MCTRACE_COMMAND
# has been set, use that as a command. Otherwise if the "mctrace"
# exectuable is already on path, we use that, otherwise we will fall
# back to cabal
if cmd := os.getenv('MCTRACE_COMMAND'):
    mctrace_command = shlex.split(cmd)
elif loc := shutil.which('mctrace'):
    mctrace_command = [loc, "--"]
else:
    mctrace_command = ["cabal", "exec", "mctrace", "--"]

is_first = True
while True:
    bb = sys.stdin.buffer.read(total_bytes)
    if len(bb) == 0:
        break

    if len(bb) < total_bytes:
        text = f"Expected {total_bytes} bytes, got {len(bb)}"
        raise Exception(text)

    if args.big_endian:
        # Divide in to 4 byte chunks
        chunked = [iter(bb)] * 8
        chunked_stream = zip(*chunked)

        # Reorder bytes to little-endian depending on whether the
        # values are 32 or 64 bit (heuristically speaking)
        reordered_bytes = []
        for (a, b, c, d, e, f, g, h) in chunked_stream:
            if e == 0 and f == 0 and g == 0 and h == 0:  # 32 bit value
                reordered_bytes.extend([d, c, b, a, e, f, g, h])
            else:  # 64 bit value
                reordered_bytes.extend([h, g, f, e, d, c, b, a])

        bb = bytes(reordered_bytes)

    tmp = tempfile.NamedTemporaryFile(delete=False)
    tmp.write(bb)
    tmp.flush()

    if args.extract:
        subprocess.check_call(
            mctrace_command + [
                "extract",
                f"--var-mapping={args.var_mapping}",
                f"--persistence-file={tmp.name}"
            ]
        )
    elif args.columns:
        res = subprocess.check_output(
            mctrace_command + [
                "extract",
                f"--var-mapping={args.var_mapping}",
                f"--persistence-file={tmp.name}"
            ]
        )

        dct = json.loads(res)

        columns = args.columns.split(',') if args.columns != 1 else list(dct.keys())
        header_format_string = "|" + "{:<21}|" * len(columns)
        if is_first:
            line_parts = ['-' * 21] * len(columns)
            print(header_format_string.format(*line_parts))
            print(header_format_string.format(*columns))
            print(header_format_string.format(*line_parts))

        format_string = "| " + "{:<20}| " * len(columns)
        values = [dct[c] for c in columns]
        print(format_string.format(*values))
    else:
        subprocess.check_call(["hexdump", tmp.name])

    is_first = False
    os.unlink(tmp.name)
