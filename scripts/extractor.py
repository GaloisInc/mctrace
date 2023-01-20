#!/usr/bin/env python

import argparse
import json
import os
import sys
import subprocess
import tempfile


parser = argparse.ArgumentParser(
    description = 'Helper script to interpret streamed data from instrumented binaries'
)
parser.add_argument("var_mapping")
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

is_first = True
while True:
    bb = sys.stdin.buffer.read(total_bytes)
    if len(bb) == 0:
        break
    
    if len(bb) < total_bytes:
        text = f"Expected {total_bytes} bytes, got {len(bb)}" 
        raise Exception(text)

    tmp = tempfile.NamedTemporaryFile(delete=False)
    tmp.write(bb)
    tmp.flush()
    
    if args.extract:
        subprocess.check_call(
            ["cabal", "exec", "mctrace", "--", "extract",
            f"--var-mapping={args.var_mapping}",
            f"--persistence-file={tmp.name}"
            ]
        )
    elif args.columns:
        res = subprocess.check_output(
            ["cabal", "exec", "mctrace", "--", "extract",
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
