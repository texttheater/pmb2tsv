#!/usr/bin/env python3


# Reads 1 or more files containing blocks (sequences of lines separated by
# empty lines), checks that corresponding blocks have the same number of lines,
# and outputs them side-by-side. Blocks that are empty in one or more files are
# dropped.


import sys
import util


if __name__ == '__main__':
    files = [util.blocks(open(p)) for p in sys.argv[1:]]
    for blocks in zip(*files):
        blocks = [b.splitlines() for b in blocks]
        # Make sure the lenghts match:
        nonempty_blocks = [b for b in blocks if b != ['']]
        lengths = [len(b) for b in nonempty_blocks]
        lengths = set(lengths)
        if len(lengths - set((0,))) > 1:
            print('ERROR: block length mismatch. Mismatching blocks follow.', file=sys.stderr)
            print(file=sys.stderr)
            for path, block in zip(sys.argv[1:], blocks):
                print('==> {} ({} lines) <=='.format(path, len(block)), file=sys.stderr)
                for line in block:
                    print(line, file=sys.stderr)
            sys.exit(1)
        # Skip if one or more blocks are empty:
        if [''] in blocks:
            print()
            continue
        # Paste side-by-side:
        for lines in zip(*blocks):
            print('\t'.join(l.rstrip() for l in lines))
