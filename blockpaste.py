#!/usr/bin/env python3


import argparse
import itertools
import sys


def blocks(f):
    block = []
    for line in f:
        line = line.rstrip()
        if line == '':
            yield block
            block = []
        else:
            block.append(line)
    if block: # in case last block not terminated by empty line
        yield block


def report(block, name):
    print(f'==> {name} ({len(block)} lines <==', file=sys.stderr)
    for line in block:
        print(line, file=sys.stderr)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''Reads blocks (sequences of
            lines separated by empty lines) from STDIN and pastes them together
            with blocks read from FILE, separated by a tab character. Fails if
            STDIN and FILE have different numbers of blocks, or if two
            corresponding blocks have different numbers of lines.''')
    parser.add_argument('file', help='''the file to read the second set of
            blocks from''')
    parser.add_argument('-e', '--empty', choices=('fail', 'drop',
            'empty-column', 'ignore'), default='fail', help='''What to do if a
            block read from FILE is empty (0 lines) while the block read from
            STDIN is nonempty. fail: fail with an error, as with other
            mismatches. drop: drop both blocks (output an empty block).
            empty-column: add a tab character to every line, followed by
            nothing (so columns still line up across all blocks). ignore:
            add nothing, just copy the block from STDIN.''')
    args = parser.parse_args()
    with open(args.file) as f:
        for block1, block2 in itertools.zip_longest(blocks(sys.stdin),
                blocks(f), fillvalue=None):
            if block1 == None:
                print(f'''ERROR: <STDIN> has more blocks than {args.file}.
                        First excess block follows.''', file=sys.stderr)
                print(file=sys.stderr)
                report(block1, '<STDIN>')
                sys.exit(1)
            if block2 == None:
                print(f'''ERROR: {args.file} has more blocks than <STDIN>.
                        First excess block follows.''', file=sys.stderr)
                print(file=sys.stderr)
                report(block2, args.file)
                sys.exit(1)
            if len(block1) > 0 and len(block2) == 0:
                if args.empty == 'fail':
                    print(f'''ERROR: block length mismatch. Mismatching blocks
                            follow.''', file=sys.stderr)
                    print(file=sys.stderr)
                    report(block1, '<STDIN>')
                    report(block2, args.file)
                    sys.exit(1)
                elif args.empty == 'drop':
                    print()
                    continue
                elif args.empty == 'empty-column':
                    block2 = ('',) * len(block1)
                elif args.empty == 'ignore':
                    for line in block1:
                        print(line)
                    print()
                    continue
            if len(block1) != len(block2):
                print(f'''ERROR: block length mismatch. Mismatchin blocks
                        follow.''', file=sys.stderr)
                print(file=sys.stderr)
                report(block1, '<STDIN>')
                report(block2, args.file)
                sys.exit(1)
            for line1, line2 in zip(block1, block2):
                print(f'{line1}\t{line2}')
            print()
