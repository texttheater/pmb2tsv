from __future__ import annotations


import argparse
import contextlib
import itertools
import sys


from enum import Enum
from typing import TextIO, Sequence, List, Dict


_zip = zip # so we can define our own zip method and still use the builtin


def read(f: TextIO=sys.stdin) -> Sequence[List[str]]:
    """Splits a text stream by empty lines.

    Reads a file-like object and returns its contents chopped into a sequence
    of blocks terminated by empty lines.
    """
    block = []
    for line in f:
        (line,) = line.splitlines()
        if line == '':
            yield block
            block = []
        else:
            block.append(line)
    if block: # in case last block not terminated by empty line
        yield block


class HandleEmpty(Enum):
    MISMATCH = 'mismatch'
    DROP = 'drop'
    EMPTY = 'empty'
    IGNORE = 'ignore'


class CountMismatch(Exception):
    def __init__(self, blocks: Dict[str, Tuple[str]]):
        Exception.__init__(self, 'different number of blocks')
        self.blocks = blocks


class LengthMismatch(Exception):
    def __init__(self, blocks: Dict[str, Tuple[str]]):
        Exception.__init__(self, 'blocks have different lengths')
        self.blocks = blocks


def zip(*files: TextIO, empty: HandleEmpty=HandleEmpty.MISMATCH) -> Sequence[Tuple[List[str]]]:
    """Read blocks from multiple files in parallel.

    All files must have the same number of blocks, otherwise CountMismatch is
    raised.

    All corresponding blocks must have the same number of lines, otherwise
    LengthMismatch is raised. As an exception, some blocks can be empty (have
    0 lines) while other corresponding ones are not, if the keyword argument
    empty is set to something other than the default HandleEmpty.MISMATCH.
    If set to HandleEmpty.DROP, all tuples of blocks that have an empty block
    in them are dropped from the output. If set to HandleEmpty.EMPTY, empty
    blocks are replaced by blocks of N empty lines ('') to match the length of
    corresponding blocks. If set to HandleEmpty.IGNORE, empty blocks are
    removed from the output tuples, making such tuples shorter.
    """
    blockss = (read(f) for f in files)
    for i, blocks in enumerate(itertools.zip_longest(*blockss, fillvalue=None), start=1):
        if None in blocks:
            raise CountMismatch({f.name: b for f, b in _zip(files, blocks)})
        lengths = set(len(b) for b in blocks)
        if empty == HandleEmpty.DROP and len(lengths) <= 2 and 0 in lengths:
            continue
        if len(lengths) == 2 and 0 in lengths:
            if empty == HandleEmpty.EMPTY:
                lengths -= set((0,))
                (length,) = lengths
                blocks = tuple([''] * length if len(b) == 0 else b for b in blocks)
            elif empty == HandleEmpty.IGNORE:
                lengths -= set((0,))
                blocks = tuple(b for b in blocks if len(b) > 0)
        if len(lengths) > 1:
            raise LengthMismatch({f'block #{i} in {f.name}': b for f, b in _zip(files, blocks)})
        yield blocks


def report(block: List[Str], name: str):
    print(f'==> {name} ({len(block)} lines) <==', file=sys.stderr)
    for line in block:
        print(line, file=sys.stderr)
    print(file=sys.stderr)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='''Utility for handling files
            organized as blocks, i.e., sequences of lines separated by emtpy
            lines.''')
    subparsers = parser.add_subparsers(title='subcommands', dest='subcommand',
            description='valid subcommands')
    parser_paste = subparsers.add_parser('paste',
            help='paste corresponding blocks from multiple files horizontally')
    parser_paste.add_argument('files', nargs='*', type=argparse.FileType('r'))
    parser_paste.add_argument('-e', '--empty', type=HandleEmpty,
            default=HandleEmpty.MISMATCH, help='''How to handle empty blocks.
            mismatch: empty blocks are not allowed unless all corresponding
            blocks are empty. drop: drop all corresponding blocks if one or
            more of them are empty. empty: fill empty blocks with empty
            strings. ignore: do not create any column at all for empty blocks
            (this may lead of columns not lining up across blocks).''')
    args = parser.parse_args()
    if args.subcommand == 'paste':
        files = []
        if (not sys.stdin.isatty()) or len(args.files) == 0:
            files = [sys.stdin,] + args.files
        else:
            files = args.files
        try:
            for blocks in sys.modules[__name__].zip(*files, empty=args.empty):
                for fields in _zip(*blocks):
                    print('\t'.join(fields))
                print()
        except CountMismatch as e:
            print('ERROR: mismatching number of blocks. First set of excess '
                    'blocks follows.', file=sys.stderr)
            print(file=sys.stderr)
            for name, block in e.blocks.items():
                if block is not None:
                    report(block, name)
            sys.exit(1)
        except LengthMismatch as e:
            print('ERROR: block length mismatch. Mismatching blocks follow.',
                    file=sys.stderr)
            print(file=sys.stderr)
            for name, block in e.blocks.items():
                report(block, name)
            sys.exit(1)
    else:
        parser.print_help()
