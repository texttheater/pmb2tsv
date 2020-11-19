import sys


from typing import TextIO, Sequence, List


def blocks(f: TextIO) -> Sequence[List[str]]:
    """Splits a text stream by empty lines.

    Reads a file-like object and returns its contents chopped into a sequence
    of blocks terminated by empty lines.
    """
    block = []
    for line in f:
        block.append(line)
        (chomped,) = line.splitlines()
        if chomped == '':
            yield block
            block = []
    if block: # in case the last block is not terminated
        yield block
