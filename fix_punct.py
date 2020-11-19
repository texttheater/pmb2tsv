#!/usr/bin/env python3


import blocks
import sys


def fix_punct(lines):
    (root_number,) = [i for i, l in enumerate(lines, start=1) if l != '' and l.split()[2] == '0']
    if root_number == len(lines):
        return # don't attach punctuation to itself
    fields = lines[-1].split()
    if fields[1] in ('.', '?', '!'):
        fields[2] = str(root_number)
    lines[-1] = '\t'.join(fields)


if __name__ == '__main__':
    for block in blocks.read(sys.stdin):
        fix_punct(lines)
        print('\n'.join(lines))
        print()
