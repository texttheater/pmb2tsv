#!/usr/bin/env python3


import blocks
import re
import sys


if __name__ == '__main__':
    start_pattern = re.compile(r'^ccg\(\d+,')
    next_number = 1
    for block in blocks.read(sys.stdin):
        print(start_pattern.sub('ccg({},'.format(next_number), '\n'.join(block) + '\n\n', 1), end='')
        next_number += 1
