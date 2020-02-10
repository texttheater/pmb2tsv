#!/usr/bin/env python3


import re
import sys
import util


if __name__ == '__main__':
    start_pattern = re.compile(r'^ccg\(\d+,')
    next_number = 1
    for block in util.blocks(sys.stdin):
        print(start_pattern.sub('ccg({},'.format(next_number), block, 1), end='')
        next_number += 1