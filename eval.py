#!/usr/bin/env python3


import blocks
import sys


if __name__ == '__main__':
    try:
        _, gold, pred = sys.argv
    except ValueError:
        print('USAGE: python3 eval.py GOLD PREDICTED', file=sys.stderr)
        sys.exit(1)
    with open(gold) as f:
        gold_blocks = list(blocks.read(f))
    with open(pred) as f:
        pred_blocks = list(blocks.read(f))
    assert len(gold_blocks) == len(pred_blocks)
    total = 0
    correct = 0
    for gold_block, pred_block in zip(gold_blocks, pred_blocks):
        assert len(gold_block) == len(pred_block)
        for gold_line, pred_line in zip(gold_block, pred_block):
            total += 1
            if gold_line.split()[2] == pred_line.split()[2]:
                correct += 1
    print('UAS:', correct / total)
