#!/usr/bin/env python3


"""Converts dependency-based to span-based semantic role annotations.
"""

import collections
import sys
import util


Token = collections.namedtuple('Token', ('toknum', 'token', 'sem', 'lemma', 'head', 'role', 'sup', 'const', 'wordnet'))


if __name__ == '__main__':
    for block in util.blocks(sys.stdin):
        tokens = []
        for line in block.splitlines()[:-1]:
            toknum, token, sem, lemma, head, role, sup, const, wordnet = line.split('\t')
            tokens.append(Token(toknum, token, sem, lemma, head, role, sup, const, wordnet))
        pred_toknums = sorted(set(t.head for t in tokens if t.role != 'O'), key=int)
        frames = []
        for pred_toknum in pred_toknums:
            pred_index = int(pred_toknum) - 1
            pred_token = tokens[pred_index]
            frame = ['O'] * len(tokens)
            if pred_token.wordnet == 'O':
               frame[pred_index] = 'pred'
            else:
                frame[pred_index] = pred_token.wordnet
            for i, t in enumerate(tokens):
                if t.head == pred_toknum:
                    agenda = [i]
                    while agenda:
                        j = agenda.pop(0)
                        frame[j] = t.role
                        agenda.extend(k for k, u in enumerate(tokens) if u.head == tokens[j].toknum)
            frames.append(frame)
        for i, t in enumerate(tokens):
            fields = [t.toknum, t.token, t.sem, t.lemma, t.head, t.sup, t.const]
            fields.extend(f[i] for f in frames)
            print('\t'.join(fields))
        print()
