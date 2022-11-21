#!/usr/bin/env python3


import json
import sys


import blocks


CONSTANTS = ('"speaker"', '"hearer"', '"now"', '"+"', '"-"', '"?"')


if __name__ == '__main__':
    for block in blocks.read():
        for line in block:
            toknum, tok, lemma, wordnet, dep, frag = line.split(maxsplit=5)
            frag = json.loads(frag)
            for clause in frag:
                # abstract senses
                if len(clause) == 4 and clause[1] == lemma:
                    wordnet = f'{lemma}.{clause[2][1:-1]}'
                    clause[1] = '$SYMBOL'
                    clause[2] = '$SENSE'
                # abstract strings
                if clause[-1].startswith('"') \
                        and clause[-1].endswith('"') \
                        and clause[-1] not in CONSTANTS:
                    clause[-1] = '$STRING'
                # TODO generalize DR indices
            frag = json.dumps(frag)
            line = '\t'.join((toknum, tok, lemma, wordnet, dep, frag))
            print(line)
        print()
