#!/usr/bin/env python3


import json
import sys


import blocks


CONSTANTS = ('"speaker"', '"hearer"', '"now"', '"+"', '"-"', '"?"')


def relativize(arg, head, heads, frags):
    typ = arg[0]
    if typ not in 'benpstx':
        return arg
    try:
        num = int(arg[1:])
    except ValueError:
        return arg
    seen = set()
    distance = 0
    while head != 0:
        frag = frags[head - 1]
        for clause in reversed(frag):
            for other_arg in reversed(clause):
                if other_arg[0] == typ and other_arg not in seen:
                    seen.add(other_arg)
                    distance += 1
                    if other_arg == arg:
                        return f'{typ}^{distance}'
        head = heads[head - 1]
    return arg


def normalize(frag):
    mapping = {}
    def normalize_(arg):
        if arg in mapping:
            return mapping[arg]
        typ = arg[0]
        if typ not in 'benpstx':
            return arg
        try:
            num = int(arg[1:])
        except ValueError:
            return arg
        count = 1
        for key in mapping:
            if key[0] == typ:
                count += 1
        new_arg = typ + str(count)
        mapping[arg] = new_arg
        return new_arg
    return tuple(tuple(normalize_(a) for a in c) for c in frag)


if __name__ == '__main__':
    for block in blocks.read():
        toknums, toks, lemmas, senses, heads, frags = \
                zip(*(l.split(maxsplit=5) for l in block))
        heads = tuple(int(h) for h in heads)
        frags = tuple(json.loads(f) for f in frags)
        for toknum, tok, lemma, sense, head, frag in \
                zip(toknums, toks, lemmas, senses, heads, frags):
            new_frag = []
            for clause in frag:
                # abstract senses
                if len(clause) == 4 and clause[1] == lemma:
                    sense = f'{lemma}.{clause[2][1:-1]}'
                    clause[1] = '$SYMBOL'
                    clause[2] = '$SENSE'
                # abstract strings
                if clause[-1].startswith('"') \
                        and clause[-1].endswith('"') \
                        and clause[-1] not in CONSTANTS:
                    clause[-1] = '$STRING'
                # relativize DR indices
                new_frag.append(tuple(relativize(a, head, heads, frags) for a in clause))
            frag = normalize(new_frag)
            frag = json.dumps(frag)
            print(toknum, tok, lemma, sense, head, frag, sep='\t')
        print()
