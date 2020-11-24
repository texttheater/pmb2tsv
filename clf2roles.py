#!/usr/bin/env python3


import blocks
import clf
import collections
import drs
import constants
import sys


if __name__ == '__main__':
    try:
        _, clf_path, lemma_path, semtag_path = sys.argv
    except ValueError:
        print('USAGE: python3 clf2roles.py myfile.drs.clf myfile.lemma myfile.sem', file=sys.stderr)
        sys.exit(1)
    with open(clf_path) as clf_file, open(lemma_path) as lemma_file, \
            open(semtag_path) as semtag_file:
        for words, fragments, symbols, semtags in clf.read_sentences(clf_file, lemma_file, semtag_file):
            # change representation of constant arguments
            fragments = constants.add_constant_clauses(symbols, fragments)
            fragments = constants.replace_constants(fragments)
            # map referents to the numbers of the tokens that introduce concepts or constants for them
            ref_toknums_map = collections.defaultdict(set)
            # map events to sets of participant-role pairs
            pas = collections.defaultdict(set)
            # set of all events
            events = set()
            # fill data structures
            for i, (fragment, semtag) in enumerate(zip(fragments, semtags), start=1):
                if semtag in ('NOW', 'PST', 'FUT', 'PRG', 'PFT'):
                    continue
                for c in fragment:
                    if len(c) == 4:
                        if drs.is_concept(c[1]):
                            ref_toknums_map[c[3]].add(i)
                            if c[2].startswith('"v.'):
                                events.add(c[3])
                        else:
                            pas[c[2]].add((c[3], c[1]))
                    elif len(c) == 3 and (drs.is_constant(c[1]) or c[1] == 'REF'):
                        ref_toknums_map[c[2]].add(i)
            events = tuple(events)
            # output (one column per event)
            if len(events) == 0:
                print()
                continue
            for toknum, word in enumerate(words, start=1):
                for e in events:
                    if toknum in ref_toknums_map[e]:
                        print('V', end='')
                    else:
                        token_role = 'O'
                        for x, role in pas[e]:
                            if toknum in ref_toknums_map[x]:
                                token_role = role
                                break
                        print(token_role, end='')
                    if toknum < len(words):
                        print('\t', end='')
                print()
            print()
