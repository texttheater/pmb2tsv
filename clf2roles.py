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
    with open(clf_path) as clf_file, open(lemma_path) as lemma_file, open(semtag_path) as semtag_file:
        for words, fragments, symbols, semtags in clf.read_sentences(clf_file, lemma_file, semtag_file):
            # change representation of constant arguments
            fragments = constants.add_constant_clauses(symbols, fragments)
            fragments = constants.replace_constants(fragments)
            # map referents to the number of the token that introduces them
            ref_toknum_map = {c[2]: i for i, f in enumerate(fragments, start=1) for c in f if c[1] in ('REF', 'Name') or drs.is_constant(c[1])}
            # extract (verbal) events
            events = set(c[3] for f in fragments for c in f if c[2].startswith('"v.'))
            # find predicate and argument token numbers for each event
            pred_arg_role_map = collections.defaultdict(dict)
            for word, semtag, fragment in zip(words, semtags, fragments):
                if semtag in ('NOW', 'PST', 'FUT', 'PRG', 'PFT'):
                    continue
                for c in fragment:
                    if len(c) == 4 and c[2] in events and not drs.is_constant(c[3]):
                        pred_toknum = ref_toknum_map[c[2]]
                        arg_toknum = ref_toknum_map[c[3]]
                        role = c[1]
                        pred_arg_role_map[pred_toknum][arg_toknum] = role
            pred_toknums = tuple(pred_arg_role_map.keys())
            # output (one column per predicate)
            for toknum, word in enumerate(words, start=1):
                for i, pred_toknum in enumerate(pred_toknums, start=1):
                    if toknum == pred_toknum:
                        print('V', end='')
                    else:
                        print(pred_arg_role_map[pred_toknum].get(toknum, 'O'), end='')
                    if i < len(pred_toknums):
                        print('\t', end='')
                    else:
                        print()
            print()
