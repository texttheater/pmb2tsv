#!/usr/bin/env python3


import blocks
import clf
import collections
import drs
import constants
import sys


def is_verb(fragment):
    return any(c[2].startswith('"v.') for c in fragment)


def spread(roletags, deps):
    """Spreads the role tag of a word to all of its dependents"""
    roletags = list(roletags)
    def sprd(i):
        for j in range(len(roletags)):
            if int(deps[j]) - 1 == i and roletags[j] == 'O':
                roletags[j] = roletags[i]
                sprd(j)
    for i, roletag in enumerate(roletags):
        if roletag not in ('V', 'O'):
            sprd(i)
    return tuple(roletags)


def remove_punctuation(roletags, words):
    roletags = list(roletags)
    def is_punctuation(word):
        return word == '.' # TODO more
    def get(lst, i):
        if i < 0 or i >= len(lst):
            return None
        return lst[i]
    for i in range(len(roletags)):
        if is_punctuation(words[i]) and (
                (get(roletags, i - 1) != roletags[i]) !=
                (roletags[i] != get(roletags, i + 1))):
            roletags[i] = 'O'
    return tuple(roletags)


if __name__ == '__main__':
    try:
        _, clf_path, lemma_path, semtag_path, dep_path = sys.argv
    except ValueError:
        print('USAGE: python3 clf2roles.py myfile.drs.clf myfile.lemma myfile.sem myfile.pmbdep', file=sys.stderr)
        sys.exit(1)
    with open(clf_path) as clf_file, open(lemma_path) as lemma_file, \
            open(semtag_path) as semtag_file, open(dep_path) as dep_file:
        for words, fragments, symbols, semtags, deps in clf.read_sentences(clf_file, lemma_file, semtag_file, dep_file):
            # change representation of constant arguments
            fragments = constants.add_constant_clauses(symbols, fragments)
            fragments = constants.replace_constants(fragments)
            # for each token, collect the set of referents that it either
            # introduces or introduces a concept or constant for
            refss = [
                # filter out auxiliary verbs
                () if s in ('NOW', 'PST', 'FUT', 'PRG', 'PFT') and len(f) < 5
                else set(
                    c[2]
                    for c in f
                    if (
                        c[1] == 'REF'
                        or drs.is_constant(c[1])
                    )
                    and not (c[2].startswith('t') and is_verb(f))
                ) | set(
                    c[3]
                    for c in f
                    if len(c) == 4
                    and drs.is_concept(c[1])
                    and not (c[3].startswith('t') and is_verb(f))
                )
                for f, s in zip(fragments, semtags)
            ]
            # create a list of all verbal events
            events = tuple(
                c[3]
                for f in fragments
                for c in f
                if c[2].startswith('"v.')
            )
            # map events to participants to roles
            pas = collections.defaultdict(dict)
            for f in fragments:
                for c in f:
                    if len(c) == 4 and c[2] in events:
                        pas[c[2]][c[3]] = c[1]
            # create role taglists for each event
            def roletag(e, refs):
                if e in refs:
                    return 'V'
                for x in refs:
                    if x in pas[e]:
                        return pas[e][x]
                return 'O'
            roletagss = tuple(
                tuple(
                    roletag(e, refs)
                    for refs in refss
                )
                for e in events
            )
            # spread roletags along dependency edges
            roletagss = tuple(spread(r, deps) for r in roletagss)
            # remove peripheral punctuation
            roletagss = tuple(remove_punctuation(r, words) for r in roletagss)
            # output (one column per event)
            if len(roletagss) > 0:
                for i in range(len(words)):
                    print('\t'.join(roletags[i] for roletags in roletagss))
            print()
