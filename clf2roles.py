#!/usr/bin/env python3


import blocks
import clf
import collections
import drs
import constants
import sys
import util


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
        return word in ('.', ',') # TODO more
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


def dedup(roletags):
    """Keep only one span per role, the one closest to the predicate."""
    role_spans_map = collections.defaultdict(list)
    for role, span in util.groupby_ranges(roletags):
        role_spans_map[role].append(span)
    roletags=list(roletags)
    for role, spans in role_spans_map.items():
        pred_span = role_spans_map['V'][0]
        keep_span_index, _ = util.minindex(spans,
                key=lambda s: abs(util.rangedist(s, pred_span)))
        for i, span in enumerate(spans):
            if i != keep_span_index:
                for j in span:
                    roletags[j] = 'O'
    return tuple(roletags)


if __name__ == '__main__':
    try:
        _, clf_path, lemma_path, semtag_path, dep_path = sys.argv
    except ValueError:
        print('USAGE: python3 clf2roles.py myfile.drs.clf myfile.lemma myfile.sem myfile.pmbdep', file=sys.stderr)
        sys.exit(1)
    with open(clf_path) as clf_file, open(lemma_path) as lemma_file, \
            open(semtag_path) as semtag_file, open(dep_path) as dep_file:
        try:
            for words, fragments, symbols, semtags, deps in clf.read_sentences(clf_file, lemma_file, semtag_file, dep_file):
                # change representation of constant arguments
                fragments = constants.add_constant_clauses(symbols, fragments)
                fragments = constants.replace_constants(fragments)
                # for each token, collect the set of referents that it either
                # introduces or introduces a concept or constant for
                refss = [
                    # filter out auxiliary verbs
                    set() if s in ('NOW', 'PST', 'FUT', 'PRG', 'PFT') and l in ('be', 'have', 'do', 'will', 'use', 'let')
                    else set(
                        c[2]
                        for c in f
                        if (
                            c[1] == 'REF'
                            or drs.is_constant(c[1])
                        )
                        and not (c[2].startswith('t') and s.startswith('E'))
                    ) | set(
                        c[3]
                        for c in f
                        if len(c) == 4
                        and drs.is_concept(c[1])
                        and not (c[3].startswith('t') and s.startswith('E'))
                    )
                    for f, l, s in zip(fragments, symbols, semtags)
                ]
                # create a list of all verbal events
                events = tuple(
                    c[3]
                    for f, s in zip(fragments, semtags)
                    if s.startswith('E')
                    for c in f
                    if c[2].startswith('"v.')
                )
                # map boxes to propositions they introduce
                box_prop_map = {
                    c[0]: c[3]
                    for f in fragments
                    for c in f
                    if len(c) == 4
                    and c[1:3] == ('proposition', '"n.01"')
                }
                # map boxes to the first event they introduce
                box_event_map = {}
                for f in fragments:
                    for c in f:
                        if len(c) == 3 and c[1] == 'REF' and c[2].startswith('e') and c[0] not in box_event_map:
                            box_event_map[c[0]] = c[2]
                # map propositions to events
                prop_event_map = {}
                for f in fragments:
                    for c in f:
                        if len(c) == 3 and c[1] == 'ATTRIBUTION' and c[0] in box_prop_map and c[2] in box_event_map:
                            prop_event_map[box_prop_map[c[0]]] = box_event_map[c[2]]
                # map events to participants to roles
                pas = collections.defaultdict(dict)
                for f in fragments:
                    for c in f:
                        if len(c) == 4 and c[2] in events:
                            participant = c[3]
                            if participant in prop_event_map:
                                participant = prop_event_map[participant]
                            pas[c[2]][participant] = c[1]
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
                # remove role taglists without predicates
                for roletags in roletagss:
                    if 'V' not in roletags:
                        print(
                            'WARNING: predicate not marked, skipping',
                            file=sys.stderr,
                        )
                roletagss = tuple(t for t in roletagss if 'V' in t)
                # spread roletags along dependency edges
                if '' in deps:
                    print(
                        'WARNING: missing dependencies, skipping',
                        file=sys.stderr
                    )
                    continue
                roletagss = tuple(spread(r, deps) for r in roletagss)
                # remove duplicate tags
                roletagss = tuple(dedup(r) for r in roletagss)
                # remove peripheral punctuation
                roletagss = tuple(remove_punctuation(r, words) for r in roletagss)
                # output (one column per event)
                if len(roletagss) > 0:
                    for i in range(len(words)):
                        print('\t'.join(roletags[i] for roletags in roletagss))
                print()
        except blocks.CountMismatch as e:
            e.report()
            sys.exit(1)
        except blocks.LengthMismatch as e:
            e.report()
            sys.exit(1)

