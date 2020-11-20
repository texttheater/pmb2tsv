"""Functions for handling clauses.

As in the PMB CLF format.
"""


import blocks
import collections
import re
import sys


from typing import List, NewType, Sequence, TextIO, Tuple


Clause = NewType('Clause', Tuple)
DRS = NewType('DRS', Tuple[Clause])


SEP_PATTERN = re.compile(r' *% ?')
TOK_PATTERN = re.compile(r'([^ ]+ \[(?P<fr>\d+)\.\.\.(?P<to>\d+)\])')


def token_sortkey(token):
    match = TOK_PATTERN.match(token)
    return int(match.group('fr'))


def read(flo: TextIO) -> Sequence[Tuple[Tuple[str], Tuple[DRS]]]:
    for block in blocks.read(flo):
        assert block[0].startswith('%%% ')
        assert block[1].startswith('%%% ')
        assert block[2].startswith('%%% ')
        sentence = tuple(t for t in block[2].rstrip()[4:].split(' ') if t != 'ø')
        token_fragment_map = collections.defaultdict(list)
        for line in block[3:]:
            clause, tokens = SEP_PATTERN.split(line, 1)
            clause = tuple(clause.split(' '))
            if clause == ('',):
                clause = ()
            else:
                assert 3 <= len(clause) <= 4
            tokens = TOK_PATTERN.findall(tokens)
            for token in tokens:
                token_fragment_map[token[0]].append(clause)
                if not clause:
                    token_fragment_map[token[0]].pop(-1)
        fragment_list = tuple(tuple(token_fragment_map[k])
            for k in sorted(token_fragment_map, key=token_sortkey))
        yield (sentence, tuple(fragment_list))


def read_sentences(clf_file: TextIO, *tag_files: TextIO) -> Sequence[Tuple[tuple]]:
    """Read DRSs in parallel with token-level annotations.

    Reads DRSs from clf_file and token-level annotations from 1 or more block
    files. For each sentence, yields a tuple of tuples, where the first tuple
    contains the words, the second contains the DRS fragments for each word,
    and the rest contain the token-level annotations.
    """
    if len(tag_files) < 1:
        raise ValueError('must provide at least one tag file')
    clf_data = iter(read(clf_file))
    tag_data = iter(blocks.zip(*tag_files))
    while True:
        try:
            words, fragments = next(clf_data)
        except StopIteration:
            try:
                next(tag_data)
                raise ValueError('Length mismatch. More tags than words.')
            except StopIteration:
                return
        offset = 0
        while offset < len(words):
            try:
                current_tag_data = next(tag_data)
            except StopIteration:
                raise ValueError('Length mismatch. More words than tags.')
            length = len(current_tag_data[0])
            if length > len(words) - offset:
                raise ValueError('Length mismatch. More tags than words.')
            current_words = words[offset:][:length]
            current_fragments = fragments[offset:][:length]
            yield (current_words, current_fragments) + current_tag_data
            offset += length


def write(drs, flo):
    for token in drs:
        for clause in token:
            print(' '.join(clause), file=flo)
    print(file=flo)
