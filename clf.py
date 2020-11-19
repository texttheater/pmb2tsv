"""Functions for handling clauses.

As in the PMB CLF format.
"""


import collections
import re
import util


from typing import List, NewType, Sequence, TextIO, Tuple


Clause = NewType('Clause', Tuple)
DRS = NewType('DRS', Tuple[Clause])


SEP_PATTERN = re.compile(r' *% ?')
TOK_PATTERN = re.compile(r'([^ ]+ \[(?P<fr>\d+)\.\.\.(?P<to>\d+)\])')


def token_sortkey(token):
    match = TOK_PATTERN.match(token)
    return int(match.group('fr'))


def blocks(stream):
    """Splits a text stream by empty lines.

    Reads a file-like object and returns its contents chopped into a sequence
    of blocks terminated by empty lines.
    """
    block = []
    for line in stream:
        block.append(line)
        (chomped,) = line.splitlines()
        if chomped == '':
            yield block
            block = []
    if block: # in case the last block is not terminated
        yield block


def read(flo: TextIO) -> Sequence[Tuple[Tuple[str], Tuple[DRS]]]:
    for block in util.blocks(flo):
        assert block[0].startswith('%%% ')
        assert block[1].startswith('%%% ')
        assert block[2].startswith('%%% ')
        if block[-1].rstrip() == '':
            block = block[:-1]
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


def read_sentences(flo: TextIO, taglists: Sequence[Sequence[str]]) -> Sequence[Tuple[Tuple[str], Tuple[DRS], Tuple[str]]]:
    clf_data = read(flo)
    while True:
        try:
            words, fragments = next(clf_data)
        except StopIteration:
            try:
                next(taglists)
                raise ValueError(f'Length mismatch. More words in taglists than in DRSs.')
            except StopIteration:
                return
        offset = 0
        while offset < len(words):
            try:
                current_taglist = next(taglists)
            except StopIteration:
                raise ValueError('Length mismatch. More words in DRSs than in taglists.')
            if len(current_taglist) > len(words) - offset:
                raise ValueError('Length mismatch. More words in tagslists than in DRSs.')
            current_words = words[offset:][:len(current_taglist)]
            current_fragments = fragments[offset:][:len(current_taglist)]
            yield current_words, current_fragments, current_taglist
            offset += len(current_taglist)


def write(drs, flo):
    for token in drs:
        for clause in token:
            print(' '.join(clause), file=flo)
    print(file=flo)
