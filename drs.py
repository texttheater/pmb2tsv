import itertools
import re
import sys


REF_PATTERN = re.compile(r'(?P<letter>[beptsx])(?P<number>\d+)$')


def args(clause):
    return itertools.chain((clause[0],), clause[2:])


def max_ref_num(letter, clauses):
    def nums():
        for clause in clauses:
            for arg in args(clause):
                match = REF_PATTERN.match(arg)
                if match and match.group('letter') == letter:
                    yield int(match.group('number'))
    return max(nums(), default=0)


def is_ref(arg):
    return REF_PATTERN.match(arg) != None


def is_concept(arg):
    return (not is_ref(arg)) and arg[0].islower()


def is_constant(arg):
    return arg.startswith('"') and arg.endswith('"') # FIXME could be a sense
