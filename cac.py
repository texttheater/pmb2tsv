"""Library for reading C&C-style Prolog parse files.

The input format is the "boxer" output format of the C&C CCG parser. It is also
supported by EasyCCG and other CCG parsers.
"""

import ast
import os
import re
import sys
import subprocess

_start_pattern = re.compile(r"^ccg\((\d+),$")
_token_pattern = re.compile(
        r"(\s*)t\((\S+), '([^ ]+)', \[(.*)\]\)(.*)")
_constopen_pattern = re.compile(
        r" +([a-z]{2,3}\(.*,)$")
rolelist_pattern = re.compile(r"\[([A-Z][a-z]+(?:,[A-Z][a-z]+)*)\]")

def attlist(tags):
    return '[%s]' % ', '.join(map(att, filter_atts(tags.items())))

def prologify_rolelist(string):
    match = rolelist_pattern.match(string)
    if not match:
        sys.stderr.write('ERROR: cannot parse rolelist %s\n' % string)
        sys.exit(1)
    roles = match.group(1).split(',')
    quoted_roles = map(lambda x: "'%s'" % x, roles)
    return '[%s]' % ','.join(quoted_roles)

def att(pair):
    key, value = pair
    if key == 'verbnet':
        return "verbnet:%s" % prologify_rolelist(value)
    elif key in ['from', 'to', 'toknum']:
        return "%s:%d" % (key, int(value))
    elif key == 'antecedent':
        try:
            return "%s:%d" % (key, int(value))
        except ValueError:
            return "%s:'%s'" % (key, escape(value))
    else:
        return "%s:'%s'" % (key, escape(value))

def filter_atts(items):
    for (key, value) in items:
        if key not in ['super']:
            yield (key, value)

class _Token:

    def __init__(self, lead, trail, form, tags):
        self.lead = lead
        self.trail = trail
        self.form = form
        self.tags = tags

    def line(self):
        return u"%st(%s, '%s', %s)%s\n" % (
                self.lead,
                self.tags['super'],
                escape(self.form),
                attlist(self.tags),
                self.trail)

def snum(line): 
    match = _start_pattern.search(line)
    if match:
        return int(match.group(1))
    return None 

def startline(snum):
    return 'ccg(' + str(snum) + ',\n'

def token(line):
    match = _token_pattern.match(line)
    if match:
        tags = {}
        lead = match.group(1)
        tags['super'] = match.group(2)
        form = unescape(match.group(3))
        kvpairs = match.group(4).split(', ')
        if kvpairs == ['']:
            kvpairs = []
        for kvpair in kvpairs:
            layer, tag = kvpair.split(':', 1)
            tags[layer] = parse_tag(tag)
        trail = match.group(5)
        return _Token(lead, trail, form, tags)


def parse_tag(tag):
    if tag.isdigit():
        return int(tag)
    if tag.startswith("'") and tag.endswith("'"):
        return unescape(tag[1:-1])
    return ast.literal_eval(tag)


def constopen(line):
    match = _constopen_pattern.match(line)
    if match:
        return match.group(1)


def prolog2ccgcat(term, path):
    query = 'write_cat(' + term + ').'
    process = subprocess.Popen(['swipl', '-l', path], stdin=subprocess.PIPE,
            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    (output, errput) = process.communicate(input=query)
    return output.rstrip()

def unescape(x):
    result = u''
    expecting = False
    for c in x:
        if expecting:
            if c in [u'\'', u'"', u'\\']:
                result += c
                expecting = False
            else:
                raise Exception('Invalid escape character ' + c + ' in string '\
                        + x)
        else:
            if c == u'\\':
                expecting = True
            else:
                result += c
    return result

def escape(x, singlequote=True):
    result = u''
    for c in x:
        if c in [u'\'', u'"', u'\\'] and (singlequote or c != u'\''):
            result += '\\'
        result += c
    return result
