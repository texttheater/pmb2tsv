#!/usr/bin/env python3


import json
import sys


import clf


if __name__ == '__main__':
    try:
        _, clf_path, tok_path = sys.argv
    except:
        print('USAGE: python3 drs2frag.py myfile.drs.clf myfile.token',
                file=sys.stderr)
        sys.exit(1)
    with open(clf_path) as clf_file, open(tok_path) as tok_file:
        for words, fragments, w in clf.read_sentences(clf_file, tok_file):
            assert words == tuple(w)
            for fragment in fragments:
                print(json.dumps(fragment))
            print()
