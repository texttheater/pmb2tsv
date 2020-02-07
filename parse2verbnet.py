import cac
import sys


if __name__ == '__main__':
    current_snum = None
    for line in sys.stdin:
        if not line.rstrip():
            if current_snum is not None:
                print()
                current_snum = None
            continue
        snum = cac.snum(line)
        if snum is not None:
            current_snum = snum
            current_const = ''
            continue
        token = cac.token(line)
        if token is not None:
            if 'verbnet' in token.tags:
                print(token.tags['verbnet'])
            else:
                print('O')
