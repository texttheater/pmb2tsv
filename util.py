import itertools


def groupby_ranges(iterable):
    """Equal consecutive elements in iterable and their index ranges."""
    offset = 0
    for key, values in itertools.groupby(iterable):
        length = len(tuple(values))
        yield key, range(offset, offset + length)
        offset += length


def minindex(iterable, key=lambda x: x):
    """Minimum with index.

    Returns a pair (i, e) where e is the smallest element in iterable (as per
    the ordering function key) and i is its index.
    """
    return min(enumerate(iterable), key=lambda p: key(p[1]))


def rangedist(range1, range2):
    """Distance between two ranges.

    0 if the ranges overlap. Negative if range1 comes before range2.
    Positive if range1 comes after range2.
    """
    if range1[-1] < range2[0]:
        return range1[-1] - range2[0]
    elif range1[0] > range2[-1]:
        return range1[0] - range2[-1]
    else:
        return 0
