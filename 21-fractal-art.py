import fileinput as fi
from math import sqrt
from functools import reduce, partial
import operator

INITIAL_PATTERN = ((0, 1, 0), (0, 0, 1), (1, 1, 1))
DECODE = ['.', '#']
ENCODE = {'.': 0, '#': 1}

concat = partial(reduce, operator.concat)


def rotate(p):
    size = len(p)
    return tuple(tuple(p[i][j] for i in range(size))
                 for j in range(size - 1, -1, -1))


def flip(p):
    return tuple(p[i] for i in range(len(p) - 1, -1, -1))


def permutations(p):
    yield p
    yield flip(p)
    for _ in range(3):
        p = rotate(p)
        yield p
        yield flip(p)


def print_pattern(p):
    print('-' * len(p))
    for row in p:
        print(' '.join(DECODE[x] for x in row))
    print('-' * len(p))


def build_pattern(s):
    return tuple(tuple(ENCODE[c] for c in row)
                 for row in s.split('/'))


def build_pattern_book(lines):
    book = {}
    for line in lines:
        source, target = line.strip().split(' => ')
        for rotation in permutations(build_pattern(source)):
            book[rotation] = build_pattern(target)

    return book


def subdivide(pattern):
    size = 2 if len(pattern) % 2 == 0 else 3
    n = len(pattern) // size
    return (tuple(tuple(pattern[i][j] for j in range(y * size, (y + 1) * size))
                  for i in range(x * size, (x + 1) * size))
            for x in range(n)
            for y in range(n))


def rejoin(parts):
    n = int(sqrt(len(parts)))
    size = len(parts[0])
    return tuple(concat(parts[i + k][j] for i in range(n))
                 for k in range(0, len(parts), n)
                 for j in range(size))


def enhance_once(p, book):
    return rejoin(tuple(book[part] for part in subdivide(p)))


def enhance(p, book, n, progress=None):
    for _ in range(n):
        p = enhance_once(p, book)
    return p


book = build_pattern_book(fi.input())

intermediate_pattern = enhance(INITIAL_PATTERN, book, 5)
print("After 5 iterations:", sum(sum(row) for row in intermediate_pattern))

final_pattern = enhance(intermediate_pattern, book, 13)
print("After 18 iterations:", sum(sum(row) for row in final_pattern))
