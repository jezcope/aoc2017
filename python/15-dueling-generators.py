import sys
from itertools import islice

M = 2147483647


def generator(factor, current):
    while True:
        current = (current * factor) % M
        yield current


def duel(n, gen_a, gen_b):
    count = 0
    for value_a, value_b in islice(zip(gen_a, gen_b), n):
        if (value_a & (2**16-1)) == (value_b & (2**16-1)):
            count += 1

    return count


factor_a = 16807
factor_b = 48271

start_a = int(sys.argv[1])
start_b = int(sys.argv[2])

print("Duel 1:", duel(40000000,
                      generator(factor_a, start_a),
                      generator(factor_b, start_b)))

print("Duel 2:", duel(5000000,
                      (x for x in generator(factor_a, start_a) if x % 4 == 0),
                      (x for x in generator(factor_b, start_b) if x % 8 == 0)))
