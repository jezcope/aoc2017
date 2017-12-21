import fileinput as fi
import numpy as np
import re

PARTICLE_RE = re.compile(r'p=<(-?\d+),(-?\d+),(-?\d+)>, '
                         r'v=<(-?\d+),(-?\d+),(-?\d+)>, '
                         r'a=<(-?\d+),(-?\d+),(-?\d+)>')


def parse_input(lines):
    x = []
    v = []
    a = []
    for l in lines:
        m = PARTICLE_RE.match(l)
        x.append([int(x) for x in m.group(1, 2, 3)])
        v.append([int(x) for x in m.group(4, 5, 6)])
        a.append([int(x) for x in m.group(7, 8, 9)])

    return (np.arange(len(x)), np.array(x), np.array(v), np.array(a))


def resolve_collisions(x, v, a):
    (_, i, c) = np.unique(x, return_index=True, return_counts=True, axis=0)
    i = i[c == 1]
    return x[i], v[i], a[i]


def simulate_collisions(x, v, a, iterations=1000):
    for _ in range(iterations):
        v += a
        x += v
        x, v, a = resolve_collisions(x, v, a)

    return len(x)


i, x, v, a = parse_input(fi.input())

a_abs = np.sum(np.abs(a), axis=1)
a_min = np.min(a_abs)
a_i = np.squeeze(np.argwhere(a_abs == a_min))
closest = i[a_i[np.argmin(np.sum(np.abs(v[a_i]), axis=1))]]
print("Closest: ", closest)
print("Remaining particles: ", simulate_collisions(x, v, a))
