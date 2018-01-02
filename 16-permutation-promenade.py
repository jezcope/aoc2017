from collections import deque

parse_params = {
    's': lambda x: int(x),
    'x': lambda x: tuple(map(int, x.split('/'))),
    'p': lambda x: tuple(x.split('/')),
}


def parse_move(move):
    move_type = move[0]
    return (move_type, parse_params[move_type](move[1:]))


def eval_spin(shift, dancers):
    dancers.rotate(shift)


def eval_exchange(params, dancers):
    i, j = params
    dancers[i], dancers[j] = dancers[j], dancers[i]


def eval_partner(params, dancers):
    a, b = params
    i = dancers.index(a)
    j = dancers.index(b)
    dancers[i], dancers[j] = dancers[j], dancers[i]


evaluators = dict(s=eval_spin, x=eval_exchange, p=eval_partner)


moves = list(map(parse_move, input().split(',')))
dancers = deque("abcdefghijklmnop")
orig_dancers = dancers.copy()
cycle = [tuple(dancers)]

for m, p in moves:
    evaluators[m](p, dancers)
print(''.join(dancers))

while dancers != orig_dancers:
    cycle.append(tuple(dancers))
    for m, p in moves:
        evaluators[m](p, dancers)

print(''.join(cycle[1_000_000_000 % len(cycle)]))
