import math


def reallocate(mem):
    max_val = -math.inf
    size = len(mem)
    for i, x in enumerate(mem):
        if x > max_val:
            max_val = x
            max_index = i

    i = max_index
    mem[i] = 0
    remaining = max_val

    while remaining > 0:
        i = (i + 1) % size
        mem[i] += 1
        remaining -= 1

    return mem


def detect_cycle(mem):
    mem = list(mem)
    steps = 0
    prev_states = {}

    while tuple(mem) not in prev_states:
        prev_states[tuple(mem)] = steps
        steps += 1
        mem = reallocate(mem)

    return (steps, steps - prev_states[tuple(mem)])


initial_state = map(int, input().split())

print("Initial state is ", initial_state)
steps, cycle = detect_cycle(initial_state)
print("Steps to cycle: ", steps)
print("Steps in cycle: ", cycle)
