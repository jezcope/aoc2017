import numpy as np

STEPS = {d: np.array(v) for d, v in
         [('ne', (1, 0)), ('se', (0, -1)), ('s', (-1, -1)),
          ('sw', (-1, 0)), ('nw', (0, 1)), ('n', (1, 1))]}


def hex_grid_distance(l):
    if sum(np.sign(l)) == 0:  # i.e. opposite signs
        return sum(abs(l))
    else:
        return max(abs(l))


path = input().strip().split(',')

location = np.array((0, 0))
max_distance = 0

for step in map(STEPS.get, path):
    location += step
    max_distance = max(max_distance, hex_grid_distance(location))

distance = hex_grid_distance(location)

print("Child process is at", location, "which is", distance, "steps away")
print("Greatest distance was", max_distance)
