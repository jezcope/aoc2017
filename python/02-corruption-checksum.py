from fileinput import input

sheet = [sorted(int(x) for x in l.split()) for l in input()]

# Part 1
print(sum(x[-1] - x[0] for x in sheet))

# Part 2
print(sum(y // x for row in sheet for i, x in enumerate(row) for y in row[i+1:] if y % x == 0))
