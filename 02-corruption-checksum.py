from fileinput import input

sheet = [[int(x) for x in l.split()] for l in input()]

# Part 1
print(sum(abs(max(x) - min(x)) for x in sheet))

# Part 2
def rowsum_div(row):
    row = sorted(row)
    return sum(y // x for i, x in enumerate(row) for y in row[i+1:] if y % x == 0)

print(sum(rowsum_div(x) for x in sheet))
