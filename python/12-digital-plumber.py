import fileinput as fi

groups = []

for line in fi.input():
    head, rest = line.split(' <-> ')
    group = set([int(head)])
    group.update([int(x) for x in rest.split(', ')])
    groups.append(group)

i = 0
while i < len(groups):
    current = groups[i]

    # keep going through until no more merges are made
    num_groups = len(groups) + 1
    while num_groups > len(groups):
        j = i+1
        num_groups = len(groups)
        while j < len(groups):
            if len(current & groups[j]) > 0:
                current.update(groups[j])
                del groups[j]
            else:
                j += 1
    i += 1


print("Number in group 0:", len([g for g in groups if 0 in g][0]))
print("Number of groups:", len(groups))
