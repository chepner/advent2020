import fileinput
from itertools import count
from functools import reduce
from operator import mul

map_input = list(fileinput.input())

tree_counts = []
for x_delta, y_delta in [(1, 1), (3, 1), (5, 1), (7,1), (1, 2)]:
    map_pattern = iter(map_input)

    trees = 0
    row = next(map_pattern)
    # We can assume there is no tree in the first square,
    # start = row.find('.')
    start = 0
    xs = count(start+x_delta, y_delta*x_delta)
    for x, row in zip(xs, map_pattern):
        for _ in range(y_delta-1):
            next(map_pattern)
        row = row.strip()
        x = x%len(row)
        # print(" "*x + "v")
        # print(row)
        if row[x] == '#':
            trees += 1

    print(f"\n==== {trees} ===\n")
    tree_counts.append(trees)


# 3102624000 is too big
print(tree_counts)
print(reduce(mul, tree_counts))
