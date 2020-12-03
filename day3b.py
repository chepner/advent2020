import fileinput
from itertools import count, islice

map_pattern = fileinput.input()


trees = 0
row = next(map_pattern)
# We can assume there is no tree in the first square,
# start = row.find('.')
start = 0
x_delta = 3
y_delta = 1
xs = count(start+x_delta, y_delta*x_delta)
for x, row in zip(xs, map_pattern):
    for _ in range(y_delta-1):
        next(map_pattern)
    row = row.strip()
    x = x%len(row)
    print(" "*x + "v")
    print(row)
    if row[x] == '#':
        trees += 1

print(trees)
