import fileinput
from itertools import count

map_pattern = fileinput.input()


trees = 0
row = next(map_pattern)
# We can assume there is no tree in the first square,
# start = row.find('.')
start = 0
delta = 3
xs = count(start+delta, delta)
for x, row in zip(xs, map_pattern):
    row = row.strip()
    x = x%len(row)
    print(" "*x + "v")
    print(row)
    if row[x] == '#':
        trees += 1

# Not 74 (I counted the first row)
# Not 63 (?. Worked with the sample...)
# 270. The newlines messed up the wrapping,
# and the sample never had to wrap.
print(trees)
