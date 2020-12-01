import fileinput
import itertools

with open("day1a.input") as f:
    pairs = itertools.combinations(map(int, f), 2)
    for x, y in pairs:
        if x + y == 2020:
            print(x, y, x*y)


