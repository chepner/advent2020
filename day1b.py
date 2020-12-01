import fileinput
import itertools

with open("day1a.input") as f:
    pairs = itertools.combinations(map(int, f), 3)
    for x, y, z in pairs:
        if x + y + z == 2020:
            print(x, y, z,  x*y*z)


