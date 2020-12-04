import fileinput
from itertools import count, islice, tee
from functools import reduce
from operator import mul


def main_a(map_pattern):


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
        # print(" "*x + "v")
        # print(row)
        if row[x] == '#':
            trees += 1

    # Not 74 (I counted the first row)
    # Not 63 (?. Worked with the sample...)
    # 270. The newlines messed up the wrapping,
    # and the sample never had to wrap.
    return(trees)


def main_b(map_data):

    map_input = list(map_data)

    tree_counts = []
    for x_delta, y_delta in [(1, 1), (3, 1), (5, 1), (7,1), (1, 2)]:
        # print(f"Slope: right {x_delta}, down {y_delta}")
        map_pattern = islice(map_input, 0, None, y_delta)

        trees = 0
        row = next(map_pattern)
        # We can assume there is no tree in the first square,
        # start = row.find('.')
        start = 0
        xs = count(start+x_delta, x_delta)
        for x, row in zip(xs, map_pattern):
            row = row.strip()
            x = x%len(row)
            # print(" "*x + "v")
            # print(row)
            if row[x] == '#':
                trees += 1

        # print(f"\n==== {trees} ===\n")
        tree_counts.append(trees)


    # 3102624000 is too big
    # 2286144000 is too big
    # 2122848000
    print(tree_counts)
    return(reduce(mul, tree_counts))

if __name__ == "__main__":
    data_a, data_b = tee(fileinput.input())
    print(main_a(data_a))
    print(main_b(data_b))
