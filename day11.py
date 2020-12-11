import fileinput
from typing import Iterable, Type, TypeVar
from itertools import product


def parse_floorplan(f):
    floorplan = []
    for line in f:
        floorplan.append(line.strip())
    return floorplan


def get(floorplan, s):
    x, y = s
    if 0 <= x < len(floorplan[0]) and 0 <= y < len(floorplan):
        return floorplan[y][x]


def neighbors(floorplan, s):
    x,y = s
    for xd, yd in product(range(-1, 2), repeat=2):
        if xd == yd == 0:
            continue
        if (n := get(floorplan, (x+xd, y+yd))) is not None:
            yield n


def transition(floorplan, s):
    state = get(floorplan, s)
    if state == 'L' and all(x != '#' for x in neighbors(floorplan, s)):
        return '#'
    elif state == '#' and sum(x == '#' for x in neighbors(floorplan, s)) >= 4:
        return 'L'
    else:
        return state


def step(floorplan):
    width = len(floorplan[0])
    height = len(floorplan)
    return [[transition(floorplan, (x,y)) for x in range(width)] for y in range(height)]


def print_floorplan(floorplan):
    for row in floorplan:
        print(''.join(row))


def count_occupied(floorplan):
    return sum(sum(x == '#' for x in row) for row in floorplan)


def main_a(f: Iterable[str]):
    floorplan = parse_floorplan(f)
    count = 0
    while True:
        next_floorplan = step(floorplan)
        if next_floorplan == floorplan:
            break
        count += 1
        floorplan = next_floorplan
    return count_occupied(floorplan)


def main_b(f: Iterable[str]):
    floorplan = parse_floorplan(f)
