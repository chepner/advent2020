import fileinput
from typing import Iterable, Type, TypeVar
import logging
import sys
from itertools import product


def parse_input(f):
    result = {}
    rows = 0
    data = []
    for line in f:
        rows += 1
        data.append(line.strip())

    cols = len(data[0])

    for row in range(rows):
        for col in range(cols):
            scaled = (row - rows//2, col - cols//2, 0)
            result[scaled] = data[row][col]

    return result

        
def neighbors(cell):
    x, y, z = cell
    for dx, dy, dz in product((-1, 0, 1), repeat=3):
        if dx == dy == dz == 0:
            continue
        yield x + dx, y + dy, z + dz


def step(grid):
    new_grid = {}
    to_update = set(n for c in grid for n in neighbors(c))
    for cell in to_update:
        state = grid.get(cell, '.')
        active_neighbors = sum(1 for neighbor in neighbors(cell) if grid.get(neighbor, '.') == '#')
        if state == '#':
            if active_neighbors in (2, 3):
                new_grid[cell] = '#'
            else:
                new_grid[cell] = '.'
        else:
            if active_neighbors == 3:
                new_grid[cell] = '#'
            else:
                new_grid[cell] = '.'
    return new_grid



def _main(f:Iterable[str]):
    grid = parse_input(f)
    for _ in range(6):
        grid = step(grid)
    return sum(x == '#' for x in grid.values())


def main_a(f: Iterable[str]):
    return _main(f)


def main_b(f: Iterable[str]):
    pass
