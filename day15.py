import fileinput
from typing import Iterable, Type, TypeVar
from itertools import count, islice
import logging
import sys


def turn(numbers):
    tn = count(1)
    memory = {}

    last = None
    for x in numbers:
        t = next(tn)
        yield x
        if last is not None:
            memory[last] = t - 1
        last = x

    for t in tn:
        t0 = memory.get(last, t-1)
        next_ = t - 1 - t0
        memory[last] = t - 1
        yield next_
        last = next_


def _main(f:Iterable[str], n):
    numbers = [int(x) for x in  f.readline().strip().split(',')]
    return next(islice(turn(numbers), n-1, n))

def main_a(f: Iterable[str]):
    return _main(f, 2020)

def main_b(f: Iterable[str]):
    return _main(f, 30000000)
