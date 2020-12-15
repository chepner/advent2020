import fileinput
from typing import Iterable, Type, TypeVar
from itertools import count, islice
import logging


def turn(numbers, n):
    tn = count(1)
    memory = {}

    last = None
    for x in numbers:
        t = next(tn)
        logging.debug("Turn %d", t)
        logging.debug("Yielding starting number %d", x)
        yield x
        if last is not None:
            memory[last] = t - 1
            logging.debug("Set memory[%d] to %s", last, memory[last])
        last = x

    for t in tn:
        if t % 1000000 == 0:
            logging.info("Turn %d/%d", t, n)
        logging.debug("Turn %d", t)
        if last in memory:
            t0 = memory[last]
            logging.debug("Found %d in memory: %s", last, t0)
            next_ = t - 1 - t0
            memory[last] = t - 1
            logging.debug("Yielding number %d", next_)
            yield next_
            last = next_
        else:
            logging.debug("%d not in memory", last)
            yield 0
            memory[last] = t - 1
            last = 0


def _main(f:Iterable[str], n):
    numbers = [int(x) for x in  f.readline().strip().split(',')]
    return next(islice(turn(numbers, n), n-1, n))

def main_a(f: Iterable[str]):
    return _main(f, 2020)

def main_b(f: Iterable[str]):
    return _main(f, 30000000)
