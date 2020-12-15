import fileinput
from typing import Iterable, Type, TypeVar
from itertools import count, islice
import logging


def turn(numbers):
    tn = count(1)
    memory = {}

    last = None
    for x in numbers:
        t = next(tn)
        logging.info("Turn %d", t)
        logging.info("Yielding starting number %d", x)
        yield x
        if last is not None:
            memory[last] = t - 1
            logging.info("Set memory[%d] to %s", last, memory[last])
        last = x

    for t in tn:
        logging.info("Turn %d", t)
        if last in memory:
            t0 = memory[last]
            logging.info("Found %d in memory: %s", last, t0)
            next_ = t - 1 - t0
            memory[last] = t - 1
            logging.info("Yielding number %d", next_)
            yield next_
            last = next_
        else:
            logging.info("%d not in memory", last)
            yield 0
            memory[last] = t - 1
            last = 0


def _main(f:Iterable[str]):
    numbers = [int(x) for x in  f.readline().strip().split(',')]
    return next(islice(turn(numbers), 2019, 2020))

def main_a(f: Iterable[str]):
    return _main(f)

def main_b(f: Iterable[str]):
    return _main(f)
