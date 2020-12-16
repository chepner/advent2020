import fileinput
from typing import Iterable, Type, TypeVar
import re
import logging
import sys
import math


def parse_input(f):
    # parse fields
    fields = set()
    for line in f:
        line = line.strip()
        if not line:
            break

        name, range_data = line.split(':')
        range_data = range_data.split('or')
        range_args = []
        for rd in range_data:
            x, y = rd.split("-")
            range_args.append((int(x), int(y)+1))

        field = (name, tuple(range(*rd) for rd in range_args))

        fields.add(field)

    next(f)  # "your ticket:"
    line = next(f).strip()
    your_ticket = [int(x) for x in line.split(',')]

    next(f)  # blank
    next(f)  # "nearby tickets"
    nearby_tickets = []
    for line in f:
        nearby_tickets.append([int(x) for x in line.strip().split(',')])

    return (fields, your_ticket, nearby_tickets)


def valid(number, ranges):
    return any(number in r for r in ranges)


def compute_error_rate(fields, tickets):
    error_rate = 0
    for t in tickets:
        for n in t:
            if not any(valid(n, ranges) for name, ranges in fields):
                error_rate += n
    return error_rate

def filter_invalid_tickets(fields, tickets):
    for t in tickets:
        for n in t:
            if not any(valid(n, ranges) for name, ranges in fields):
                break
        else:
            yield t


def determine_field_order(fields, tickets):
    num_cols = len(fields)
    possibilities = {}
    for field_name, ranges in fields:
        p = set()
        for col in range(num_cols):
            if all(valid(t[col], ranges) for t in tickets):
                p.add(col)
        possibilities[field_name] = p

    fixed = {}
    while possibilities:
        for field_name in list(possibilities.keys()):
            values = list(possibilities[field_name])
            if len(values) == 1:
                fixed[field_name] = values[0]
                del possibilities[field_name]
                for other in possibilities:
                    possibilities[other].remove(values[0])


    return fixed


def _main(f:Iterable[str]):
    notes = parse_input(f)
    erate = compute_error_rate(notes[0], notes[2])
    # assert erate == 23115
    return erate


def main_a(f: Iterable[str]):
    return _main(f)


def main_b(f: Iterable[str]):
    notes = parse_input(f)
    valid = list(filter_invalid_tickets(notes[0], notes[2]))
    field_assignments = determine_field_order(notes[0], valid)
    result = math.prod(notes[1][v] for k, v in field_assignments.items() if k.startswith("departure"))
    # assert result == 239727793813
    return result
