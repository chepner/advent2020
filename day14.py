import fileinput
from typing import Iterable, Type, TypeVar
from itertools import chain, combinations
import re


def parse_program(f):
    program = []
    for line in f:
        line = line.strip()
        if line.startswith("mask"):
            mask = line.split()[2]
            mask = ("mask",
                    int(mask.replace("X", "1"), base=2),
                    int(mask.replace("X", "0"), base=2),
                    mask)
            program.append(mask)
        else:
            m = re.match(r"mem\[(.*)\] = (.*)", line)
            assignment = ("assign", int(m.group(1)), int(m.group(2)))
            program.append(assignment)

    return program


def run_program(p):
    mask = (-1, 0)
    mem = {}
    for instr in p:
        # print(instr)
        if instr[0] == "mask":
            mask = instr[1:]
            # print(mask)
        elif instr[0] == "assign":
            addr, val = instr[1:]
            # print(f"mem[{addr}] = {mem.get(addr)} -> ", end='')
            val = val & mask[0] | mask[1]
            mem[addr] = val
            # print(f"mem[{addr}] = {mem[addr]}")

    return mem
            

def power(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))


def addresses(mask):
    if "X" not in mask:
        mask = int(mask, base=2)
        yield mask
    else:
        yield from addresses(mask.replace("X", "1", 1))
        yield from addresses(mask.replace("X", "0", 1))


def v2_mask(addr, mask):
    return "".join( "X" if m == "X" else "1" if m == "1" else a for a, m in zip(addr, mask))


def run_program_v2(p):
    mem = {}
    for instr in p:
        if instr[0] == "mask":
            mask = instr[1:]
        elif instr[0] == "assign":
            addr, val = instr[1:]
            addr_bin = f'{bin(addr)[2:]:0>36}'
            addr_template = v2_mask(addr_bin, mask[2])
            for real_addr in addresses(addr_template):
                mem[real_addr] = val

    return mem


def _main(f:Iterable[str], d):
    program = parse_program(f)
    memory = d(program)
    return sum(memory.values())


def main_a(f: Iterable[str]):
    return _main(f, run_program)


def main_b(f: Iterable[str]):
    return _main(f, run_program_v2)
