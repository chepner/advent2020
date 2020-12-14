import fileinput
from typing import Iterable, Type, TypeVar
import re

def parse_program(f):
    program = []
    for line in f:
        line = line.strip()
        if line.startswith("mask"):
            mask = line.split()[2]
            mask = ("mask", int(mask.replace("X", "1"), base=2), int(mask.replace("X", "0"), base=2))
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
            

def _main(f:Iterable[str]):
    program = parse_program(f)
    memory = run_program(program)
    return sum(memory.values())


def main_a(f: Iterable[str]):
    return _main(f)


def main_b(f: Iterable[str]):
    return _main(f)
