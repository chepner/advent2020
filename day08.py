import fileinput
from typing import Iterable, Type, TypeVar
from dataclasses import dataclass
import re


def run_program(prg):
    pc = 0
    acc = 0

    seen = set()
    while True:
        instr, arg = prg[pc]

        if pc in seen:
            print(f"Infinite loop detected, accumulator = {acc}")
            break

        seen.add(pc)
        if instr == "nop":
            pc += 1
        elif instr == "acc":
            acc += arg
            pc += 1
        elif instr == "jmp":
            pc += arg
        else:
            raise RuntimeError(f"Error: unrecognized opcode {instr}")


def main_a(f: Iterable[str]):
    program = []
    for line in f:
        instruction, arg = line.strip().split()
        arg = int(arg)
        program.append((instruction, arg))

    run_program(program)


def main_b(f: Iterable[str]):
    pass
