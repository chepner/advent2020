import fileinput
from typing import Iterable, Type, TypeVar
from itertools import chain, combinations
import re


class ProgramError(RuntimeError):
    def __init__(self, *, ps, **kwargs):
        super().__init__(**kwargs)
        self.program_state = ps


class InfiniteRecursionError(ProgramError):
    pass


class InvalidOpcodeError(ProgramError):
    def __init__(self, *, opcode, **kwargs):
        super().__init__(**kwargs)
        self.opcode = opcode


class ProgramState:
    def __init__(self, *, pc, acc):
        self.pc = pc
        self.acc = acc


def run_program(prg):
    state = ProgramState(pc=0, acc=0)

    seen = set()
    while True:
        try:
            instr, arg = prg[state.pc]
        except IndexError:
            return state

        if state.pc in seen:
            raise InfiniteRecursionError(ps=state)

        seen.add(state.pc)

        if instr == "nop":
            state.pc += 1
        elif instr == "acc":
            state.acc += arg
            state.pc += 1
        elif instr == "jmp":
            state.pc += arg
        else:
            raise InvalidOpcodeError(ps=state, opcode=instr)


# From itertools recipes
def power(iterable):
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))


# Change a jmp to a no-op or vice versa
def fix_program(prg):
    jmp_or_nop = [i for i, (instr, _) in enumerate(prg) if instr == "jmp" or instr == "nop"]
    
    for alteration in power(jmp_or_nop):
        new_prog = prg[:]
        for i in alteration:
            if prg[i][0] == "jmp":
                new_prog[i] = ("nop", prg[i][1])
            elif prg[i][0] == "nop":
                new_prog[i] = ("jmp", prg[i][1])

        try:
            rv = run_program(new_prog)
            print(f"Accumulator = {rv}")
            return new_prog

        except ProgramError as exc:
            print(f"Failed: {exc!r}")

    raise RuntimeError("Cannot fix infinite recursion") 


def parse_program(f):
    program = []
    for line in f:
        instruction, arg = line.strip().split()
        arg = int(arg)
        program.append((instruction, arg))
    return program


def main_a(f: Iterable[str]):
    program = parse_program(f)
    try:
        state = run_program(program)
    except InfiniteRecursionError as exc:
        return exc.program_state.acc


def main_b(f: Iterable[str]):
    program = parse_program(f)
    try:
        program = fix_program(program)
        state = run_program(program)  # Yes, we already ran it.
        return state.acc
    except RuntimeError:
        print("Unable to fix program")
