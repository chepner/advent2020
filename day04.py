import fileinput
from typing import Iterable
from dataclasses import dataclass

@dataclass
class Passport:
    byr: str
    iyr: str
    eyr: str
    hgt: str
    hcl: str
    ecl: str
    pid: str
    cid: str = None


def read_passport_data(f:Iterable[str] ) -> Iterable[dict[str,str]]:
    current = []
    count = 0
    total = 0
    nonempty = 0
    for line in f:
        total += 1
        line = line.strip()
        if line:
            nonempty += 1
            fields = line.split()
            current.extend(fields)
        elif current:
            yield dict(field.split(":") for field in current)
            count += 1
            current = []
    if current:
        count += 1
        yield dict(field.split(":") for field in current)

    print(f'{count} passports read on {nonempty}/{total} lines')


def validate_passports(p: Iterable[dict[str,str]]) -> list[Passport]:
    valid = []
    for fields in p:
        try:
            yield Passport(**fields)
        except TypeError:
            pass


def main_a(f: Iterable[str]):
    passports = read_passport_data(f)

    valid_passports = validate_passports(passports)
    return sum(1 for v in valid_passports)


def main_b(map_data):
    pass


if __name__ == "__main__":
    data = fileinput.input()
    print(main_a(data))
    print(main_a(data))
