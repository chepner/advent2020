import fileinput
from typing import Iterable, Type, TypeVar
from dataclasses import dataclass
import re

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

@dataclass
class StrictPassport(Passport):
    def __post_init__(self):
        self.byr = int(self.byr)
        if not (1920 <= self.byr <= 2002):
            raise ValueError(f"Invalid birth year {self.byr}")
        self.iyr = int(self.iyr)
        if not(2010 <= self.iyr <= 2020):
            raise ValueError(f"Invalid issue year {self.iyr}")
        self.eyr = int(self.eyr)
        if not(2020 <= self.eyr <= 2030):
            raise ValueError(f"Invalid expiration year {self.eyr}")

        if self.hgt.endswith("cm"):
            lbound = 150
            hbound = 193
        elif self.hgt.endswith("in"):
            lbound = 59
            hbound = 76
        else:
            raise ValueError("Invalid height unit")

        hgt = int(self.hgt[:-2])
        if not(lbound <= hgt <= hbound):
            raise ValueError("Invalid height")

        if re.match("#[0-9a-fA-F]{6}", self.hcl) is None:
            raise ValueError("Invalid hair color")

        if self.ecl not in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}:
            raise ValueError("Invalid eye color")

        if len(self.pid) != 9 and not self.pid.isdigit():
            raise ValueError("Invalid passport ID")


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


T = TypeVar('T', bound=Passport)

def validate_passports(kind: Type[T], p: Iterable[dict[str,str]]) -> list[T]:
    valid = []
    for i, fields in enumerate(p):
        try:
            yield kind(**fields)
        except (TypeError, ValueError) as exc:
            print(f'Passport {i} is invalid: {exc}')


def main_a(f: Iterable[str]):
    passports = read_passport_data(f)

    valid_passports = validate_passports(Passport, passports)
    return sum(1 for v in valid_passports)


def main_b(f: Iterable[str]):
    passports = read_passport_data(f)

    valid_passports = validate_passports(StrictPassport, passports)
    return sum(1 for v in valid_passports)


if __name__ == "__main__":
    data = fileinput.input()
    print(main_a(data))
    print(main_b(data))
