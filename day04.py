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


class PassportError(ValueError):
    pass


@dataclass
class StrictPassport(Passport):
    def __post_init__(self):
        if len(self.byr) != 4 or not self.byr.isdigit() or not (1920 <= int(self.byr) <= 2002):
            raise PassportError(f"Invalid birth year {self.byr}")
        if len(self.iyr) != 4 or not self.iyr.isdigit() or not (2010 <= int(self.iyr) <= 2020):
            raise PassportError(f"Invalid issue year {self.byr}")
        if len(self.eyr) != 4 or not self.eyr.isdigit() or not (2020 <= int(self.eyr) <= 2030):
            raise PassportError(f"Invalid expiration year {self.byr}")

        if not self.hgt.endswith(("cm", "in")):
            raise PassportError(f"Invalid height: {self.hgt}")

        bounds = {
                "in": range(59, 77),
                "cm": range(150, 194)
                }
        h = self.hgt[:-2]
        unit = self.hgt[-2:]

        if not h.isdigit() or int(h) not in bounds[unit]:
            raise PassportError(f"Invalid height: {self.hgt}")

        if re.match("#[0-9a-fA-F]{6,6}", self.hcl) is None:
            raise PassportError(f"Invalid hair color {self.hcl}")

        if self.ecl not in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}:
            raise PassportError(f"Invalid eye color {self.ecl}")

        if len(self.pid) != 9 or not self.pid.isdigit():
            raise PassportError(f"Invalid passport ID {self.pid}")


def read_passport_data(f:Iterable[str] ) -> Iterable[dict[str,str]]:
    current = []
    for line in f:
        line = line.strip()
        if line:
            fields = line.split()
            current.extend(fields)
        elif current:
            yield dict(field.split(":") for field in current)
            current = []
    if current:
        yield dict(field.split(":") for field in current)


T = TypeVar('T', bound=Passport)

def validate_passports(kind: Type[T], p: Iterable[dict[str,str]]) -> list[T]:
    for i, fields in enumerate(p):
        try:
            yield kind(**fields)
        except (TypeError, ValueError) as exc:
            pass


def _main(passport_type, f):
    passports = read_passport_data(f)
    valid_passports = validate_passports(passport_type, passports)
    return sum(1 for v in valid_passports)


def main_a(f: Iterable[str]):
    return _main(Passport, f)


def main_b(f: Iterable[str]):
    return _main(StrictPassport, f)
