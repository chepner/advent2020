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


class YearError(ValueError): pass
class YearTooEarly(YearError): pass
class YearTooLate(YearError): pass


def _validate_year(s, lbound, hbound):
    if len(s) != 4:
        raise ValueError(f"Not 4-digit year: {s}")
    s = int(s)
    if s < lbound:
        raise YearTooEarly(s)
    if s > hbound:
        raise YearTooLate(s)


class HeightError(ValueError): pass
class TooShort(HeightError): pass
class TooTall(HeightError): pass


def _validate_height(h):
    if h.endswith("cm"):
        lbound = 150
        hbound = 193
    elif h.endswith("in"):
        lbound = 59
        hbound = 76
    else:
        raise HeightError(f"Invalid unit {h[-2:]}")
    h = int(h[:-2])
    if h < lbound:
        raise TooShort(h)
    if h > hbound:
        raise TooTall(h)


class PassportError(ValueError):
    pass


@dataclass
class StrictPassport(Passport):
    def __post_init__(self):
        try:
            _validate_year(self.byr, 1920, 2002)
        except YearError as exc:
            raise PassportError(f"Invalid birth year: {exc}")
        try:
            _validate_year(self.iyr, 2010, 2020)
        except YearError as exc:
            raise PassportError(f"Invalid issue year: {exc}")
        try:
            _validate_year(self.eyr, 2020, 2030)
        except YearError as exc:
            raise PassportError(f"Invalid expiration year: {exc}")

        try:
            _validate_height(self.hgt)
        except HeightError as exc:
            raise PassportError(f"Invalid height: {exc}")

        if re.match("#[0-9a-fA-F]{6,6}", self.hcl) is None:
            raise PassportError(f"Invalid hair color {self.hcl}")

        if self.ecl not in {"amb", "blu", "brn", "gry", "grn", "hzl", "oth"}:
            raise PassportError(f"Invalid eye color {self.ecl}")

        if len(self.pid) != 9 or not self.pid.isdigit():
            raise PassportError(f"Invalid passport ID {self.pid}")


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
    for i, fields in enumerate(p):
        try:
            yield kind(**fields)
        except (TypeError, ValueError) as exc:
            print(f'Passport {i} is invalid: {exc}')


def _main(passport_type, f):
    passports = read_passport_data(f)
    valid_passports = validate_passports(passport_type, passports)
    return sum(1 for v in valid_passports)


def main_a(f: Iterable[str]):
    return _main(Passport, f)


def main_b(f: Iterable[str]):
    return _main(StrictPassport, f)
