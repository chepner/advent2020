import fileinput
import re

valid = 0

for line in fileinput.input():
    first_pos, second_pos, char, password = re.match("(\d+)-(\d+) (.): (.*)", line).groups()
    p1 = int(first_pos) - 1
    p2 = int(second_pos) - 1

    if password[p1] == char or password[p2] == char:
        if password[p1] != password[p2]:
            valid += 1

print(valid)
