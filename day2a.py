import fileinput
import re

valid = 0

for line in fileinput.input():
    low_limit, high_limit, char, password = re.match("(\d+)-(\d+) (.): (.*)", line).groups()
    if int(low_limit) <= password.count(char) <= int(high_limit):
        valid += 1

print(valid)
