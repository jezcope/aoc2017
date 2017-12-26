import re
import fileinput as fi
from math import inf
from collections import defaultdict

INSTRUCTION_RE = re.compile(r'(\w+) (inc|dec) (-?\d+) if (.+)\s*')


def parse_instruction(instruction):
    match = INSTRUCTION_RE.match(instruction)
    return match.group(1, 2, 3, 4)


def exec_instruction(registers, instruction):
    name, op, value, cond = instruction

    value = int(value)
    if op == 'dec':
        value = -value

    if eval(cond, globals(), registers):
        registers[name] += value


def highest_value(registers):
    return sorted(registers.items(), key=lambda x: x[1], reverse=True)[0][1]


global_max = -inf
registers = defaultdict(lambda: 0)
for i in map(parse_instruction, fi.input()):
    exec_instruction(registers, i)
    global_max = max(global_max, highest_value(registers))

print('Max value:', highest_value(registers))
print('All-time max:', global_max)
