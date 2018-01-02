from collections import deque, namedtuple
from collections.abc import Iterator
from tqdm import tqdm
import re
import fileinput as fi

RE_ISTATE = re.compile(r'Begin in state (?P<state>\w+)\.')
RE_RUNTIME = re.compile(
    r'Perform a diagnostic checksum after (?P<steps>\d+) steps.')
RE_STATETRANS = re.compile(
    r"In state (?P<state>\w+):\n"
    r"  If the current value is (?P<read0>\d+):\n"
    r"    - Write the value (?P<write0>\d+)\.\n"
    r"    - Move one slot to the (?P<move0>left|right).\n"
    r"    - Continue with state (?P<next0>\w+).\n"
    r"  If the current value is (?P<read1>\d+):\n"
    r"    - Write the value (?P<write1>\d+)\.\n"
    r"    - Move one slot to the (?P<move1>left|right).\n"
    r"    - Continue with state (?P<next1>\w+).")
MOVE = {'left': -1, 'right': 1}

Rule = namedtuple('Rule', 'write move next_state')

class TuringMachine:
    def __init__(self, program=None):
        self.tape = deque()
        self.transition_table = {}
        self.state = None
        self.runtime = 0
        self.steps = 0
        self.pos = 0
        self.offset = 0

        if program is not None:
            self.load(program)

    def __str__(self):
        return f"Current: {self.state}; steps: {self.steps} of {self.runtime}"

    def __getitem__(self, i):
        i += self.offset
        if i < 0 or i >= len(self.tape):
            return 0
        else:
            return self.tape[i]

    def __setitem__(self, i, x):
        i += self.offset
        if i >= 0 and i < len(self.tape):
            self.tape[i] = x
        elif i == -1:
            self.tape.appendleft(x)
            self.offset += 1
        elif i == len(self.tape):
            self.tape.append(x)
        else:
            raise IndexError('Tried to set position off end of tape')

    def load(self, program):
        if isinstance(program, Iterator):
            program = ''.join(program)

        match = RE_ISTATE.search(program)
        self.state = match['state']

        match = RE_RUNTIME.search(program)
        self.runtime = int(match['steps'])

        for match in RE_STATETRANS.finditer(program):
            self.transition_table[match['state']] = {
                int(match['read0']): Rule(write=int(match['write0']),
                                          move=MOVE[match['move0']],
                                          next_state=match['next0']),
                int(match['read1']): Rule(write=int(match['write1']),
                                          move=MOVE[match['move1']],
                                          next_state=match['next1']),
            }

    def run(self):
        for _ in tqdm(range(self.runtime),
                      desc="Running", unit="steps", unit_scale=True):
            self.step()

    def step(self):
        read = self[self.pos]
        rule = self.transition_table[self.state][read]
        self[self.pos] = rule.write
        self.pos += rule.move
        self.state = rule.next_state

    @property
    def checksum(self):
        return sum(self.tape)


machine = TuringMachine(fi.input())
machine.run()

print("Checksum:", machine.checksum)
