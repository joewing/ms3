
from memsim import parser
from memsim.memory import base
from memsim.memory import join
from memsim.memory import transform


def random_shift(machine, nxt, rand, cost):
    bits = machine.addr_bits - machine.word_bits - 1
    shift = rand.randint(-bits, bits)
    return Shift(join.Join(), nxt, shift)


class Shift(transform.Transform):

    def __init__(self, bank, mem, shift):
        transform.Transform.__init__(self, bank, mem)
        self.shift = shift

    def __str__(self):
        result = "(shift "
        result += "(value " + str(self.shift) + ")"
        result += '(bank ' + str(self.bank) + ')'
        result += '(memory ' + str(self.mem) + ')'
        result += ")"
        return result

    def generate(self, gen, mach):
        self.generate_transform("shift", self.shift, -self.shift, gen, mach)

    def reset(self, machine):
        transform.Transform.reset(self, machine)
        bits = machine.addr_bits - machine.word_bits
        while self.shift < 0:
            self.shift += bits
        self.shift %= bits

    def is_empty(self):
        return self.shift == 0

    def combine(self, other):
        self.shift += other.shift

    def permute(self, rand, max_cost):
        bits = self.machine.addr_bits - self.machine.word_bits - 1
        self.shift = rand.randint(-bits, bits)
        return True

    def push_transform(self, index, rand):
        if index == 0:
            rand.push_transform(lambda a: self._rotate(a, self.shift))
        else:
            rand.push_transform(lambda a: a)

    def pop_transform(self, rand):
        rand.pop_transform()

    def get_transform_path_length(self):
        return 0

    def _rotate(self, value, count):
        bits = self.machine.addr_bits - self.machine.word_bits
        mask = (1 << bits) - 1
        shift_part = value >> self.machine.word_bits
        word_part = value & self.machine.word_mask
        if count >= 0:
            count2 = bits - count
            shifted = ((shift_part << count) & mask) | (shift_part >> count2)
        else:
            count2 = bits + count
            shifted = ((shift_part << count2) & mask) | (shift_part >> -count)
        return (shifted << self.machine.word_bits) | word_part

    def process(self, start, write, addr, size):
        addr = self._rotate(addr, self.shift)
        return base.send_request(self.bank, start, write, addr, size)

    def forward(self, index, start, write, addr, size):
        addr = self._rotate(addr, -self.shift)
        return base.send_request(self.mem, start, write, addr, size)


def _create_shift(lexer, args):
    value = parser.get_argument(lexer, args, 'value', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank = parser.get_argument(lexer, args, 'bank')
    return Shift(bank, mem, value)
base.constructors['shift'] = _create_shift
