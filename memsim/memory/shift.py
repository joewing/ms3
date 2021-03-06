from memsim import parser, util
from memsim.memory import base
from memsim.memory import join
from memsim.memory import transform


def random_shift(machine, nxt, rand):
    word_size = nxt.get_word_size()
    word_bits = util.get_bus_shift(word_size)
    bits = machine.addr_bits - word_bits - 1
    shift = rand.randint(-bits, bits)
    result = Shift(join.Join(), nxt, shift)
    result.reset(machine)
    return result


class Shift(transform.Transform):

    def __init__(self, bank, mem, shift):
        transform.Transform.__init__(self, bank, mem)
        self.shift = shift

    def get_name(self, full):
        result = '(shift '
        result += '(value ' + str(self.shift) + ')'
        result += '(bank ' + str(self.bank.get_name(full)) + ')'
        result += '(memory ' + str(self.mem.get_name(full)) + ')'
        result += ')'
        return result

    def generate(self, gen, source):
        return self.generate_transform('shift', self.shift, -self.shift,
                                       gen, source)

    def get_cost(self):
        temp = Shift(join.Join(), self.get_next(), 5)
        temp.reset(self.machine)
        return transform.Transform.get_cost(temp)

    def reset(self, machine):
        transform.Transform.reset(self, machine)
        word_size = self.get_word_size()
        bits = machine.addr_bits - util.get_bus_shift(word_size)
        while self.shift < 0:
            self.shift += bits
        self.shift %= bits

    def permute(self, rand):
        word_bits = util.get_bus_shift(self.get_word_size())
        bits = self.machine.addr_bits - word_bits - 1
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
        word_size = self.get_word_size()
        word_bits = util.get_bus_shift(word_size)
        word_mask = word_size - 1
        bits = self.machine.addr_bits - word_bits
        mask = (1 << bits) - 1
        shift_part = value >> word_bits
        word_part = value & word_mask
        if count >= 0:
            count2 = bits - count
            shifted = ((shift_part << count) & mask) | (shift_part >> count2)
        else:
            count2 = bits + count
            shifted = ((shift_part << count2) & mask) | (shift_part >> -count)
        return (shifted << word_bits) | word_part


def _create_shift(lexer, args):
    value = parser.get_argument(lexer, args, 'value', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank = parser.get_argument(lexer, args, 'bank')
    return Shift(bank, mem, value)
base.constructors['shift'] = _create_shift
