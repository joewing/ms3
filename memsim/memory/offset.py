
from memsim import parser
from memsim.memory import base
from memsim.memory import join
from memsim.memory import transform


def random_offset(machine, nxt, rand, cost):
    if rand.randbool():
        offset = rand.randint(-machine.word_size, machine.word_size)
    else:
        offset = rand.random_address(machine.word_size)
    return Offset(join.Join(), nxt, offset)


class Offset(transform.Transform):

    def __init__(self, bank, mem, offset):
        transform.Transform.__init__(self, bank, mem)
        self.offset = offset

    def __str__(self):
        result = '(offset '
        result += '(value ' + str(self.offset) + ')'
        result += '(bank ' + str(self.bank) + ')'
        result += '(memory ' + str(self.mem) + ')'
        result += ')'
        return result

    def generate(self, gen, mach):
        self.generate_transform("offset", self.offset, -self.offset, gen, mach)

    def is_empty(self):
        return self.offset == 0

    def combine(self, other):
        assert(isinstance(other, Offset))
        self.offset += other.offset

    def permute(self, rand, max_cost):
        word_size = self.machine.word_size
        if rand.randbool():
            self.offset = rand.randint(-word_size, word_size)
        else:
            self.offset = rand.random_address(word_size)
        return True

    def push_transform(self, index, rand):
        if index == 0:
            rand.push_transform(lambda a: a + self.offset)
        else:
            rand.push_transform(lambda a: a)

    def pop_transform(self, rand):
        rand.pop_transform()

    def get_transform_path_length(self):
        return self.machine.addr_bits

    def process(self, start, write, addr, size):
        addr = (addr + self.offset) & self.machine.addr_mask
        return self.bank.process(start, write, addr, size)

    def forward(self, index, start, write, addr, size):
        assert(index == 0)
        addr = (addr - self.offset) & self.machine.addr_mask
        return self.mem.process(start, write, addr, size)


def _create_offset(lexer, args):
    offset = parser.get_argument(lexer, args, 'value', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank = parser.get_argument(lexer, args, 'bank')
    return Offset(bank, mem, offset)
base.constructors['offset'] = _create_offset
