from memsim import parser
from memsim.memory import base
from memsim.memory import join
from memsim.memory import transform


def random_offset(machine, nxt, rand, cost):
    word_size = nxt.get_word_size()
    if rand.randbool():
        offset = rand.randint(-word_size, word_size)
    else:
        offset = -rand.random_address(word_size)
    return Offset(join.Join(), nxt, offset)


class Offset(transform.Transform):

    def __init__(self, bank, mem, offset):
        transform.Transform.__init__(self, bank, mem)
        self.offset = offset

    def __str__(self):
        result = '(offset '
        result += '(value ' + str(self.offset) + ')'
        result += '(bank ' + str(self.bank.get_name()) + ')'
        result += '(memory ' + str(self.mem.get_name()) + ')'
        result += ')'
        return result

    def generate(self, gen, source):
        value = self.offset
        return self.generate_transform("offset", value, -value, gen, source)

    def is_empty(self):
        return self.offset == 0

    def combine(self, other):
        assert(isinstance(other, Offset))
        self.offset += other.offset

    def permute(self, rand, max_cost, max_size):
        word_size = self.get_word_size()
        if rand.randbool():
            self.offset = rand.randint(-word_size, word_size)
        else:
            self.offset = -rand.random_address(word_size)
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
        return base.send_request(self.bank, start, write, addr, size)

    def forward(self, index, start, write, addr, size):
        assert(index == 0)
        addr = (addr - self.offset) & self.machine.addr_mask
        return base.send_request(self.mem, start, write, addr, size)


def _create_offset(lexer, args):
    offset = parser.get_argument(lexer, args, 'value', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank = parser.get_argument(lexer, args, 'bank')
    return Offset(bank, mem, offset)
base.constructors['offset'] = _create_offset
