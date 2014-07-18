from memsim import parser
from memsim.memory import base, join, transform


def random_xor(machine, nxt, rand, cost):
    value = 1 << rand.randint(0, machine.addr_bits - 1)
    result = XOR(join.Join(), nxt, value)
    result.reset(machine)
    return result if result.get_cost().fits(cost) else None


class XOR(transform.Transform):

    def __init__(self, bank, mem, value):
        transform.Transform.__init__(self, bank, mem)
        self.value = value

    def __str__(self):
        result = '(xor '
        result += '(value ' + str(self.value) + ')'
        result += '(bank ' + str(self.bank.get_name()) + ')'
        result += '(memory ' + str(self.mem.get_name()) + ')'
        result += ')'
        return result

    def generate(self, gen, source):
        value = self.value
        return self.generate_transform('eor', value, value, gen, source)

    def get_cost(self):
        temp = XOR(join.Join(), self.get_next(), 1024)
        temp.reset(self.machine)
        return transform.Transform.get_cost(temp)

    def is_empty(self):
        return self.value == 0

    def combine(self, other):
        assert(isinstance(other, XOR))
        self.value ^= other.value

    def permute(self, rand, max_cost):
        value = self.value
        self.value = 1 << rand.randint(0, self.machine.addr_bits - 1)
        if not self.get_cost().fits(max_cost):
            self.value = value
            return False
        return True

    def push_transform(self, index, rand):
        if index == 0:
            rand.push_transform(lambda a: a ^ self.value)
        else:
            rand.push_transform(lambda a: a)

    def pop_transform(self, rand):
        rand.pop_transform()

    def get_transform_path_length(self):
        return 1

    def process(self, baddr, start, write, addr, size):
        addr ^= self.value
        return base.send_request(self.bank, baddr, start, write, addr, size)

    def forward(self, baddr, index, start, write, addr, size):
        assert(index == 0)
        addr ^= self.value
        return base.send_request(self.mem, baddr, start, write, addr, size)


def _create_xor(lexer, args):
    value = parser.get_argument(lexer, args, 'value', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank = parser.get_argument(lexer, args, 'bank')
    return XOR(bank, mem, value)
base.constructors['xor'] = _create_xor
