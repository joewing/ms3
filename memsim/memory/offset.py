from memsim import parser
from memsim.memory import base, join, transform


def random_offset(machine, nxt, rand):
    word_size = nxt.get_word_size()
    if rand.randbool():
        offset = rand.randint(-word_size, word_size)
    else:
        offset = -rand.random_address(word_size)
    result = Offset(join.Join(), nxt, offset)
    result.reset(machine)
    return result


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

    def get_cost(self):
        temp = Offset(join.Join(), self.get_next(), self.get_word_size())
        temp.reset(self.machine)
        return transform.Transform.get_cost(temp)

    def permute(self, rand):
        word_size = self.get_word_size()
        direction = 1 if rand.randbool() else -1
        direction *= 1 if rand.randbool() else word_size
        self.offset += direction
        while rand.randint(0, 1) == 0:
            self.offset += direction
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


def _create_offset(lexer, args):
    offset = parser.get_argument(lexer, args, 'value', 0)
    mem = parser.get_argument(lexer, args, 'memory')
    bank = parser.get_argument(lexer, args, 'bank')
    return Offset(bank, mem, offset)
base.constructors['offset'] = _create_offset
