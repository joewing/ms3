
from memsim import parser
from memsim.memory import base
from memsim.memory import container


def random_prefetch(machine, nxt, rand, cost):
    stride = machine.word_size * rand.randint(-8, 8)
    result = Prefetch(nxt, stride)
    result.reset(machine)
    return result if result.get_cost() <= cost else None


class Prefetch(container.Container):

    def __init__(self, mem, stride):
        container.Container.__init__(self, mem)
        self.stride = stride
        self.time = 0

    def __str__(self):
        result = "(prefetch "
        result += "(stride " + str(self.stride) + ")"
        result += "(memory " + str(self.mem) + ")"
        result += ")"
        return result

    def permute(self, rand, max_cost):
        self.stride = self.machine.word_size * rand.randint(-8, 8)
        return True

    def simplify(self):
        self.mem = self.mem.simplify()
        if self.stride == 0:
            return self.mem
        else:
            return self

    def reset(self, m):
        container.Container.reset(self, m)
        self.time = 0

    def done(self):
        return max(self.time - self.machine.time, 0)

    def process(self, start, write, addr, size):
        result = max(start, self.time - self.machine.time)
        result = self.mem.process(result, write, addr, size)
        if not write:
            temp = (addr + self.stride) & self.machine.addr_mask
            t = self.mem.process(result, write, temp, 1)
            self.time = self.machine.time + t
        else:
            self.time = 0
        return result


def _create_prefetch(lexer, args):
    mem = parser.get_argument(lexer, args, 'memory')
    stride = parser.get_argument(lexer, args, 'stride', 0)
    return Prefetch(mem, stride)
base.constructors['prefetch'] = _create_prefetch
