from memsim import parser
from memsim.memory import base


class FIFOContainer(base.Memory):
    """Container for FIFO memories."""

    def __init__(self, mem, item_size, size):
        base.Memory.__init__(self)
        self.item_size = item_size
        self.size = size
        self.mem = mem

    def __str__(self):
        result = '(fifo '
        result += '(size ' + str(self.size) + ')'
        result += '(item_size ' + str(self.item_size) + ')'
        result += '(memory ' + self.get_next().get_name() + ')'
        result += ')'
        return result

    def generate(self, gen, mach):
        return self.mem.generate(gen, mach)

    def get_next(self):
        return self.mem

    def set_next(self, n):
        self.mem = n

    def get_path_length(self):
        return self.get_next().get_path_length()

    def permute(self, rand, max_cost):
        temp = rand.randint(0, 1)
        if temp == 0 and self.size // 2 >= self.item_size:
            self.size //= 2
        else:
            self.size *= 2
        return True

    def process(self, start, write, addr, size):
        return self.mem.process(start, write, addr, size)

    def done(self):
        return self.mem.done()


def _create_fifo(lexer, args):
    item_size = parser.get_argument(lexer, args, 'item_size', 4)
    size = parser.get_argument(lexer, args, 'size', item_size)
    mem = parser.get_argument(lexer, args, 'memory')
    return FIFOContainer(mem=mem, size=size, item_size=item_size)
base.constructors['fifo'] = _create_fifo
